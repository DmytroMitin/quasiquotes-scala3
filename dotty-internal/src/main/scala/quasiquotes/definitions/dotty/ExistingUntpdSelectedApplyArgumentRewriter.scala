package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Replaces one exact immediate argument in one existing selected-member Apply. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentRewriter:
  private val MinimumArguments = 2
  private val MaximumArguments = 3

  final case class Result(
      originalRoot: untpd.TypeDef,
      originalTemplate: untpd.Template,
      originalTarget: untpd.DefDef,
      originalApply: untpd.Apply,
      originalArgument: untpd.Tree,
      argumentIndex: Int,
      replacementLeaf: untpd.Tree,
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template,
      rebuiltTarget: untpd.DefDef,
      rebuiltApply: untpd.Apply,
      prefix: List[untpd.Tree],
      suffix: List[untpd.Tree]
  ):
    val preservedDirectChildren: List[untpd.Tree] = prefix ::: suffix
    val preservedArguments: List[untpd.Tree] =
      originalApply.args.zipWithIndex.collect {
        case (argument, index) if index != argumentIndex => argument
      }

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      replacementLeaf: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Result] =
    try
      for
        presentRoot <- Option(root).toRight(
          error("ROOT_REQUIRED", "the existing TypeDef root was null.")
        )
        template <- presentRoot.rhs match
          case value: untpd.Template => Right(value)
          case other =>
            Left(
              error(
                "ROOT_TEMPLATE_REQUIRED",
                s"the existing TypeDef rhs was ${nodeKind(other)}, not Template."
              )
            )
        templateBody <- Option(template.body).toRight(
          error("TEMPLATE_BODY_REQUIRED", "the existing Template body was null.")
        )
        presentTarget <- Option(exactTarget).toRight(
          error("TARGET_REQUIRED", "the exact target DefDef was null.")
        )
        targetIndex <- uniqueTargetIndex(templateBody, presentTarget)
        _ <- Either.cond(
          Option(presentTarget.paramss).exists(_.isEmpty),
          (),
          error(
            "TARGET_PARAMETER_CLAUSES_UNSUPPORTED",
            "the exact target must have a non-null empty parameter-clause list."
          )
        )
        targetRhs <- Option(presentTarget.rhs).filterNot(_.isEmpty).toRight(
          error("TARGET_BODY_REQUIRED", "the exact target has no existing body.")
        )
        originalApply <- targetRhs match
          case value: untpd.Apply => Right(value)
          case other =>
            Left(
              error(
                "RHS_SELECTED_APPLY_REQUIRED",
                s"the exact target rhs was ${nodeKind(other)}, not Apply."
              )
            )
        selection <- Option(originalApply.fun) match
          case Some(value: untpd.Select) => Right(value)
          case Some(other) =>
            Left(
              error(
                "APPLY_FUNCTION_SELECT_REQUIRED",
                s"the existing Apply function was ${nodeKind(other)}, not Select."
              )
            )
          case None =>
            Left(
              error("APPLY_FUNCTION_SELECT_REQUIRED", "the existing Apply function was null.")
            )
        _ <- Option(selection.qualifier) match
          case Some(_: untpd.Ident) => Right(())
          case Some(other) =>
            Left(
              error(
                "SELECT_QUALIFIER_IDENT_REQUIRED",
                s"the existing Select qualifier was ${nodeKind(other)}, not Ident."
              )
            )
          case None =>
            Left(
              error("SELECT_QUALIFIER_IDENT_REQUIRED", "the existing Select qualifier was null.")
            )
        _ <- Either.cond(
          Option(selection.name).exists(_.isTermName),
          (),
          error(
            "SELECT_NAME_TERM_REQUIRED",
            "the existing Select name must be a non-null term name."
          )
        )
        arguments <- Option(originalApply.args).toRight(
          error("APPLY_ARGUMENT_LIST_REQUIRED", "the existing Apply argument list was null.")
        )
        _ <- Either.cond(
          arguments.size >= MinimumArguments && arguments.size <= MaximumArguments,
          (),
          error(
            "APPLY_ARGUMENT_COUNT_REQUIRED",
            s"U014 admits $MinimumArguments..$MaximumArguments arguments; found ${arguments.size}."
          )
        )
        _ <- arguments.zipWithIndex.collectFirst { case (null, index) => index } match
          case Some(index) =>
            Left(
              error(
                "APPLY_ARGUMENT_ENTRY_REQUIRED",
                s"the existing Apply argument at index $index was null."
              )
            )
          case None => Right(())
        presentArgument <- Option(exactArgument).toRight(
          error("ARGUMENT_REQUIRED", "the exact existing argument was null.")
        )
        argumentIndex <- uniqueArgumentIndex(arguments, presentArgument)
        _ <- Either.cond(
          isAdmittedLeaf(presentArgument),
          (),
          error(
            "TARGET_ARGUMENT_LEAF_REQUIRED",
            s"the exact existing argument was ${nodeKind(presentArgument)}, not Ident, Number, or Literal."
          )
        )
        _ <- Either.cond(
          presentArgument.source.exists && presentArgument.span.exists,
          (),
          error(
            "TARGET_ARGUMENT_SITE_REQUIRED",
            "the exact existing argument must have a parsed source and span."
          )
        )
        _ <- arguments.zipWithIndex.collectFirst {
          case (argument, index) if !isAdmittedLeaf(argument) => (argument, index)
        } match
          case Some((argument, index)) =>
            Left(
              error(
                "APPLY_ARGUMENT_LEAF_REQUIRED",
                s"the existing Apply argument at index $index was ${nodeKind(argument)}, not Ident, Number, or Literal."
              )
            )
          case None => Right(())
        _ <- arguments.zipWithIndex.collectFirst {
          case (argument, index) if !argument.source.exists || !argument.span.exists => index
        } match
          case Some(index) =>
            Left(
              error(
                "APPLY_ARGUMENT_SITE_REQUIRED",
                s"the existing Apply argument at index $index must have a parsed source and span."
              )
            )
          case None => Right(())
        replacement <- Option(replacementLeaf).toRight(
          error("REPLACEMENT_REQUIRED", "the replacement leaf was null.")
        )
        _ <- validateReplacement(replacement)
        result <- rebuild(
          presentRoot,
          template,
          presentTarget,
          targetIndex,
          originalApply,
          presentArgument,
          argumentIndex,
          replacement
        )
      yield result
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "SELECTED_APPLY_ARGUMENT_REWRITE_FAILED",
            Option(exception.getMessage)
              .filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def uniqueTargetIndex(
      body: List[untpd.Tree],
      exactTarget: untpd.DefDef
  ): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Int] =
    val indices = body.iterator.zipWithIndex.collect {
      case (tree, index) if tree != null && tree.eq(exactTarget) => index
    }.toVector
    indices match
      case Vector(index) => Right(index)
      case Vector() =>
        Left(
          error(
            "TARGET_NOT_DIRECT_MEMBER",
            "the exact target object is not a direct member of the root Template body."
          )
        )
      case _ =>
        Left(
          error(
            "TARGET_IDENTITY_NOT_UNIQUE",
            s"the exact target object occurs ${indices.size} times in the direct Template body."
          )
        )

  private def uniqueArgumentIndex(
      arguments: List[untpd.Tree],
      exactArgument: untpd.Tree
  ): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Int] =
    val indices = arguments.iterator.zipWithIndex.collect {
      case (argument, index) if argument != null && argument.eq(exactArgument) => index
    }.toVector
    indices match
      case Vector(index) => Right(index)
      case Vector() =>
        Left(
          error(
            "TARGET_ARGUMENT_NOT_DIRECT_MEMBER",
            "the exact argument object is not an immediate member of the existing Apply arguments."
          )
        )
      case _ =>
        Left(
          error(
            "TARGET_ARGUMENT_IDENTITY_NOT_UNIQUE",
            s"the exact argument object occurs ${indices.size} times in the existing Apply arguments."
          )
        )

  private def validateReplacement(
      replacement: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Unit] =
    if replacement.isInstanceOf[untpd.TypedSplice] then
      Left(
        error(
          "REPLACEMENT_TYPED_SPLICE_UNSUPPORTED",
          "the replacement leaf was TypedSplice."
        )
      )
    else if !isAdmittedLeaf(replacement) then
      Left(
        error(
          "REPLACEMENT_LEAF_REQUIRED",
          s"the replacement was ${nodeKind(replacement)}, not Ident, Number, or Literal."
        )
      )
    else if replacement.source.exists then
      Left(
        error(
          "REPLACEMENT_SOURCE_PROVENANCE",
          s"the replacement contains source-bearing ${nodeKind(replacement)}."
        )
      )
    else if replacement.span.exists then
      Left(
        error(
          "REPLACEMENT_SPAN_PROVENANCE",
          s"the replacement contains spanned ${nodeKind(replacement)}."
        )
      )
    else if replacement.symbol != NoSymbol then
      Left(
        error(
          "REPLACEMENT_SYMBOL_PROVENANCE",
          s"the replacement contains symbol-bearing ${nodeKind(replacement)}."
        )
      )
    else Right(())

  private def rebuild(
      root: untpd.TypeDef,
      template: untpd.Template,
      target: untpd.DefDef,
      targetIndex: Int,
      originalApply: untpd.Apply,
      originalArgument: untpd.Tree,
      argumentIndex: Int,
      replacement: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Result] =
    given SourceFile = NoSource
    val rebuiltArguments = originalApply.args.zipWithIndex.map {
      case (_, index) if index == argumentIndex => replacement
      case (argument, _) => argument
    }
    val rebuiltApply = untpd.Apply(originalApply.fun, rebuiltArguments)
    val rebuiltTarget =
      untpd
        .DefDef(target.name, target.paramss, target.tpt, rebuiltApply)
        .withMods(target.mods)
    val (prefix, targetAndSuffix) = template.body.splitAt(targetIndex)
    val suffix = targetAndSuffix.tail
    val rebuiltTemplate = untpd.Template(
      template.constr,
      template.parentsOrDerived,
      template.derived,
      template.self,
      prefix ::: rebuiltTarget :: suffix
    )
    val rebuiltRoot = untpd.TypeDef(root.name, rebuiltTemplate).withMods(root.mods)
    val result = Result(
      root,
      template,
      target,
      originalApply,
      originalArgument,
      argumentIndex,
      replacement,
      rebuiltRoot,
      rebuiltTemplate,
      rebuiltTarget,
      rebuiltApply,
      prefix,
      suffix
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteError, Unit] =
    val reconstructed = Vector[untpd.Tree](
      result.rebuiltRoot,
      result.rebuiltTemplate,
      result.rebuiltTarget,
      result.rebuiltApply,
      result.replacementLeaf
    )
    val reconstructedValid = reconstructed.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val originalBody = result.originalTemplate.body
    val rebuiltBody = result.rebuiltTemplate.body
    val bodyIdentityValid =
      originalBody.size == rebuiltBody.size &&
        originalBody.indices.forall { index =>
          if originalBody(index).eq(result.originalTarget) then
            rebuiltBody(index).eq(result.rebuiltTarget)
          else rebuiltBody(index).eq(originalBody(index))
        }
    val argumentIdentityValid =
      result.originalApply.args.size == result.rebuiltApply.args.size &&
        result.originalApply.args.indices.forall { index =>
          if index == result.argumentIndex then
            result.rebuiltApply.args(index).eq(result.replacementLeaf) &&
              !result.rebuiltApply.args(index).eq(result.originalArgument)
          else result.rebuiltApply.args(index).eq(result.originalApply.args(index))
        }
    val identityValid =
      !result.rebuiltRoot.eq(result.originalRoot) &&
        !result.rebuiltTemplate.eq(result.originalTemplate) &&
        !result.rebuiltTarget.eq(result.originalTarget) &&
        !result.rebuiltApply.eq(result.originalApply) &&
        result.rebuiltRoot.mods.eq(result.originalRoot.mods) &&
        result.rebuiltTemplate.constr.eq(result.originalTemplate.constr) &&
        result.rebuiltTemplate.parentsOrDerived.eq(
          result.originalTemplate.parentsOrDerived
        ) &&
        result.rebuiltTemplate.derived.eq(result.originalTemplate.derived) &&
        result.rebuiltTemplate.self.eq(result.originalTemplate.self) &&
        result.rebuiltTarget.mods.eq(result.originalTarget.mods) &&
        result.rebuiltTarget.tpt.eq(result.originalTarget.tpt) &&
        result.rebuiltTarget.rhs.eq(result.rebuiltApply) &&
        result.rebuiltApply.fun.eq(result.originalApply.fun) &&
        bodyIdentityValid && argumentIdentityValid
    Either.cond(
      reconstructedValid && identityValid,
      (),
      error(
        "RECONSTRUCTION_INVARIANT_FAILED",
        "the mixed-provenance structural result violated its bounded identity/provenance contract."
      )
    )

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentRewriteError =
    ExistingUntpdSelectedApplyArgumentRewriteError(code, detail)
