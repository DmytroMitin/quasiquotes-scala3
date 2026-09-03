package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Replaces one exact U014 argument with one bounded selected-member Apply subtree. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter:
  private val MinimumReplacementArguments = 1
  private val MaximumReplacementArguments = 3

  final case class Result(
      validatedExisting: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      replacementApply: untpd.Apply,
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template,
      rebuiltTarget: untpd.DefDef,
      rebuiltApply: untpd.Apply
  ):
    def originalRoot: untpd.TypeDef = validatedExisting.originalRoot
    def originalTemplate: untpd.Template = validatedExisting.originalTemplate
    def originalTarget: untpd.DefDef = validatedExisting.originalTarget
    def originalApply: untpd.Apply = validatedExisting.originalApply
    def originalArgument: untpd.Tree = validatedExisting.originalArgument
    def argumentIndex: Int = validatedExisting.argumentIndex
    def prefix: List[untpd.Tree] = validatedExisting.prefix
    def suffix: List[untpd.Tree] = validatedExisting.suffix
    def preservedArguments: List[untpd.Tree] = validatedExisting.preservedArguments

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      replacement: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, Result] =
    try
      for
        replacementApply <- validateReplacement(replacement)
        existing <- validateExisting(root, exactTarget, exactArgument)
        result <- rebuild(existing, replacementApply)
      yield result
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "SELECTED_APPLY_ARGUMENT_SELECTED_APPLY_REWRITE_FAILED",
            Option(exception.getMessage).filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def validateExisting(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError,
    ExistingUntpdSelectedApplyArgumentRewriter.Result] =
    given SourceFile = NoSource
    val validationLeaf = untpd.Number("0", untpd.NumberKind.Whole(10))
    ExistingUntpdSelectedApplyArgumentRewriter
      .rewrite(root, exactTarget, exactArgument, validationLeaf)
      .left.map(problem => error(problem.code, problem.detail))

  private def validateReplacement(
      replacement: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, untpd.Apply] =
    Option(replacement).toRight(
      error("REPLACEMENT_REQUIRED", "the selected-member child-bearing replacement was null.")
    ).flatMap {
      case apply: untpd.Apply =>
        for
          function <- Option(apply.fun).toRight(
            error("REPLACEMENT_FUNCTION_SELECT_REQUIRED", "the replacement function was null.")
          )
          selection <- function match
            case value: untpd.Select => Right(value)
            case other =>
              Left(error(
                "REPLACEMENT_FUNCTION_SELECT_REQUIRED",
                s"the replacement function was ${nodeKind(other)}, not Select."
              ))
          qualifier <- Option(selection.qualifier).toRight(
            error("REPLACEMENT_QUALIFIER_IDENT_REQUIRED", "the replacement qualifier was null.")
          )
          _ <- Either.cond(
            qualifier.isInstanceOf[untpd.Ident],
            (),
            error(
              "REPLACEMENT_QUALIFIER_IDENT_REQUIRED",
              s"the replacement qualifier was ${nodeKind(qualifier)}, not direct Ident."
            )
          )
          _ <- Either.cond(
            Option(selection.name).exists(_.isTermName),
            (),
            error(
              "REPLACEMENT_MEMBER_TERM_NAME_REQUIRED",
              "the replacement selected member was not a term name."
            )
          )
          arguments <- Option(apply.args).toRight(
            error("REPLACEMENT_ARGUMENT_LIST_REQUIRED", "the replacement argument list was null.")
          )
          _ <- Either.cond(
            arguments.size >= MinimumReplacementArguments &&
              arguments.size <= MaximumReplacementArguments,
            (),
            error(
              "REPLACEMENT_ARGUMENT_COUNT_REQUIRED",
              s"U016 admits $MinimumReplacementArguments..$MaximumReplacementArguments replacement arguments; found ${arguments.size}."
            )
          )
          _ <- arguments.zipWithIndex.collectFirst { case (null, index) => index } match
            case Some(index) =>
              Left(error("REPLACEMENT_ARGUMENT_REQUIRED", s"replacement argument $index was null."))
            case None => Right(())
          _ <- arguments.zipWithIndex.collectFirst {
            case (argument, index) if !isAdmittedLeaf(argument) => argument -> index
          } match
            case Some((argument, index)) =>
              Left(
                error(
                  "REPLACEMENT_ARGUMENT_LEAF_REQUIRED",
                  s"replacement argument $index was ${nodeKind(argument)}, not Ident, Number, or Literal."
                )
              )
            case None => Right(())
          _ <- validateSourceFree(apply, selection, qualifier, arguments)
        yield apply
      case other =>
        Left(
          error(
            "REPLACEMENT_APPLY_REQUIRED",
            s"the selected-member child-bearing replacement was ${nodeKind(other)}, not Apply."
          )
        )
    }

  private def validateSourceFree(
      apply: untpd.Apply,
      selection: untpd.Select,
      qualifier: untpd.Tree,
      arguments: List[untpd.Tree]
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, Unit] =
    val nodes = apply +: selection +: qualifier +: arguments.toVector
    nodes.collectFirst { case tree if tree.isInstanceOf[untpd.TypedSplice] => tree } match
      case Some(tree) =>
        Left(error("REPLACEMENT_TYPED_SPLICE_UNSUPPORTED", s"replacement contains ${nodeKind(tree)}."))
      case None =>
        nodes.collectFirst { case tree if tree.source.exists => tree } match
          case Some(tree) =>
            Left(error("REPLACEMENT_SOURCE_PROVENANCE", s"replacement contains source-bearing ${nodeKind(tree)}."))
          case None =>
            nodes.collectFirst { case tree if tree.span.exists => tree } match
              case Some(tree) =>
                Left(error("REPLACEMENT_SPAN_PROVENANCE", s"replacement contains spanned ${nodeKind(tree)}."))
              case None =>
                nodes.collectFirst { case tree if tree.symbol != NoSymbol => tree } match
                  case Some(tree) =>
                    Left(error("REPLACEMENT_SYMBOL_PROVENANCE", s"replacement contains symbol-bearing ${nodeKind(tree)}."))
                  case None => Right(())

  private def rebuild(
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      replacement: untpd.Apply
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, Result] =
    given SourceFile = NoSource
    val arguments = existing.originalApply.args.zipWithIndex.map {
      case (_, index) if index == existing.argumentIndex => replacement
      case (argument, _) => argument
    }
    val rebuiltApply = untpd.Apply(existing.originalApply.fun, arguments)
    val rebuiltTarget = untpd
      .DefDef(existing.originalTarget.name, existing.originalTarget.paramss,
        existing.originalTarget.tpt, rebuiltApply)
      .withMods(existing.originalTarget.mods)
    val rebuiltTemplate = untpd.Template(
      existing.originalTemplate.constr,
      existing.originalTemplate.parentsOrDerived,
      existing.originalTemplate.derived,
      existing.originalTemplate.self,
      existing.prefix ::: rebuiltTarget :: existing.suffix
    )
    val rebuiltRoot = untpd.TypeDef(existing.originalRoot.name, rebuiltTemplate)
      .withMods(existing.originalRoot.mods)
    val result = Result(existing, replacement, rebuiltRoot, rebuiltTemplate, rebuiltTarget, rebuiltApply)
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, Unit] =
    val selection = result.replacementApply.fun.asInstanceOf[untpd.Select]
    val replacementNodes = result.replacementApply +:
      selection +: selection.qualifier +: result.replacementApply.args.toVector
    val freshNodes = Vector[untpd.Tree](
      result.rebuiltRoot, result.rebuiltTemplate, result.rebuiltTarget, result.rebuiltApply
    ) ++ replacementNodes
    val provenanceValid = freshNodes.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val argumentsValid =
      result.rebuiltApply.args.size == result.originalApply.args.size &&
        result.originalApply.args.indices.forall { index =>
          if index == result.argumentIndex then
            result.rebuiltApply.args(index).eq(result.replacementApply) &&
              !result.rebuiltApply.args(index).eq(result.originalArgument)
          else result.rebuiltApply.args(index).eq(result.originalApply.args(index))
        }
    val bodyValid =
      result.rebuiltTemplate.body.size == result.originalTemplate.body.size &&
        result.rebuiltTemplate.body.indices.forall { index =>
          val original = result.originalTemplate.body(index)
          if original.eq(result.originalTarget) then
            result.rebuiltTemplate.body(index).eq(result.rebuiltTarget)
          else result.rebuiltTemplate.body(index).eq(original)
        }
    val identityValid =
      !result.rebuiltRoot.eq(result.originalRoot) &&
        !result.rebuiltTemplate.eq(result.originalTemplate) &&
        !result.rebuiltTarget.eq(result.originalTarget) &&
        !result.rebuiltApply.eq(result.originalApply) &&
        !result.replacementApply.eq(result.originalArgument) &&
        result.rebuiltRoot.name == result.originalRoot.name &&
        result.rebuiltRoot.mods.eq(result.originalRoot.mods) &&
        result.rebuiltTemplate.constr.eq(result.originalTemplate.constr) &&
        result.rebuiltTemplate.parentsOrDerived.eq(result.originalTemplate.parentsOrDerived) &&
        result.rebuiltTemplate.derived.eq(result.originalTemplate.derived) &&
        result.rebuiltTemplate.self.eq(result.originalTemplate.self) &&
        result.rebuiltTarget.name == result.originalTarget.name &&
        result.rebuiltTarget.mods.eq(result.originalTarget.mods) &&
        result.rebuiltTarget.paramss.eq(result.originalTarget.paramss) &&
        result.rebuiltTarget.tpt.eq(result.originalTarget.tpt) &&
        result.rebuiltTarget.rhs.eq(result.rebuiltApply) &&
        result.rebuiltApply.fun.eq(result.originalApply.fun) &&
        argumentsValid && bodyValid
    Either.cond(
      provenanceValid && identityValid,
      (),
      error("RECONSTRUCTION_INVARIANT_FAILED",
        "the U016 mixed-provenance result violated its bounded identity/provenance contract.")
    )

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] || tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError =
    ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError(code, detail)
