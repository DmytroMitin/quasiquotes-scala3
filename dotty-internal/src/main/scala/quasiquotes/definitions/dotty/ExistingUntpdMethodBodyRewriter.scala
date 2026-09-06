package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Bounded identity-preserving rewrite of one existing direct method body. */
private[quasiquotes] object ExistingUntpdMethodBodyRewriter:
  enum ProvenanceKind:
    case Preserved, Reconstructed, Replacement

  final case class Result(
      originalRoot: untpd.TypeDef,
      originalTemplate: untpd.Template,
      originalTarget: untpd.DefDef,
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template,
      rebuiltTarget: untpd.DefDef,
      replacementBody: untpd.Tree,
      prefix: List[untpd.Tree],
      suffix: List[untpd.Tree]
  ):
    val preservedDirectChildren: List[untpd.Tree] = prefix ::: suffix
    val provenanceKinds: Vector[ProvenanceKind] =
      Vector(
        ProvenanceKind.Preserved,
        ProvenanceKind.Reconstructed,
        ProvenanceKind.Replacement
      )

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Result] =
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
        presentTarget <- Option(exactTarget).toRight(
          error("TARGET_REQUIRED", "the exact target DefDef was null.")
        )
        targetIndex <- uniqueTargetIndex(template.body, presentTarget)
        _ <- Either.cond(
          presentTarget.paramss.isEmpty,
          (),
          error(
            "TARGET_PARAMETER_CLAUSES_UNSUPPORTED",
            s"the exact target has ${presentTarget.paramss.size} parameter clause(s)."
          )
        )
        _ <- Either.cond(
          !presentTarget.rhs.isEmpty,
          (),
          error("TARGET_BODY_REQUIRED", "the exact target has no existing body.")
        )
        presentReplacement <- Option(replacementBody).toRight(
          error("REPLACEMENT_BODY_REQUIRED", "the replacement body was null.")
        )
        _ <- validateReplacement(presentReplacement)
        result <- rebuild(
          presentRoot,
          template,
          presentTarget,
          targetIndex,
          presentReplacement
        )
      yield result
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "EXISTING_TREE_REWRITE_FAILED",
            Option(exception.getMessage)
              .filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private[dotty] def rewriteSingleParameter(
      view: ExistingUntpdSingleParameterMethodView.View,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U028 single-parameter method view was null.")
      )
      _ <- ExistingUntpdSingleParameterMethodView
        .validate(presentView)
        .left
        .map(problem => error("VIEW_INVALID", problem.message))
      targetIndex <- uniqueTargetIndex(
        presentView.captured.originalTemplate.body,
        presentView.method
      )
      _ <- Either.cond(
        targetIndex == presentView.memberIndex,
        (),
        error(
          "VIEW_MEMBER_INDEX_MISMATCH",
          "the exact method identity no longer occurs at the U028 captured direct-member index."
        )
      )
      presentReplacement <- Option(replacementBody).toRight(
        error("REPLACEMENT_BODY_REQUIRED", "the replacement body was null.")
      )
      _ <- Either.cond(
        !presentReplacement.isEmpty,
        (),
        error("REPLACEMENT_BODY_REQUIRED", "the replacement body was EmptyTree.")
      )
      _ <- Either.cond(
        !rawTreeGraphHasNull(presentReplacement),
        (),
        error(
          "REPLACEMENT_GRAPH_MALFORMED",
          "the replacement graph contained a null tree, child, or child sequence."
        )
      )
      _ <- validateReplacement(presentReplacement)
      result <- rebuild(
        presentView.captured.originalRoot,
        presentView.captured.originalTemplate,
        presentView.method,
        targetIndex,
        presentReplacement
      )
      _ <- validateSingleParameterResult(presentView, result)
    yield result

  private[dotty] def rewriteTwoParameter(
      view: ExistingUntpdTwoParameterMethodView.View,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Result] =
    for
      presentView <- Option(view).toRight(
        error("VIEW_REQUIRED", "the U033 two-parameter method view was null.")
      )
      _ <- ExistingUntpdTwoParameterMethodView
        .validate(presentView)
        .left
        .map(problem => error("VIEW_INVALID", problem.message))
      targetIndex <- uniqueTargetIndex(
        presentView.captured.originalTemplate.body,
        presentView.method
      )
      _ <- Either.cond(
        targetIndex == presentView.memberIndex,
        (),
        error(
          "VIEW_MEMBER_INDEX_MISMATCH",
          "the exact method identity no longer occurs at the U033 captured direct-member index."
        )
      )
      presentReplacement <- Option(replacementBody).toRight(
        error("REPLACEMENT_BODY_REQUIRED", "the replacement body was null.")
      )
      _ <- Either.cond(
        !presentReplacement.isEmpty,
        (),
        error("REPLACEMENT_BODY_REQUIRED", "the replacement body was EmptyTree.")
      )
      _ <- Either.cond(
        !rawTreeGraphHasNull(presentReplacement),
        (),
        error(
          "REPLACEMENT_GRAPH_MALFORMED",
          "the replacement graph contained a null tree, child, or child sequence."
        )
      )
      _ <- validateReplacement(presentReplacement)
      result <- rebuild(
        presentView.captured.originalRoot,
        presentView.captured.originalTemplate,
        presentView.method,
        targetIndex,
        presentReplacement
      )
      _ <- validateTwoParameterResult(presentView, result)
    yield result

  private[dotty] def rawTreeGraphHasNull(tree: untpd.Tree): Boolean =
    def containsNull(value: Any): Boolean =
      value match
        case null => true
        case current: untpd.Tree =>
          current.productIterator.exists(containsNull)
        case values: Iterable[?] => values.iterator.exists(containsNull)
        case _ => false

    containsNull(tree)

  private def uniqueTargetIndex(
      body: List[untpd.Tree],
      exactTarget: untpd.DefDef
  ): Either[ExistingUntpdMethodBodyRewriteError, Int] =
    val indices = body.iterator.zipWithIndex.collect {
      case (tree, index) if tree.eq(exactTarget) => index
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

  private def validateReplacement(
      replacement: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Unit] =
    val trees = allTrees(replacement)
    trees.find(_.isInstanceOf[untpd.TypedSplice]) match
      case Some(tree) =>
        Left(
          error(
            "REPLACEMENT_TYPED_SPLICE_UNSUPPORTED",
            s"the replacement contains ${nodeKind(tree)}."
          )
        )
      case None =>
        trees.find(_.source.exists) match
          case Some(tree) =>
            Left(
              error(
                "REPLACEMENT_SOURCE_PROVENANCE",
                s"the replacement contains source-bearing ${nodeKind(tree)}."
              )
            )
          case None =>
            trees.find(_.span.exists) match
              case Some(tree) =>
                Left(
                  error(
                    "REPLACEMENT_SPAN_PROVENANCE",
                    s"the replacement contains spanned ${nodeKind(tree)}."
                  )
                )
              case None =>
                trees.find(_.symbol != NoSymbol) match
                  case Some(tree) =>
                    Left(
                      error(
                        "REPLACEMENT_SYMBOL_PROVENANCE",
                        s"the replacement contains symbol-bearing ${nodeKind(tree)}."
                      )
                    )
                  case None => Right(())

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def rebuild(
      root: untpd.TypeDef,
      template: untpd.Template,
      target: untpd.DefDef,
      targetIndex: Int,
      replacementBody: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Result] =
    given SourceFile = NoSource
    val (prefix, targetAndSuffix) = template.body.splitAt(targetIndex)
    val suffix = targetAndSuffix.tail
    val rebuiltTarget =
      untpd
        .DefDef(target.name, target.paramss, target.tpt, replacementBody)
        .withMods(target.mods)
    val rebuiltTemplate = untpd.Template(
      template.constr,
      template.parentsOrDerived,
      template.derived,
      template.self,
      prefix ::: rebuiltTarget :: suffix
    )
    val rebuiltRoot =
      untpd.TypeDef(root.name, rebuiltTemplate).withMods(root.mods)
    val result = Result(
      root,
      template,
      target,
      rebuiltRoot,
      rebuiltTemplate,
      rebuiltTarget,
      replacementBody,
      prefix,
      suffix
    )
    verifyResult(result).map(_ => result)

  private def verifyResult(
      result: Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Unit] =
    val reconstructed =
      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget
      )
    val reconstructedValid = reconstructed.forall(tree =>
      !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
        !tree.isInstanceOf[untpd.TypedSplice]
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
    val identityValid =
      !result.rebuiltRoot.eq(result.originalRoot) &&
        !result.rebuiltTemplate.eq(result.originalTemplate) &&
        !result.rebuiltTarget.eq(result.originalTarget) &&
        result.rebuiltRoot.mods.eq(result.originalRoot.mods) &&
        result.rebuiltTemplate.constr.eq(result.originalTemplate.constr) &&
        result.rebuiltTemplate.parentsOrDerived.eq(
          result.originalTemplate.parentsOrDerived
        ) &&
        result.rebuiltTemplate.derived.eq(result.originalTemplate.derived) &&
        result.rebuiltTemplate.self.eq(result.originalTemplate.self) &&
        result.rebuiltTarget.mods.eq(result.originalTarget.mods) &&
        result.rebuiltTarget.tpt.eq(result.originalTarget.tpt) &&
        result.rebuiltTarget.rhs.eq(result.replacementBody) &&
        bodyIdentityValid
    Either.cond(
      reconstructedValid && identityValid,
      (),
      error(
        "RECONSTRUCTED_PROVENANCE_INVARIANT_FAILED",
        "the rebuilt root/template/target or preserved direct children violated the bounded identity/provenance contract."
      )
    )

  private def validateSingleParameterResult(
      view: ExistingUntpdSingleParameterMethodView.View,
      result: Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Unit] =
    val rebuiltParameter =
      Option(result.rebuiltTarget.paramss)
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .filter(_ != null)
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .collect { case parameter: untpd.ValDef => parameter }
    val valid =
      result.originalRoot.eq(view.captured.originalRoot) &&
        result.originalTemplate.eq(view.captured.originalTemplate) &&
        result.originalTarget.eq(view.method) &&
        rebuiltParameter.exists(parameter =>
          parameter.eq(view.parameter) && parameter.tpt.eq(view.parameterType)
        ) &&
        result.rebuiltTarget.tpt.eq(view.resultType) &&
        result.rebuiltTarget.rhs.eq(result.replacementBody)
    Either.cond(
      valid,
      (),
      error(
        "SINGLE_PARAMETER_REWRITE_INVARIANT_FAILED",
        "the reconstructed method did not preserve the exact U028 parameter, parameter type, result type, or captured owner identity."
      )
    )

  private def validateTwoParameterResult(
      view: ExistingUntpdTwoParameterMethodView.View,
      result: Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteError, Unit] =
    val rebuiltParameters =
      Option(result.rebuiltTarget.paramss)
        .filter(_.size == 1)
        .flatMap(_.headOption)
        .filter(_ != null)
        .filter(_.size == 2)
        .flatMap { clause =>
          (clause(0), clause(1)) match
            case (first: untpd.ValDef, second: untpd.ValDef) => Some((first, second))
            case _ => None
        }
    val valid =
      result.originalRoot.eq(view.captured.originalRoot) &&
        result.originalTemplate.eq(view.captured.originalTemplate) &&
        result.originalTarget.eq(view.method) &&
        rebuiltParameters.exists { case (first, second) =>
          first.eq(view.firstParameter) &&
          first.tpt.eq(view.firstParameterType) &&
          second.eq(view.secondParameter) &&
          second.tpt.eq(view.secondParameterType)
        } &&
        result.rebuiltTarget.tpt.eq(view.resultType) &&
        result.rebuiltTarget.rhs.eq(result.replacementBody)
    Either.cond(
      valid,
      (),
      error(
        "TWO_PARAMETER_REWRITE_INVARIANT_FAILED",
        "the reconstructed method did not preserve both exact U033 parameters, parameter types, result type, or captured owner identity."
      )
    )

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdMethodBodyRewriteError =
    ExistingUntpdMethodBodyRewriteError(code, detail)
