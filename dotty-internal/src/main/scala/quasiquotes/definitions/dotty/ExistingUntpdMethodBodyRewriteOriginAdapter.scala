package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Attributes a bounded U002 rewrite to the original sites it transforms. */
private[quasiquotes] object ExistingUntpdMethodBodyRewriteOriginAdapter:
  enum OriginKind:
    case PreservedOriginalObject
    case ReconstructedAtOriginalSite
    case ReplacementAtTransformationSite

  final case class Result(
      structuralResult: ExistingUntpdMethodBodyRewriter.Result,
      positionedRoot: untpd.TypeDef,
      positionedTemplate: untpd.Template,
      positionedTarget: untpd.DefDef,
      positionedReplacement: untpd.Tree
  ):
    val originKinds: Vector[OriginKind] = Vector(
      OriginKind.PreservedOriginalObject,
      OriginKind.ReconstructedAtOriginalSite,
      OriginKind.ReplacementAtTransformationSite
    )

  def adapt(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    try
      Option(structural)
        .toRight(error("RESULT_REQUIRED", "the U002 structural result was null."))
        .flatMap(validateInput)
        .flatMap(position)
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "ORIGIN_ADAPTATION_FAILED",
            Option(exception.getMessage)
              .filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def validateInput(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, ExistingUntpdMethodBodyRewriter.Result] =
    val sourceFreeNodes = Vector[untpd.Tree](
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget
    ) ++ allTrees(structural.replacementBody)
    if sourceFreeNodes.exists(tree =>
        tree.source.exists || tree.span.exists || tree.symbol != NoSymbol ||
          tree.isInstanceOf[untpd.TypedSplice]
      )
    then
      Left(
        error(
          "SOURCE_FREE_INTERMEDIATE_REQUIRED",
          "the U002 rebuilt containers and complete replacement must remain source/span/symbol-free and contain no TypedSplice."
        )
      )
    else if allTrees(structural.replacementBody).size != 1 then
      Left(
        error(
          "REPLACEMENT_CHILDREN_UNSUPPORTED",
          "U003 admits only a single-node replacement; child-bearing replacements require a wider positioning policy."
        )
      )
    else
      val originalSites = Vector[untpd.Tree](
        structural.originalRoot,
        structural.originalTemplate,
        structural.originalTarget,
        structural.originalTarget.rhs
      )
      Either.cond(
        originalSites.forall(tree => tree.source.exists && tree.span.exists),
        structural,
        error(
          "ORIGINAL_SITE_REQUIRED",
          "the original root, template, target, and replaced RHS must each have a source and span."
        )
      )

  private def position(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    val positionedReplacement = structural.replacementBody
      .cloneIn(structural.originalTarget.rhs.source)
      .withSpan(structural.originalTarget.rhs.span)
    val positionedTarget =
      untpd
        .cpy
        .DefDef(structural.rebuiltTarget)(
          structural.rebuiltTarget.name,
          structural.rebuiltTarget.paramss,
          structural.rebuiltTarget.tpt,
          positionedReplacement
        )
        .cloneIn(structural.originalTarget.source)
        .withSpan(structural.originalTarget.span)
    val positionedTemplate =
      untpd
        .cpy
        .Template(structural.rebuiltTemplate)(
          structural.rebuiltTemplate.constr,
          structural.rebuiltTemplate.parentsOrDerived,
          structural.rebuiltTemplate.derived,
          structural.rebuiltTemplate.self,
          structural.prefix ::: positionedTarget :: structural.suffix
        )
        .cloneIn(structural.originalTemplate.source)
        .withSpan(structural.originalTemplate.span)
    val positionedRoot =
      untpd
        .cpy
        .TypeDef(structural.rebuiltRoot)(
          structural.rebuiltRoot.name,
          positionedTemplate
        )
        .cloneIn(structural.originalRoot.source)
        .withSpan(structural.originalRoot.span)
    val result = Result(
      structural,
      positionedRoot,
      positionedTemplate,
      positionedTarget,
      positionedReplacement
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Unit] =
    val structural = result.structuralResult
    val originalUntouched =
      structural.originalTemplate.body.filterNot(_.eq(structural.originalTarget))
    val positionedUntouched =
      result.positionedTemplate.body.filterNot(_.eq(result.positionedTarget))
    val valid =
      !result.positionedRoot.eq(structural.originalRoot) &&
        !result.positionedRoot.eq(structural.rebuiltRoot) &&
        !result.positionedTemplate.eq(structural.originalTemplate) &&
        !result.positionedTemplate.eq(structural.rebuiltTemplate) &&
        !result.positionedTarget.eq(structural.originalTarget) &&
        !result.positionedTarget.eq(structural.rebuiltTarget) &&
        !result.positionedReplacement.eq(structural.replacementBody) &&
        result.positionedRoot.source == structural.originalRoot.source &&
        result.positionedRoot.span == structural.originalRoot.span &&
        result.positionedTemplate.source == structural.originalTemplate.source &&
        result.positionedTemplate.span == structural.originalTemplate.span &&
        result.positionedTarget.source == structural.originalTarget.source &&
        result.positionedTarget.span == structural.originalTarget.span &&
        result.positionedReplacement.source == structural.originalTarget.rhs.source &&
        result.positionedReplacement.span == structural.originalTarget.rhs.span &&
        result.positionedRoot.mods.eq(structural.originalRoot.mods) &&
        result.positionedTemplate.constr.eq(structural.originalTemplate.constr) &&
        result.positionedTemplate.parentsOrDerived.eq(
          structural.originalTemplate.parentsOrDerived
        ) &&
        result.positionedTemplate.derived.eq(structural.originalTemplate.derived) &&
        result.positionedTemplate.self.eq(structural.originalTemplate.self) &&
        result.positionedTarget.mods.eq(structural.originalTarget.mods) &&
        result.positionedTarget.tpt.eq(structural.originalTarget.tpt) &&
        result.positionedTarget.rhs.eq(result.positionedReplacement) &&
        originalUntouched.size == positionedUntouched.size &&
        originalUntouched.zip(positionedUntouched).forall((left, right) =>
          left.eq(right)
        ) &&
        allTrees(result.positionedRoot).forall(tree =>
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
    Either.cond(
      valid,
      (),
      error(
        "ORIGIN_ADAPTATION_INVARIANT_FAILED",
        "the adapted tree violated the bounded identity or original-site attribution contract."
      )
    )

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdMethodBodyRewriteOriginError =
    ExistingUntpdMethodBodyRewriteOriginError(code, detail)
