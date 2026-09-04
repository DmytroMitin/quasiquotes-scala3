package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Applies U019 S4 origins while preserving the original first wrapper child. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter:
  final case class Result(
      structuralResult: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result,
      positionedRoot: untpd.TypeDef,
      positionedTemplate: untpd.Template,
      positionedTarget: untpd.DefDef,
      positionedApply: untpd.Apply,
      positionedWrapperApply: untpd.Apply,
      positionedWrapperFunction: untpd.Ident,
      positionedFreshSibling: untpd.Tree
  )

  def adapt(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Result] =
    try
      for
        present <- Option(structural).toRight(
          error("RESULT_REQUIRED", "the U019 structural result was null.")
        )
        _ <- validateStructuralPresence(present)
        _ <- validateStructuralGraph(present)
        _ <- validateExistingAndSites(present)
        result <- position(present)
      yield result
    catch
      case NonFatal(exception) =>
        Left(error(
          "ORIGIN_ADAPTATION_FAILED",
          Option(exception.getMessage).filter(_.nonEmpty)
            .getOrElse(exception.getClass.getSimpleName)
        ))

  private def validateStructuralPresence(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Unit] =
    val existing = structural.validatedExisting
    val valid =
      existing != null && existing.originalRoot != null &&
        existing.originalTemplate != null && existing.originalTarget != null &&
        existing.originalApply != null && existing.originalArgument != null &&
        existing.originalTemplate.body != null && existing.originalApply.args != null &&
        structural.wrapperApply != null && structural.wrapperApply.fun != null &&
        structural.wrapperApply.args != null && structural.rebuiltRoot != null &&
        structural.rebuiltTemplate != null && structural.rebuiltTarget != null &&
        structural.rebuiltApply != null && structural.rebuiltTemplate.body != null &&
        structural.rebuiltApply.args != null
    Either.cond(
      valid,
      (),
      error("STRUCTURAL_IDENTITY_REQUIRED",
        "the U019 structural carrier or one of its required graph nodes was null.")
    )

  private def validateExistingAndSites(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Unit] =
    ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter
      .adapt(structural.validatedExisting)
      .left.map(problem => error(problem.code, problem.detail))
      .map(_ => ())

  private def validateStructuralGraph(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Unit] =
    val existing = structural.validatedExisting
    val wrapperFunction = structural.wrapperApply.fun
    val wrapperArguments = structural.wrapperApply.args
    val wrapperShapePresent = wrapperArguments != null && wrapperArguments.size == 2
    val sibling = if wrapperShapePresent then wrapperArguments(1) else null
    val freshNodes = Vector[untpd.Tree](
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget,
      structural.rebuiltApply,
      structural.wrapperApply,
      wrapperFunction,
      sibling
    )
    val freshProvenanceValid = freshNodes.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val wrapperValid =
      wrapperShapePresent && wrapperFunction.isInstanceOf[untpd.Ident] &&
        wrapperArguments(0).eq(structural.originalArgument) &&
        wrapperArguments(1).eq(sibling) &&
        isAdmittedLeaf(sibling) && !sibling.eq(structural.originalArgument) &&
        !structural.originalApply.args.exists(_.eq(sibling))
    val argumentsValid =
      structural.rebuiltApply.args.size == structural.originalApply.args.size &&
        structural.originalApply.args.indices.forall { index =>
          if index == structural.argumentIndex then
            structural.rebuiltApply.args(index).eq(structural.wrapperApply)
          else structural.rebuiltApply.args(index).eq(structural.originalApply.args(index))
        }
    val bodyValid =
      structural.rebuiltTemplate.body.size == structural.originalTemplate.body.size &&
        structural.rebuiltTemplate.body.indices.forall { index =>
          val original = structural.originalTemplate.body(index)
          if original.eq(structural.originalTarget) then
            structural.rebuiltTemplate.body(index).eq(structural.rebuiltTarget)
          else structural.rebuiltTemplate.body(index).eq(original)
        }
    val graphValid =
      existing != null &&
        structural.originalRoot.eq(existing.originalRoot) &&
        structural.originalTemplate.eq(existing.originalTemplate) &&
        structural.originalTarget.eq(existing.originalTarget) &&
        structural.originalApply.eq(existing.originalApply) &&
        structural.originalArgument.eq(existing.originalArgument) &&
        structural.argumentIndex == existing.argumentIndex &&
        structural.rebuiltRoot.name == structural.originalRoot.name &&
        structural.rebuiltRoot.mods.eq(structural.originalRoot.mods) &&
        structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
        structural.rebuiltTemplate.constr.eq(structural.originalTemplate.constr) &&
        structural.rebuiltTemplate.parentsOrDerived.eq(
          structural.originalTemplate.parentsOrDerived
        ) &&
        structural.rebuiltTemplate.derived.eq(structural.originalTemplate.derived) &&
        structural.rebuiltTemplate.self.eq(structural.originalTemplate.self) &&
        structural.rebuiltTarget.name == structural.originalTarget.name &&
        structural.rebuiltTarget.mods.eq(structural.originalTarget.mods) &&
        structural.rebuiltTarget.paramss.eq(structural.originalTarget.paramss) &&
        structural.rebuiltTarget.tpt.eq(structural.originalTarget.tpt) &&
        structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
        structural.rebuiltApply.fun.eq(structural.originalApply.fun) &&
        !structural.rebuiltRoot.eq(structural.originalRoot) &&
        !structural.rebuiltTemplate.eq(structural.originalTemplate) &&
        !structural.rebuiltTarget.eq(structural.originalTarget) &&
        !structural.rebuiltApply.eq(structural.originalApply) &&
        wrapperValid && argumentsValid && bodyValid
    Either.cond(
      freshProvenanceValid && graphValid,
      (),
      error("STRUCTURAL_IDENTITY_REQUIRED",
        "the U019 structural result violated its exact wrapper-order and preserved-identity contract.")
    )

  private def position(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Result] =
    val site = structural.originalArgument
    val source = site.source
    val span = site.span
    val positionedFunction = structural.wrapperFunction.cloneIn(source)
      .withSpan(span).asInstanceOf[untpd.Ident]
    val positionedSibling = structural.freshSibling.cloneIn(source).withSpan(span)
    val positionedWrapper = untpd.Apply(
      positionedFunction,
      site :: positionedSibling :: Nil
    ).cloneIn(source).withSpan(span)
    val outerArguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => positionedWrapper
      case (argument, _) => argument
    }
    val positionedApply = untpd.Apply(structural.originalApply.fun, outerArguments)
      .cloneIn(structural.originalApply.source).withSpan(structural.originalApply.span)
    val positionedTarget = untpd.cpy.DefDef(structural.rebuiltTarget)(
      structural.rebuiltTarget.name,
      structural.rebuiltTarget.paramss,
      structural.rebuiltTarget.tpt,
      positionedApply
    ).cloneIn(structural.originalTarget.source).withSpan(structural.originalTarget.span)
    val positionedTemplate = untpd.cpy.Template(structural.rebuiltTemplate)(
      structural.rebuiltTemplate.constr,
      structural.rebuiltTemplate.parentsOrDerived,
      structural.rebuiltTemplate.derived,
      structural.rebuiltTemplate.self,
      structural.prefix ::: positionedTarget :: structural.suffix
    ).cloneIn(structural.originalTemplate.source).withSpan(structural.originalTemplate.span)
    val positionedRoot = untpd.cpy.TypeDef(structural.rebuiltRoot)(
      structural.rebuiltRoot.name,
      positionedTemplate
    ).cloneIn(structural.originalRoot.source).withSpan(structural.originalRoot.span)
    val result = Result(
      structural,
      positionedRoot,
      positionedTemplate,
      positionedTarget,
      positionedApply,
      positionedWrapper,
      positionedFunction,
      positionedSibling
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError,
    Unit] =
    val structural = result.structuralResult
    val site = structural.originalArgument
    val freshSitesValid = Vector[untpd.Tree](
      result.positionedWrapperApply,
      result.positionedWrapperFunction,
      result.positionedFreshSibling
    ).forall(tree =>
      tree.source == site.source && tree.span == site.span &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val wrapperValid =
      result.positionedWrapperApply.fun.eq(result.positionedWrapperFunction) &&
        result.positionedWrapperApply.args.size == 2 &&
        result.positionedWrapperApply.args(0).eq(site) &&
        result.positionedWrapperApply.args(1).eq(result.positionedFreshSibling) &&
        !result.positionedWrapperApply.eq(structural.wrapperApply) &&
        !result.positionedWrapperFunction.eq(structural.wrapperFunction) &&
        !result.positionedFreshSibling.eq(structural.freshSibling)
    val argumentsValid =
      result.positionedApply.args.size == structural.originalApply.args.size &&
        structural.originalApply.args.indices.forall { index =>
          if index == structural.argumentIndex then
            result.positionedApply.args(index).eq(result.positionedWrapperApply)
          else result.positionedApply.args(index).eq(structural.originalApply.args(index))
        }
    val untouchedOriginal = structural.originalTemplate.body
      .filterNot(_.eq(structural.originalTarget))
    val untouchedPositioned = result.positionedTemplate.body
      .filterNot(_.eq(result.positionedTarget))
    val containersValid =
      result.positionedRoot.source == structural.originalRoot.source &&
        result.positionedRoot.span == structural.originalRoot.span &&
        result.positionedTemplate.source == structural.originalTemplate.source &&
        result.positionedTemplate.span == structural.originalTemplate.span &&
        result.positionedTarget.source == structural.originalTarget.source &&
        result.positionedTarget.span == structural.originalTarget.span &&
        result.positionedApply.source == structural.originalApply.source &&
        result.positionedApply.span == structural.originalApply.span &&
        result.positionedApply.fun.eq(structural.originalApply.fun) &&
        result.positionedTarget.rhs.eq(result.positionedApply) &&
        result.positionedRoot.rhs.eq(result.positionedTemplate) &&
        result.positionedRoot.mods.eq(structural.originalRoot.mods) &&
        result.positionedTemplate.constr.eq(structural.originalTemplate.constr) &&
        result.positionedTemplate.parentsOrDerived.eq(
          structural.originalTemplate.parentsOrDerived
        ) &&
        result.positionedTemplate.derived.eq(structural.originalTemplate.derived) &&
        result.positionedTemplate.self.eq(structural.originalTemplate.self) &&
        result.positionedTarget.mods.eq(structural.originalTarget.mods) &&
        result.positionedTarget.paramss.eq(structural.originalTarget.paramss) &&
        result.positionedTarget.tpt.eq(structural.originalTarget.tpt) &&
        untouchedOriginal.size == untouchedPositioned.size &&
        untouchedOriginal.indices.forall(index =>
          untouchedOriginal(index).eq(untouchedPositioned(index))
        ) && argumentsValid
    Either.cond(
      freshSitesValid && wrapperValid && containersValid,
      (),
      error("ORIGIN_ADAPTATION_INVARIANT_FAILED",
        "the U019 S4 result violated its bounded identity/site contract.")
    )

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError =
    ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError(code, detail)
