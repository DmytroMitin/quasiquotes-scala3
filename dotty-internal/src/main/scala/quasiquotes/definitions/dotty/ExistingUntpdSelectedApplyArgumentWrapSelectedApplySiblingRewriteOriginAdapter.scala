package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Applies U021 T7 argument-site origins without changing the exact preserved first child. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter:
  final case class Result(
      structuralResult: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result,
      positionedRoot: untpd.TypeDef,
      positionedTemplate: untpd.Template,
      positionedTarget: untpd.DefDef,
      positionedApply: untpd.Apply,
      positionedWrapperApply: untpd.Apply,
      positionedWrapperFunction: untpd.Ident,
      positionedFreshSiblingApply: untpd.Apply,
      positionedFreshSiblingSelection: untpd.Select,
      positionedFreshSiblingQualifier: untpd.Ident,
      positionedFreshSiblingArguments: List[untpd.Tree]
  )

  def adapt(
      structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result
  )(using Context): Either[
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, Result] =
    try
      for
        present <- Option(structural).toRight(error("RESULT_REQUIRED",
          "the U021 structural result was null."))
        _ <- validate(present)
        result <- position(present)
      yield result
    catch
      case NonFatal(exception) => Left(error("ORIGIN_ADAPTATION_FAILED",
        Option(exception.getMessage).filter(_.nonEmpty).getOrElse(exception.getClass.getSimpleName)))

  private def validate(
      structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result
  )(using Context): Either[
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, Unit] =
    val existing = structural.validatedExisting
    val required = Vector[AnyRef](existing, structural.rebuiltRoot, structural.rebuiltTemplate,
      structural.rebuiltTarget, structural.rebuiltApply, structural.wrapperApply,
      structural.suppliedFreshSiblingApply, structural.suppliedFreshSiblingSelection,
      structural.suppliedFreshSiblingQualifier, structural.suppliedFreshSiblingArguments)
    if required.exists(_ == null) || existing.originalRoot == null ||
        existing.originalTemplate == null || existing.originalTarget == null ||
        existing.originalApply == null || existing.originalArgument == null ||
        structural.wrapperApply.fun == null || structural.wrapperApply.args == null ||
        structural.suppliedFreshSiblingApply.fun == null ||
        structural.suppliedFreshSiblingApply.args == null then
      Left(error("STRUCTURAL_IDENTITY_REQUIRED",
        "the U021 structural carrier or one of its required graph nodes was null."))
    else
      ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter.adapt(existing)
        .left.map(problem => error(problem.code, problem.detail))
        .flatMap(_ => validateGraph(structural))

  private def validateGraph(
      structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result
  )(using Context): Either[
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, Unit] =
    val existing = structural.validatedExisting
    val selection = structural.suppliedFreshSiblingSelection
    val arguments = structural.suppliedFreshSiblingArguments
    val fresh = Vector[untpd.Tree](structural.rebuiltRoot, structural.rebuiltTemplate,
      structural.rebuiltTarget, structural.rebuiltApply, structural.wrapperApply,
      structural.wrapperFunction, structural.suppliedFreshSiblingApply, selection,
      structural.suppliedFreshSiblingQualifier) ++ arguments
    val detached = fresh.forall(tree => tree != null && !tree.source.exists &&
      !tree.span.exists && tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice])
    val wrapperValid = structural.wrapperApply.args.size == 2 &&
      structural.wrapperApply.fun.eq(structural.wrapperFunction) &&
      structural.wrapperApply.args(0).eq(structural.originalArgument) &&
      structural.wrapperApply.args(1).eq(structural.suppliedFreshSiblingApply)
    val siblingValid = structural.suppliedFreshSiblingApply.fun.eq(selection) &&
      selection.qualifier.eq(structural.suppliedFreshSiblingQualifier) &&
      selection.name == structural.suppliedFreshSiblingMemberName &&
      structural.suppliedFreshSiblingApply.args.size == arguments.size &&
      arguments.size >= 1 && arguments.size <= 3 &&
      arguments.indices.forall(index =>
        structural.suppliedFreshSiblingApply.args(index).eq(arguments(index)))
    val outerArgumentsValid = structural.rebuiltApply.args.size == structural.originalApply.args.size &&
      structural.originalApply.args.indices.forall { index =>
        if index == structural.argumentIndex then
          structural.rebuiltApply.args(index).eq(structural.wrapperApply)
        else structural.rebuiltApply.args(index).eq(structural.originalApply.args(index))
      }
    val bodyValid = structural.rebuiltTemplate.body.size == structural.originalTemplate.body.size &&
      structural.rebuiltTemplate.body.indices.forall { index =>
        val original = structural.originalTemplate.body(index)
        if original.eq(structural.originalTarget) then
          structural.rebuiltTemplate.body(index).eq(structural.rebuiltTarget)
        else structural.rebuiltTemplate.body(index).eq(original)
      }
    val containersValid = structural.originalRoot.eq(existing.originalRoot) &&
      structural.originalTemplate.eq(existing.originalTemplate) &&
      structural.originalTarget.eq(existing.originalTarget) &&
      structural.originalApply.eq(existing.originalApply) &&
      structural.originalArgument.eq(existing.originalArgument) &&
      structural.argumentIndex == existing.argumentIndex &&
      structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
      structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
      structural.rebuiltApply.fun.eq(structural.originalApply.fun) &&
      !structural.rebuiltRoot.eq(structural.originalRoot) &&
      !structural.rebuiltTemplate.eq(structural.originalTemplate) &&
      !structural.rebuiltTarget.eq(structural.originalTarget) &&
      !structural.rebuiltApply.eq(structural.originalApply) &&
      outerArgumentsValid && bodyValid
    Either.cond(detached && wrapperValid && siblingValid && containersValid, (), error(
      "STRUCTURAL_IDENTITY_REQUIRED",
      "the U021 structural result did not preserve its exact mixed-provenance graph."))

  private def position(
      structural: ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter.Result
  )(using Context): Either[
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, Result] =
    val site = structural.originalArgument
    val source = site.source
    val span = site.span
    val positionedWrapperFunction = structural.wrapperFunction.cloneIn(source)
      .withSpan(span).asInstanceOf[untpd.Ident]
    val positionedQualifier = structural.suppliedFreshSiblingQualifier.cloneIn(source)
      .withSpan(span).asInstanceOf[untpd.Ident]
    val positionedSelection = untpd.Select(positionedQualifier,
      structural.suppliedFreshSiblingMemberName).cloneIn(source)
      .withSpan(span).asInstanceOf[untpd.Select]
    val positionedArguments = structural.suppliedFreshSiblingArguments.map(
      _.cloneIn(source).withSpan(span))
    val positionedSibling = untpd.Apply(positionedSelection, positionedArguments)
      .cloneIn(source).withSpan(span)
    val positionedWrapper = untpd.Apply(positionedWrapperFunction,
      structural.originalArgument :: positionedSibling :: Nil)
      .cloneIn(source).withSpan(span)
    val outerArguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => positionedWrapper
      case (argument, _) => argument
    }
    val positionedApply = untpd.Apply(structural.originalApply.fun, outerArguments)
      .cloneIn(structural.originalApply.source).withSpan(structural.originalApply.span)
    val positionedTarget = untpd.cpy.DefDef(structural.rebuiltTarget)(
      structural.rebuiltTarget.name, structural.rebuiltTarget.paramss,
      structural.rebuiltTarget.tpt, positionedApply)
      .cloneIn(structural.originalTarget.source).withSpan(structural.originalTarget.span)
    val positionedTemplate = untpd.cpy.Template(structural.rebuiltTemplate)(
      structural.rebuiltTemplate.constr, structural.rebuiltTemplate.parentsOrDerived,
      structural.rebuiltTemplate.derived, structural.rebuiltTemplate.self,
      structural.prefix ::: positionedTarget :: structural.suffix)
      .cloneIn(structural.originalTemplate.source).withSpan(structural.originalTemplate.span)
    val positionedRoot = untpd.cpy.TypeDef(structural.rebuiltRoot)(
      structural.rebuiltRoot.name, positionedTemplate)
      .cloneIn(structural.originalRoot.source).withSpan(structural.originalRoot.span)
    val result = Result(structural, positionedRoot, positionedTemplate, positionedTarget,
      positionedApply, positionedWrapper, positionedWrapperFunction, positionedSibling,
      positionedSelection, positionedQualifier, positionedArguments)
    verify(result).map(_ => result)

  private def verify(result: Result)(using Context): Either[
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, Unit] =
    val structural = result.structuralResult
    val site = structural.originalArgument
    val fresh = Vector[untpd.Tree](result.positionedWrapperApply,
      result.positionedWrapperFunction, result.positionedFreshSiblingApply,
      result.positionedFreshSiblingSelection, result.positionedFreshSiblingQualifier) ++
      result.positionedFreshSiblingArguments
    val sitesValid = fresh.forall(tree => tree.source == site.source &&
      tree.span == site.span && tree.symbol == NoSymbol &&
      !tree.isInstanceOf[untpd.TypedSplice])
    val linksValid = result.positionedWrapperApply.fun.eq(result.positionedWrapperFunction) &&
      result.positionedWrapperApply.args(0).eq(structural.originalArgument) &&
      result.positionedWrapperApply.args(1).eq(result.positionedFreshSiblingApply) &&
      result.positionedFreshSiblingApply.fun.eq(result.positionedFreshSiblingSelection) &&
      result.positionedFreshSiblingSelection.qualifier.eq(result.positionedFreshSiblingQualifier) &&
      result.positionedFreshSiblingSelection.name == structural.suppliedFreshSiblingMemberName &&
      result.positionedFreshSiblingApply.args.size == result.positionedFreshSiblingArguments.size &&
      result.positionedFreshSiblingArguments.indices.forall(index =>
        result.positionedFreshSiblingApply.args(index)
          .eq(result.positionedFreshSiblingArguments(index)))
    val freshIdentity = !result.positionedWrapperApply.eq(structural.wrapperApply) &&
      !result.positionedWrapperFunction.eq(structural.wrapperFunction) &&
      !result.positionedFreshSiblingApply.eq(structural.suppliedFreshSiblingApply) &&
      !result.positionedFreshSiblingSelection.eq(structural.suppliedFreshSiblingSelection) &&
      !result.positionedFreshSiblingQualifier.eq(structural.suppliedFreshSiblingQualifier) &&
      result.positionedFreshSiblingArguments.indices.forall(index =>
        !result.positionedFreshSiblingArguments(index)
          .eq(structural.suppliedFreshSiblingArguments(index)))
    val argumentsValid = result.positionedApply.args.size == structural.originalApply.args.size &&
      structural.originalApply.args.indices.forall { index =>
        if index == structural.argumentIndex then
          result.positionedApply.args(index).eq(result.positionedWrapperApply)
        else result.positionedApply.args(index).eq(structural.originalApply.args(index))
      }
    val containersValid = result.positionedRoot.source == structural.originalRoot.source &&
      result.positionedRoot.span == structural.originalRoot.span &&
      result.positionedTemplate.source == structural.originalTemplate.source &&
      result.positionedTemplate.span == structural.originalTemplate.span &&
      result.positionedTarget.source == structural.originalTarget.source &&
      result.positionedTarget.span == structural.originalTarget.span &&
      result.positionedApply.source == structural.originalApply.source &&
      result.positionedApply.span == structural.originalApply.span &&
      result.positionedApply.fun.eq(structural.originalApply.fun) &&
      result.positionedTarget.rhs.eq(result.positionedApply) &&
      result.positionedRoot.rhs.eq(result.positionedTemplate) && argumentsValid
    Either.cond(sitesValid && linksValid && freshIdentity && containersValid, (), error(
      "ORIGIN_ADAPTATION_INVARIANT_FAILED",
      "the U021 T7 result violated its bounded identity/site contract."))

  private def error(code: String, detail: String) =
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError(code, detail)
