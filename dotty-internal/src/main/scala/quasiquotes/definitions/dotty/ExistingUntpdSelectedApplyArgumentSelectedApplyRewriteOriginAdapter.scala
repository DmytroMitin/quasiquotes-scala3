package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Applies U016 uniform argument-site attribution without widening U014 or U015. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter:
  final case class Result(
      structuralResult: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result,
      positionedRoot: untpd.TypeDef,
      positionedTemplate: untpd.Template,
      positionedTarget: untpd.DefDef,
      positionedApply: untpd.Apply,
      positionedReplacement: untpd.Apply,
      positionedReplacementSelection: untpd.Select,
      positionedReplacementQualifier: untpd.Ident,
      positionedReplacementArguments: List[untpd.Tree]
  )

  def adapt(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Result] =
    try
      for
        present <- Option(structural).toRight(
          error("RESULT_REQUIRED", "the U016 structural result was null.")
        )
        _ <- validateReplacement(present.replacementApply)
        _ <- validateStructuralPresence(present)
        _ <- validateStructuralGraph(present)
        _ <- validateExistingAndSites(present)
        result <- position(present)
      yield result
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "ORIGIN_ADAPTATION_FAILED",
            Option(exception.getMessage).filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def validateStructuralPresence(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Unit] =
    val existing = structural.validatedExisting
    val valid =
      existing != null &&
        existing.originalRoot != null && existing.originalTemplate != null &&
        existing.originalTarget != null && existing.originalApply != null &&
        existing.originalArgument != null && existing.rebuiltRoot != null &&
        existing.rebuiltTemplate != null && existing.rebuiltTarget != null &&
        existing.rebuiltApply != null && existing.replacementLeaf != null &&
        existing.originalTemplate.body != null && existing.originalApply.args != null &&
        structural.rebuiltRoot != null && structural.rebuiltTemplate != null &&
        structural.rebuiltTarget != null && structural.rebuiltApply != null &&
        structural.rebuiltTemplate.body != null && structural.rebuiltApply.args != null
    Either.cond(
      valid,
      (),
      error(
        "STRUCTURAL_IDENTITY_REQUIRED",
        "the U016 structural carrier or one of its required graph nodes was null."
      )
    )

  private def validateExistingAndSites(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Unit] =
    ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter
      .adapt(structural.validatedExisting)
      .left.map(problem => error(problem.code, problem.detail))
      .map(_ => ())

  private def validateReplacement(
      replacement: untpd.Apply
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Unit] =
    val selection = Option(replacement).flatMap(value => Option(value.fun)).collect {
      case value: untpd.Select => value
    }
    val qualifier = selection.flatMap(value => Option(value.qualifier)).collect {
      case value: untpd.Ident => value
    }
    val selectionValid = selection.exists(value =>
      value.name != null && value.name.isTermName && qualifier.nonEmpty
    )
    val arguments = Option(replacement).flatMap(value => Option(value.args))
    val argumentsValid = arguments.exists(values =>
      values.size >= 1 && values.size <= 3 &&
        values.forall(value => value != null && isAdmittedLeaf(value))
    )
    val nodes = Option(replacement).toVector ++ selection.toVector ++
      qualifier.toVector ++ arguments.toVector.flatten
    val provenanceValid = nodes.nonEmpty && nodes.forall(tree =>
      !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol &&
        !tree.isInstanceOf[untpd.TypedSplice]
    )
    Either.cond(
      selectionValid && argumentsValid && provenanceValid,
      (),
      error(
        "SOURCE_FREE_REPLACEMENT_SELECTED_APPLY_REQUIRED",
        "the complete replacement must remain a source/span/symbol-free direct-Ident selected-member Apply with 1..3 direct leaves."
      )
    )

  private def validateStructuralGraph(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Unit] =
    val existing = structural.validatedExisting
    val reconstructed = Vector[untpd.Tree](
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget,
      structural.rebuiltApply
    )
    val reconstructedValid = reconstructed.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val argumentsValid =
      structural.rebuiltApply.args != null &&
        structural.rebuiltApply.args.size == structural.originalApply.args.size &&
        structural.originalApply.args.indices.forall { index =>
          if index == structural.argumentIndex then
            structural.rebuiltApply.args(index).eq(structural.replacementApply) &&
              !structural.replacementApply.eq(structural.originalArgument)
          else structural.rebuiltApply.args(index).eq(structural.originalApply.args(index))
        }
    val bodyValid =
      structural.rebuiltTemplate.body != null &&
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
        structural.rebuiltTemplate.parentsOrDerived.eq(structural.originalTemplate.parentsOrDerived) &&
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
        argumentsValid && bodyValid
    Either.cond(
      reconstructedValid && graphValid,
      (),
      error(
        "STRUCTURAL_IDENTITY_REQUIRED",
        "the U016 structural result did not preserve the exact U014 existing graph and selected-member replacement identity contract."
      )
    )

  private def position(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Result] =
    val site = structural.originalArgument
    val source = site.source
    val span = site.span
    val structuralSelection = structural.replacementApply.fun.asInstanceOf[untpd.Select]
    val positionedQualifier = structuralSelection.qualifier
      .cloneIn(source).withSpan(span).asInstanceOf[untpd.Ident]
    val positionedSelection = untpd
      .Select(positionedQualifier, structuralSelection.name)
      .cloneIn(source).withSpan(span).asInstanceOf[untpd.Select]
    val positionedArguments = structural.replacementApply.args.map(
      _.cloneIn(source).withSpan(span)
    )
    val positionedReplacement = untpd
      .Apply(positionedSelection, positionedArguments)
      .cloneIn(source).withSpan(span)
    val outerArguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => positionedReplacement
      case (argument, _) => argument
    }
    val positionedApply = untpd
      .Apply(structural.originalApply.fun, outerArguments)
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
      positionedReplacement,
      positionedSelection,
      positionedQualifier,
      positionedArguments
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, Unit] =
    val structural = result.structuralResult
    val site = structural.originalArgument
    val structuralSelection = structural.replacementApply.fun.asInstanceOf[untpd.Select]
    val replacementNodes = result.positionedReplacement +:
      result.positionedReplacementSelection +:
      result.positionedReplacementQualifier +:
      result.positionedReplacementArguments.toVector
    val replacementSitesValid = replacementNodes.forall(tree =>
      tree.source == site.source && tree.span == site.span &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val replacementLinksValid =
      result.positionedReplacement.fun.eq(result.positionedReplacementSelection) &&
        result.positionedReplacementSelection.qualifier.eq(result.positionedReplacementQualifier) &&
        result.positionedReplacementSelection.name == structuralSelection.name &&
        result.positionedReplacement.args.size == result.positionedReplacementArguments.size &&
        result.positionedReplacementArguments.indices.forall(index =>
          result.positionedReplacement.args(index).eq(result.positionedReplacementArguments(index))
        )
    val replacementFresh =
      !result.positionedReplacement.eq(structural.replacementApply) &&
        !result.positionedReplacementSelection.eq(structuralSelection) &&
        !result.positionedReplacementQualifier.eq(structuralSelection.qualifier) &&
        result.positionedReplacementArguments.indices.forall(index =>
          !result.positionedReplacementArguments(index).eq(structural.replacementApply.args(index))
        )
    val argumentsValid =
      result.positionedApply.args.size == structural.originalApply.args.size &&
        structural.originalApply.args.indices.forall { index =>
          if index == structural.argumentIndex then
            result.positionedApply.args(index).eq(result.positionedReplacement)
          else result.positionedApply.args(index).eq(structural.originalApply.args(index))
        }
    val untouchedOriginal = structural.originalTemplate.body.filterNot(_.eq(structural.originalTarget))
    val untouchedPositioned = result.positionedTemplate.body.filterNot(_.eq(result.positionedTarget))
    val containersValid =
      !result.positionedRoot.eq(structural.originalRoot) &&
        !result.positionedRoot.eq(structural.rebuiltRoot) &&
        !result.positionedTemplate.eq(structural.originalTemplate) &&
        !result.positionedTemplate.eq(structural.rebuiltTemplate) &&
        !result.positionedTarget.eq(structural.originalTarget) &&
        !result.positionedTarget.eq(structural.rebuiltTarget) &&
        !result.positionedApply.eq(structural.originalApply) &&
        !result.positionedApply.eq(structural.rebuiltApply) &&
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
        result.positionedTemplate.parentsOrDerived.eq(structural.originalTemplate.parentsOrDerived) &&
        result.positionedTemplate.derived.eq(structural.originalTemplate.derived) &&
        result.positionedTemplate.self.eq(structural.originalTemplate.self) &&
        result.positionedTarget.mods.eq(structural.originalTarget.mods) &&
        result.positionedTarget.paramss.eq(structural.originalTarget.paramss) &&
        result.positionedTarget.tpt.eq(structural.originalTarget.tpt) &&
        untouchedOriginal.size == untouchedPositioned.size &&
        untouchedOriginal.indices.forall(index => untouchedOriginal(index).eq(untouchedPositioned(index))) &&
        argumentsValid
    Either.cond(
      replacementSitesValid && replacementLinksValid && replacementFresh && containersValid,
      (),
      error("ORIGIN_ADAPTATION_INVARIANT_FAILED",
        "the U016 P4 result violated its bounded identity/site contract.")
    )

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] || tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError =
    ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError(code, detail)
