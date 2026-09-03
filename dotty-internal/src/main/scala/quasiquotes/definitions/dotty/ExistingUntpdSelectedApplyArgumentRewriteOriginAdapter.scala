package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Applies granular original-site attribution to one U014 structural result. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentRewriteOriginAdapter:
  final case class Result(
      structuralResult: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      positionedRoot: untpd.TypeDef,
      positionedTemplate: untpd.Template,
      positionedTarget: untpd.DefDef,
      positionedApply: untpd.Apply,
      positionedReplacement: untpd.Tree
  )

  def adapt(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Result] =
    try
      for
        present <- Option(structural).toRight(
          error("RESULT_REQUIRED", "the U014 structural result was null.")
        )
        _ <- validateSourceFreeReconstructedObjects(present)
        _ <- validateStructuralIdentity(present)
        _ <- validateOriginalSites(present)
        result <- position(present)
      yield result
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

  private def validateSourceFreeReconstructedObjects(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Unit] =
    val reconstructed = Vector[untpd.Tree](
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget,
      structural.rebuiltApply,
      structural.replacementLeaf
    )
    Either.cond(
      reconstructed.forall(tree =>
        tree != null && !tree.source.exists && !tree.span.exists &&
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ),
      (),
      error(
        "SOURCE_FREE_STRUCTURAL_RESULT_REQUIRED",
        "the rebuilt containers, Apply, and replacement must remain source/span/symbol-free and non-TypedSplice."
      )
    )

  private def validateStructuralIdentity(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Unit] =
    val originals = Vector[untpd.Tree](
      structural.originalRoot,
      structural.originalTemplate,
      structural.originalTarget,
      structural.originalApply,
      structural.originalArgument
    )
    val originalsPresent = originals.forall(_ != null)
    val originalBody =
      if originalsPresent then Option(structural.originalTemplate.body) else None
    val rebuiltBody = Option(structural.rebuiltTemplate.body)
    val originalArguments =
      if originalsPresent then Option(structural.originalApply.args) else None
    val rebuiltArguments = Option(structural.rebuiltApply.args)
    val targetIndices = originalBody.toVector.flatten.iterator.zipWithIndex.collect {
      case (tree, index) if tree != null && tree.eq(structural.originalTarget) => index
    }.toVector
    val argumentIndices = originalArguments.toVector.flatten.iterator.zipWithIndex.collect {
      case (tree, index) if tree != null && tree.eq(structural.originalArgument) => index
    }.toVector
    val originalSelectionValid =
      if originalsPresent then
        structural.originalApply.fun match
          case selection: untpd.Select =>
            selection.qualifier.isInstanceOf[untpd.Ident] &&
              Option(selection.name).exists(_.isTermName) &&
              selection.source.exists && selection.span.exists &&
              selection.qualifier.source.exists && selection.qualifier.span.exists
          case _ => false
      else false
    val originalArgumentsValid = originalArguments.exists(arguments =>
      arguments.size >= 2 && arguments.size <= 3 &&
        arguments.forall(argument =>
          argument != null && isAdmittedLeaf(argument) &&
            argument.source.exists && argument.span.exists
        )
    )
    val originalGraphValid =
      originalsPresent && originalBody.nonEmpty && originalArguments.nonEmpty &&
        structural.originalRoot.rhs != null &&
        structural.originalRoot.rhs.eq(structural.originalTemplate) &&
        structural.originalTarget.rhs != null &&
        structural.originalTarget.rhs.eq(structural.originalApply) &&
        Option(structural.originalTarget.paramss).exists(_.isEmpty) &&
        targetIndices.size == 1 && argumentIndices == Vector(structural.argumentIndex) &&
        originalSelectionValid && originalArgumentsValid &&
        allTrees(structural.originalApply).forall(tree =>
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
    val preservedComponentsValid =
      originalsPresent &&
        structural.rebuiltRoot.name == structural.originalRoot.name &&
        structural.rebuiltRoot.mods.eq(structural.originalRoot.mods) &&
        structural.rebuiltTemplate.constr.eq(structural.originalTemplate.constr) &&
        structural.rebuiltTemplate.parentsOrDerived.eq(
          structural.originalTemplate.parentsOrDerived
        ) &&
        structural.rebuiltTemplate.derived.eq(structural.originalTemplate.derived) &&
        structural.rebuiltTemplate.self.eq(structural.originalTemplate.self) &&
        structural.rebuiltTarget.name == structural.originalTarget.name &&
        structural.rebuiltTarget.mods.eq(structural.originalTarget.mods) &&
        structural.rebuiltTarget.paramss != null &&
        structural.rebuiltTarget.paramss.eq(structural.originalTarget.paramss) &&
        structural.rebuiltTarget.tpt.eq(structural.originalTarget.tpt)
    val bodyValid = (originalBody, rebuiltBody, targetIndices.headOption) match
      case (Some(original), Some(rebuilt), Some(targetIndex)) =>
        original.size == rebuilt.size &&
          sameIdentity(structural.prefix, original.take(targetIndex)) &&
          sameIdentity(structural.suffix, original.drop(targetIndex + 1)) &&
          rebuilt.indices.forall { index =>
            if index == targetIndex then rebuilt(index).eq(structural.rebuiltTarget)
            else rebuilt(index) != null && rebuilt(index).eq(original(index))
          }
      case _ => false
    val argumentsValid = (originalArguments, rebuiltArguments) match
      case (Some(original), Some(rebuilt)) =>
        structural.argumentIndex >= 0 &&
          structural.argumentIndex < original.size &&
          original.size == rebuilt.size &&
          isAdmittedLeaf(structural.replacementLeaf) &&
          rebuilt.indices.forall { index =>
            if index == structural.argumentIndex then
              rebuilt(index).eq(structural.replacementLeaf) &&
                !rebuilt(index).eq(structural.originalArgument)
            else rebuilt(index) != null && rebuilt(index).eq(original(index))
          }
      case _ => false
    val rebuiltGraphValid =
      structural.rebuiltRoot.rhs != null &&
        structural.rebuiltRoot.rhs.eq(structural.rebuiltTemplate) &&
        structural.rebuiltTarget.rhs != null &&
        structural.rebuiltTarget.rhs.eq(structural.rebuiltApply) &&
        structural.rebuiltApply.fun != null && originalsPresent &&
        structural.rebuiltApply.fun.eq(structural.originalApply.fun) &&
        preservedComponentsValid && bodyValid && argumentsValid
    val freshnessValid =
      originalsPresent &&
        !structural.rebuiltRoot.eq(structural.originalRoot) &&
        !structural.rebuiltTemplate.eq(structural.originalTemplate) &&
        !structural.rebuiltTarget.eq(structural.originalTarget) &&
        !structural.rebuiltApply.eq(structural.originalApply) &&
        !structural.replacementLeaf.eq(structural.originalArgument)
    val valid = originalGraphValid && rebuiltGraphValid && freshnessValid
    Either.cond(
      valid,
      (),
      error(
        "STRUCTURAL_IDENTITY_REQUIRED",
        "the U014 structural result did not preserve its exact function/untouched-argument identity contract."
      )
    )

  private def validateOriginalSites(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Unit] =
    val sites = Vector[untpd.Tree](
      structural.originalRoot,
      structural.originalTemplate,
      structural.originalTarget,
      structural.originalApply,
      structural.originalArgument
    )
    Either.cond(
      sites.forall(tree => tree != null && tree.source.exists && tree.span.exists),
      (),
      error(
        "ORIGINAL_SITE_REQUIRED",
        "the original root, template, target, Apply, and exact argument must each have a source and span."
      )
    )

  private def position(
      structural: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Result] =
    val positionedReplacement = structural.replacementLeaf
      .cloneIn(structural.originalArgument.source)
      .withSpan(structural.originalArgument.span)
    val positionedArguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => positionedReplacement
      case (argument, _) => argument
    }
    val positionedApply = untpd
      .Apply(structural.originalApply.fun, positionedArguments)
      .cloneIn(structural.originalApply.source)
      .withSpan(structural.originalApply.span)
    val positionedTarget = untpd
      .cpy
      .DefDef(structural.rebuiltTarget)(
        structural.rebuiltTarget.name,
        structural.rebuiltTarget.paramss,
        structural.rebuiltTarget.tpt,
        positionedApply
      )
      .cloneIn(structural.originalTarget.source)
      .withSpan(structural.originalTarget.span)
    val positionedTemplate = untpd
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
    val positionedRoot = untpd
      .cpy
      .TypeDef(structural.rebuiltRoot)(structural.rebuiltRoot.name, positionedTemplate)
      .cloneIn(structural.originalRoot.source)
      .withSpan(structural.originalRoot.span)
    val result = Result(
      structural,
      positionedRoot,
      positionedTemplate,
      positionedTarget,
      positionedApply,
      positionedReplacement
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentRewriteOriginError, Unit] =
    val structural = result.structuralResult
    val originalArguments = structural.originalApply.args
    val positionedArguments = result.positionedApply.args
    val argumentsValid =
      originalArguments.size == positionedArguments.size &&
        originalArguments.indices.forall { index =>
          if index == structural.argumentIndex then
            positionedArguments(index).eq(result.positionedReplacement) &&
              !positionedArguments(index).eq(structural.originalArgument) &&
              !positionedArguments(index).eq(structural.replacementLeaf)
          else positionedArguments(index).eq(originalArguments(index))
        }
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
        !result.positionedApply.eq(structural.originalApply) &&
        !result.positionedApply.eq(structural.rebuiltApply) &&
        !result.positionedReplacement.eq(structural.originalArgument) &&
        !result.positionedReplacement.eq(structural.replacementLeaf) &&
        result.positionedRoot.source == structural.originalRoot.source &&
        result.positionedRoot.span == structural.originalRoot.span &&
        result.positionedTemplate.source == structural.originalTemplate.source &&
        result.positionedTemplate.span == structural.originalTemplate.span &&
        result.positionedTarget.source == structural.originalTarget.source &&
        result.positionedTarget.span == structural.originalTarget.span &&
        result.positionedApply.source == structural.originalApply.source &&
        result.positionedApply.span == structural.originalApply.span &&
        result.positionedReplacement.source == structural.originalArgument.source &&
        result.positionedReplacement.span == structural.originalArgument.span &&
        result.positionedApply.fun.eq(structural.originalApply.fun) &&
        result.positionedTarget.rhs.eq(result.positionedApply) &&
        result.positionedRoot.mods.eq(structural.originalRoot.mods) &&
        result.positionedTemplate.constr.eq(structural.originalTemplate.constr) &&
        result.positionedTemplate.parentsOrDerived.eq(
          structural.originalTemplate.parentsOrDerived
        ) &&
        result.positionedTemplate.derived.eq(structural.originalTemplate.derived) &&
        result.positionedTemplate.self.eq(structural.originalTemplate.self) &&
        result.positionedTarget.mods.eq(structural.originalTarget.mods) &&
        result.positionedTarget.tpt.eq(structural.originalTarget.tpt) &&
        originalUntouched.size == positionedUntouched.size &&
        originalUntouched.zip(positionedUntouched).forall((left, right) => left.eq(right)) &&
        argumentsValid &&
        allTrees(result.positionedRoot).forall(tree =>
          tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
        )
    Either.cond(
      valid,
      (),
      error(
        "ORIGIN_ADAPTATION_INVARIANT_FAILED",
        "the granular origin result violated its bounded identity/site contract."
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

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def sameIdentity(left: List[untpd.Tree], right: List[untpd.Tree]): Boolean =
    left.size == right.size && left.indices.forall { index =>
      left(index) != null && right(index) != null && left(index).eq(right(index))
    }

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentRewriteOriginError =
    ExistingUntpdSelectedApplyArgumentRewriteOriginError(code, detail)
