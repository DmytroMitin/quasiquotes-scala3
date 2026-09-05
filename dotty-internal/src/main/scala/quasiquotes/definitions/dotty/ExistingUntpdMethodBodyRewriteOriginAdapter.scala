package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol

/** Attributes a bounded U002 rewrite to the original sites it transforms. */
private[quasiquotes] object ExistingUntpdMethodBodyRewriteOriginAdapter:
  private val MaxApplyArguments = 3

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
    Option(structural)
      .toRight(error("RESULT_REQUIRED", "the U002 structural result was null."))
      .flatMap(validateStructuralCarrier)
      .flatMap(validateSourceFreeInput)
      .flatMap(validateSingleNodeReplacement)
      .flatMap(validateOriginalSites)
      .flatMap(value => reconstructSafely(positionSingleNode(value)))

  /** Attributes one direct-Ident Apply and 1..3 leaf arguments uniformly to the replaced RHS site. */
  def adaptApply(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    Option(structural)
      .toRight(error("RESULT_REQUIRED", "the U002 structural result was null."))
      .flatMap(validateStructuralCarrier)
      .flatMap(validateSourceFreeInput)
      .flatMap(validateApplyReplacement)
      .flatMap(validated =>
        validateOriginalSites(validated.structural).map(_ => validated)
      )
      .flatMap(value => reconstructSafely(positionApply(value)))

  /** Attributes one direct-Ident-qualified selected Apply and 1..3 leaf arguments
    * uniformly to the replaced RHS site.
    */
  def adaptSelectedApply(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    Option(structural)
      .toRight(error("RESULT_REQUIRED", "the U002 structural result was null."))
      .flatMap(validateStructuralCarrier)
      .flatMap(validateSelectedApplyReplacement)
      .flatMap(validated =>
        validateSourceFreeInput(validated.structural).map(_ => validated)
      )
      .flatMap(validated =>
        validateOriginalSites(validated.structural).map(_ => validated)
      )
      .flatMap(value => reconstructSafely(positionSelectedApply(value)))

  private def validateStructuralCarrier(
      structural: ExistingUntpdMethodBodyRewriter.Result
  ): Either[ExistingUntpdMethodBodyRewriteOriginError, ExistingUntpdMethodBodyRewriter.Result] =
    val requiredTrees = Vector[untpd.Tree](
      structural.originalRoot,
      structural.originalTemplate,
      structural.originalTarget,
      structural.rebuiltRoot,
      structural.rebuiltTemplate,
      structural.rebuiltTarget
    )
    val listsPresent =
      Option(structural.prefix).exists(_.forall(_ != null)) &&
        Option(structural.suffix).exists(_.forall(_ != null))
    val treeGraphsPresent =
      requiredTrees.forall(tree =>
        tree != null && !ExistingUntpdMethodBodyRewriter.rawTreeGraphHasNull(tree)
      )
    Either.cond(
      listsPresent && treeGraphsPresent,
      structural,
      error(
        "ORIGIN_ADAPTATION_FAILED",
        "the U002 structural result contained a null tree, child, or child sequence."
      )
    )

  private def validateSourceFreeInput(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, ExistingUntpdMethodBodyRewriter.Result] =
    if structural.replacementBody == null ||
      ExistingUntpdMethodBodyRewriter.rawTreeGraphHasNull(structural.replacementBody)
    then
      Left(
        error(
          "ORIGIN_ADAPTATION_FAILED",
          "the U002 replacement graph contained a null tree, child, or child sequence."
        )
      )
    else
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
            "the fresh rebuilt root/Template/method shells and complete replacement must remain source/span/symbol-free and contain no TypedSplice; exact preserved children retain their original provenance."
          )
        )
      else Right(structural)

  private inline def reconstructSafely(
      reconstruction: => Either[ExistingUntpdMethodBodyRewriteOriginError, Result]
  ): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    try reconstruction
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

  private def validateSingleNodeReplacement(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, ExistingUntpdMethodBodyRewriter.Result] =
    Either.cond(
      allTrees(structural.replacementBody).size == 1,
      structural,
      error(
        "REPLACEMENT_CHILDREN_UNSUPPORTED",
        "U003 admits only a single-node replacement; child-bearing replacements require a wider positioning policy."
      )
    )

  private def validateOriginalSites(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, ExistingUntpdMethodBodyRewriter.Result] =
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

  private final case class ValidatedApply(
      structural: ExistingUntpdMethodBodyRewriter.Result,
      replacement: untpd.Apply,
      function: untpd.Ident,
      arguments: List[untpd.Tree]
  )

  private final case class ValidatedSelectedApply(
      structural: ExistingUntpdMethodBodyRewriter.Result,
      replacement: untpd.Apply,
      selection: untpd.Select,
      qualifier: untpd.Ident,
      arguments: List[untpd.Tree]
  )

  private def validateApplyReplacement(
      structural: ExistingUntpdMethodBodyRewriter.Result
  ): Either[ExistingUntpdMethodBodyRewriteOriginError, ValidatedApply] =
    structural.replacementBody match
      case replacement: untpd.Apply =>
        replacement.fun match
          case function: untpd.Ident =>
            if replacement.args.isEmpty || replacement.args.size > MaxApplyArguments then
              Left(
                error(
                  "APPLY_ARGUMENT_COUNT_REQUIRED",
                  s"U005 admits 1..$MaxApplyArguments Apply arguments; found ${replacement.args.size}."
                )
              )
            else
              replacement.args.find(argument => !isAdmittedApplyLeaf(argument)) match
                case Some(argument) =>
                  Left(
                    error(
                      "APPLY_ARGUMENT_LEAF_REQUIRED",
                      s"U005 Apply arguments must be direct Ident, Number, or Literal leaves; found ${nodeKind(argument)}."
                    )
                  )
                case None =>
                  Right(
                    ValidatedApply(
                      structural,
                      replacement,
                      function,
                      replacement.args
                    )
                  )
          case other =>
            Left(
              error(
                "APPLY_FUNCTION_IDENT_REQUIRED",
                s"U005 requires one direct Ident Apply function; found ${nodeKind(other)}."
              )
            )
      case other =>
        Left(
          error(
            "APPLY_REPLACEMENT_REQUIRED",
            s"U005 requires one ordinary Apply replacement; found ${nodeKind(other)}."
          )
        )

  private def validateSelectedApplyReplacement(
      structural: ExistingUntpdMethodBodyRewriter.Result
  ): Either[ExistingUntpdMethodBodyRewriteOriginError, ValidatedSelectedApply] =
    structural.replacementBody match
      case replacement: untpd.Apply =>
        Option(replacement.args)
          .toRight(
            error(
              "ORIGIN_ADAPTATION_FAILED",
              "the U013 replacement Apply argument sequence was null."
            )
          )
          .flatMap { arguments =>
            if arguments.isEmpty || arguments.size > MaxApplyArguments then
              Left(
                error(
                  "SELECTED_APPLY_ARGUMENT_COUNT_REQUIRED",
                  s"U013 admits 1..$MaxApplyArguments selected Apply arguments; found ${arguments.size}."
                )
              )
            else
              replacement.fun match
                case selection: untpd.Select =>
                  selection.qualifier match
                    case qualifier: untpd.Ident =>
                      if Option(selection.name).forall(name => !name.isTermName) then
                        Left(
                          error(
                            "SELECTED_APPLY_NAME_TERM_REQUIRED",
                            s"U013 requires a non-null term-name selection; found ${Option(selection.name).fold("null")(_.toString)}."
                          )
                        )
                      else
                        arguments.find(argument =>
                          Option(argument).forall(value =>
                            !isAdmittedSelectedApplyLeafForValidation(value)
                          )
                        ) match
                          case Some(argument) =>
                            Left(
                              error(
                                "SELECTED_APPLY_ARGUMENT_LEAF_REQUIRED",
                                s"U013 selected Apply arguments must be direct Ident, Number, or Literal leaves; found ${nodeKind(argument)}."
                              )
                            )
                          case None =>
                            Right(
                              ValidatedSelectedApply(
                                structural,
                                replacement,
                                selection,
                                qualifier,
                                arguments
                              )
                            )
                    case other =>
                      Left(
                        error(
                          "SELECTED_APPLY_QUALIFIER_IDENT_REQUIRED",
                          s"U013 requires one direct Ident selection qualifier; found ${nodeKind(other)}."
                        )
                      )
                case other =>
                  Left(
                    error(
                      "SELECTED_APPLY_FUNCTION_SELECT_REQUIRED",
                      s"U013 requires one selected-member Apply function; found ${nodeKind(other)}."
                    )
                  )
          }
      case other =>
        Left(
          error(
            "SELECTED_APPLY_REPLACEMENT_REQUIRED",
            s"U013 requires one ordinary selected-member Apply replacement; found ${nodeKind(other)}."
          )
        )

  private def isAdmittedApplyLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def isAdmittedSelectedApplyLeafForValidation(tree: untpd.Tree): Boolean =
    isAdmittedApplyLeaf(tree) || tree.isInstanceOf[untpd.TypedSplice]

  private def positionSingleNode(
      structural: ExistingUntpdMethodBodyRewriter.Result
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    val positionedReplacement = structural.replacementBody
      .cloneIn(structural.originalTarget.rhs.source)
      .withSpan(structural.originalTarget.rhs.span)
    positionContainers(structural, positionedReplacement)

  private def positionApply(
      validated: ValidatedApply
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    val structural = validated.structural
    val source = structural.originalTarget.rhs.source
    val span = structural.originalTarget.rhs.span
    val positionedFunction = validated.function.cloneIn(source).withSpan(span)
    val positionedArguments = validated.arguments.map(_.cloneIn(source).withSpan(span))
    val positionedReplacement = untpd
      .Apply(positionedFunction, positionedArguments)
      .cloneIn(source)
      .withSpan(span)
    positionContainers(structural, positionedReplacement)

  private def positionSelectedApply(
      validated: ValidatedSelectedApply
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
    val structural = validated.structural
    val source = structural.originalTarget.rhs.source
    val span = structural.originalTarget.rhs.span
    val positionedQualifier = validated.qualifier.cloneIn(source).withSpan(span)
    val positionedSelection = untpd
      .Select(positionedQualifier, validated.selection.name)
      .cloneIn(source)
      .withSpan(span)
    val positionedArguments = validated.arguments.map(_.cloneIn(source).withSpan(span))
    val positionedReplacement = untpd
      .Apply(positionedSelection, positionedArguments)
      .cloneIn(source)
      .withSpan(span)
    positionContainers(structural, positionedReplacement)

  private def positionContainers(
      structural: ExistingUntpdMethodBodyRewriter.Result,
      positionedReplacement: untpd.Tree
  )(using Context): Either[ExistingUntpdMethodBodyRewriteOriginError, Result] =
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
    val structuralReplacementNodes = allTrees(structural.replacementBody)
    val positionedReplacementNodes = allTrees(result.positionedReplacement)
    val replacementNodesFresh =
      structuralReplacementNodes.size == positionedReplacementNodes.size &&
        structuralReplacementNodes.zip(positionedReplacementNodes).forall((left, right) =>
          !left.eq(right)
        )
    val replacementOriginUniform = positionedReplacementNodes.forall(tree =>
      tree.source == structural.originalTarget.rhs.source &&
        tree.span == structural.originalTarget.rhs.span
    )
    val valid =
      !result.positionedRoot.eq(structural.originalRoot) &&
        !result.positionedRoot.eq(structural.rebuiltRoot) &&
        !result.positionedTemplate.eq(structural.originalTemplate) &&
        !result.positionedTemplate.eq(structural.rebuiltTemplate) &&
        !result.positionedTarget.eq(structural.originalTarget) &&
        !result.positionedTarget.eq(structural.rebuiltTarget) &&
        replacementNodesFresh &&
        result.positionedRoot.source == structural.originalRoot.source &&
        result.positionedRoot.span == structural.originalRoot.span &&
        result.positionedTemplate.source == structural.originalTemplate.source &&
        result.positionedTemplate.span == structural.originalTemplate.span &&
        result.positionedTarget.source == structural.originalTarget.source &&
        result.positionedTarget.span == structural.originalTarget.span &&
        result.positionedReplacement.source == structural.originalTarget.rhs.source &&
        result.positionedReplacement.span == structural.originalTarget.rhs.span &&
        replacementOriginUniform &&
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

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdMethodBodyRewriteOriginError =
    ExistingUntpdMethodBodyRewriteOriginError(code, detail)
