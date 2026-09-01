package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import AuxTypeAliasUntypedLoweringInput.*
import AuxTypeAliasUntypedLoweringInput.TypeInput.*

/** Deterministic generated origin for the one admitted AUXify-039 alias tree. */
private[quasiquotes] object AuxTypeAliasGeneratedOriginAdapter:
  private enum PlanKind:
    case Outer, Lambda, Parameter, Bounds, Refined, Applied, Alias, Identifier

  private final case class TreePlan(
      kind: PlanKind,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[TreePlan]
  ):
    def span: Span = Span(start, end, point)

  private final case class GeneratedPlan(source: String, root: TreePlan)

  def lower(
      validated: Validated,
      virtualSourceName: String
  )(using Context): Either[
    AuxTypeAliasUntypedLoweringError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(validated).toRight(
        error("VALIDATED_INPUT_REQUIRED", "the validated lowering input was null.")
      )
      virtualName <- Option(virtualSourceName).toRight(
        error(
          "GENERATED_ORIGIN_FAILED",
          "the virtual source name must be present."
        )
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_FAILED", problem.message))
      generated <- render(present)
      raw <- AuxTypeAliasUntypedLowerer.lower(present)
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.TypeDef => Right(value)
        case other =>
          Left(
            error(
              "GENERATED_ORIGIN_FAILED",
              s"positioning returned ${other.getClass.getSimpleName}, not TypeDef."
            )
          )
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(
      positioned,
      generated.source,
      source
    )

  private def render(
      validated: Validated
  ): Either[AuxTypeAliasUntypedLoweringError, GeneratedPlan] =
    val builder = new StringBuilder("type ")
    val outerPoint = builder.length
    builder.append(validated.aliasName).append('[')
    val lambdaStart = builder.length
    val parameterPlans = validated.parameters.zipWithIndex.map {
      case (parameter, index) =>
        if index > 0 then builder.append(", ")
        val parameterStart = builder.length
        builder.append(parameter.displayName)
        builder.append(" <: ")
        val boundsStart = builder.length
        val SourceName(upperBound) = parameter.upperBound.get: @unchecked
        val upper = appendIdentifier(builder, upperBound)
        TreePlan(
          PlanKind.Parameter,
          parameterStart,
          upper.end,
          parameterStart,
          Vector(
            TreePlan(
              PlanKind.Bounds,
              boundsStart,
              upper.end,
              boundsStart,
              Vector(upper)
            )
          )
        )
    }
    builder.append("] = ")
    val refinedStart = builder.length
    val SourceName(constructorName) = validated.target.constructor: @unchecked
    val constructor = appendIdentifier(builder, constructorName)
    builder.append('[')
    val arguments = validated.target.arguments.zipWithIndex.map {
      case (argument, index) =>
        if index > 0 then builder.append(", ")
        val BinderReference(_, displayName) = argument: @unchecked
        appendIdentifier(builder, displayName)
    }
    builder.append(']')
    val applied = TreePlan(
      PlanKind.Applied,
      refinedStart,
      builder.length,
      refinedStart,
      constructor +: arguments
    )
    builder.append(" { type ")
    val aliasPoint = builder.length
    val aliasStart = aliasPoint - "type ".length
    builder.append(validated.refinement.memberName).append(" = ")
    val BinderReference(_, outputName) = validated.refinement.rhs: @unchecked
    val output = appendIdentifier(builder, outputName)
    val alias = TreePlan(
      PlanKind.Alias,
      aliasStart,
      output.end,
      aliasPoint,
      Vector(output)
    )
    builder.append(" }")
    val refined = TreePlan(
      PlanKind.Refined,
      refinedStart,
      builder.length,
      refinedStart,
      Vector(applied, alias)
    )
    val lambda = TreePlan(
      PlanKind.Lambda,
      lambdaStart,
      builder.length,
      lambdaStart,
      parameterPlans :+ refined
    )
    val generated = GeneratedPlan(
      builder.toString,
      TreePlan(
        PlanKind.Outer,
        0,
        builder.length,
        outerPoint,
        Vector(lambda)
      )
    )
    validatePlan(generated).map(_ => generated)

  private def appendIdentifier(
      builder: StringBuilder,
      value: String
  ): TreePlan =
    val start = builder.length
    builder.append(value)
    TreePlan(PlanKind.Identifier, start, builder.length, start, Vector.empty)

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    val errors = Vector.newBuilder[String]
    def visit(plan: TreePlan): Unit =
      if plan.start < 0 || plan.start > plan.point || plan.point > plan.end ||
          plan.end > generated.source.length
      then errors += s"${plan.kind} has an invalid planned span"
      plan.children.foreach { child =>
        if child.start < plan.start || child.end > plan.end then
          errors += s"${plan.kind} does not contain ${child.kind}"
        visit(child)
      }
      plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
        if left.end > right.start then
          errors += s"${plan.kind} children overlap or are out of source order"
      }
    visit(generated.root)
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("GENERATED_ORIGIN_FAILED", result.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: TreePlan,
      source: SourceFile
  )(using Context): Either[AuxTypeAliasUntypedLoweringError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.TypeDef, PlanKind.Outer)
          if plan.children.size == 1 =>
        position(definition.rhs, plan.children.head, source).map(rhs =>
          untpd
            .TypeDef(definition.name, rhs)
            .withMods(definition.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (lambda: untpd.LambdaTypeTree, PlanKind.Lambda)
          if plan.children.size == lambda.tparams.size + 1 =>
        for
          parameters <- sequence(
            lambda.tparams.zip(plan.children.init).map(position(_, _, source))
          )
          body <- position(lambda.body, plan.children.last, source)
        yield untpd
          .LambdaTypeTree(
            parameters.map(_.asInstanceOf[untpd.TypeDef]),
            body
          )
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.TypeDef, PlanKind.Parameter)
          if plan.children.size == 1 =>
        position(parameter.rhs, plan.children.head, source).map(rhs =>
          untpd
            .TypeDef(parameter.name, rhs)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (bounds: untpd.TypeBoundsTree, PlanKind.Bounds)
          if plan.children.size == 1 && bounds.lo.isEmpty &&
            bounds.alias.isEmpty =>
        position(bounds.hi, plan.children.head, source).map(upper =>
          untpd
            .TypeBoundsTree(untpd.EmptyTree, upper)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (refined: untpd.RefinedTypeTree, PlanKind.Refined)
          if plan.children.size == 2 && refined.refinements.size == 1 =>
        for
          base <- position(refined.tpt, plan.children.head, source)
          member <- position(refined.refinements.head, plan.children(1), source)
        yield untpd
          .RefinedTypeTree(
            base,
            member.asInstanceOf[untpd.TypeDef] :: Nil
          )
          .cloneIn(source)
          .withSpan(plan.span)
      case (applied: untpd.AppliedTypeTree, PlanKind.Applied)
          if plan.children.size == applied.args.size + 1 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          arguments <- sequence(
            applied.args.zip(plan.children.tail).map(position(_, _, source))
          )
        yield untpd
          .AppliedTypeTree(constructor, arguments)
          .cloneIn(source)
          .withSpan(plan.span)
      case (alias: untpd.TypeDef, PlanKind.Alias)
          if plan.children.size == 1 =>
        position(alias.rhs, plan.children.head, source).map(rhs =>
          untpd
            .TypeDef(alias.name, rhs)
            .withMods(alias.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (identifier: untpd.Ident, PlanKind.Identifier)
          if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ =>
        Left(
          error(
            "GENERATED_ORIGIN_FAILED",
            s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
          )
        )

  private def sequence(
      values: List[Either[AuxTypeAliasUntypedLoweringError, untpd.Tree]]
  ): Either[AuxTypeAliasUntypedLoweringError, List[untpd.Tree]] =
    values.foldRight(
      Right(Nil): Either[AuxTypeAliasUntypedLoweringError, List[untpd.Tree]]
    )((value, result) =>
      for head <- value; tail <- result yield head :: tail
    )

  private def validatePositioned(
      tree: untpd.TypeDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 18 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 18"
    trees.foreach { current =>
      if !current.source.exists || current.source.path != source.path ||
          current.source.content.mkString != generated.source
      then errors += s"${current.getClass.getSimpleName} has divergent source provenance"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point ||
          current.span.point > current.span.end ||
          current.span.end > generated.source.length
      then errors += s"${current.getClass.getSimpleName} has an invalid span"
      if current.symbol != NoSymbol then
        errors += s"${current.getClass.getSimpleName} gained a symbol before typing"
      if current.isInstanceOf[untpd.TypedSplice] then
        errors += "positioned tree contains TypedSplice"
      directChildren(current).foreach { child =>
        if child.span.start < current.span.start ||
            child.span.end > current.span.end
        then errors += s"${current.getClass.getSimpleName} does not contain a child span"
      }
    }
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("GENERATED_ORIGIN_FAILED", result.mkString("; "))
    )

  private def validateTreeAgainstPlan(
      tree: untpd.Tree,
      plan: TreePlan,
      source: SourceFile,
      generatedSource: String,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  )(using Context): Unit =
    if tree.source.path != source.path ||
        tree.source.content.mkString != generatedSource || tree.span != plan.span
    then errors += s"${plan.kind} tree does not match its exact source/span plan"
    val children = directChildren(tree)
    if children.size != plan.children.size then
      errors += s"${plan.kind} tree/plan child counts differ"
    children.zip(plan.children).foreach { case (child, childPlan) =>
      validateTreeAgainstPlan(child, childPlan, source, generatedSource, errors)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): AuxTypeAliasUntypedLoweringError =
    AuxTypeAliasUntypedLoweringError(code, detail)
