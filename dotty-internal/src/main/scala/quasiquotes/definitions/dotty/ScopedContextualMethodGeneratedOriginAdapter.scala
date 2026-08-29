package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[quasiquotes] final case class ScopedContextualMethodGeneratedOriginError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Complete generated-origin positioning for the exact Phase-134 raw shape. */
private[quasiquotes] object ScopedContextualMethodGeneratedOriginAdapter:
  import ScopedType.*

  private enum PlanKind:
    case Method
    case TypeParameter
    case TypeBounds
    case ContextualParameter
    case AppliedType
    case TypeIdentifier
    case Refinement
    case RefinementAlias
    case DirectSelectedType
    case TermIdentifier

  private final case class TreePlan(
      kind: PlanKind,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[TreePlan]
  ):
    def span: Span = Span(start, end, point)

  private final case class GeneratedPlan(
      source: String,
      root: TreePlan
  )

  def lower(
      plan: ScopedContextualMethodPlan,
      virtualSourceName: String
  )(using Context): Either[
    ScopedContextualMethodGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      _ <- Option(plan).toRight(error("the scoped contextual-method plan was null."))
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
        .left
        .map(problem => error(problem.message))
      generated <- render(plan)
      raw <- ScopedContextualMethodUntypedLowerer
        .lower(plan)
        .left
        .map(problem => error(problem.message))
      source = SourceFile.virtual(virtualSourceName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case definition: untpd.DefDef => Right(definition)
        case other =>
          Left(error(s"positioning returned ${other.getClass.getSimpleName}, not DefDef."))
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(
      positioned,
      generated.source,
      source
    )

  private def render(
      plan: ScopedContextualMethodPlan
  ): Either[ScopedContextualMethodGeneratedOriginError, GeneratedPlan] =
    val builder = new StringBuilder("def ")
    val methodPoint = builder.length
    builder.append(plan.methodDisplayName)
    builder.append('[')

    val parameterPlans = plan.typeParameters.zipWithIndex.map {
      case (parameter, index) =>
        if index > 0 then builder.append(", ")
        val parameterStart = builder.length
        builder.append(parameter.displayName)
        builder.append(" <: ")
        val upper = parameter.upperBound match
          case SourceName(value) => value
          case _ => ""
        val upperPlan = appendIdentifier(builder, upper, PlanKind.TypeIdentifier)
        val boundsPlan = TreePlan(
          PlanKind.TypeBounds,
          upperPlan.start,
          upperPlan.end,
          upperPlan.point,
          Vector(upperPlan)
        )
        TreePlan(
          PlanKind.TypeParameter,
          parameterStart,
          upperPlan.end,
          parameterStart,
          Vector(boundsPlan)
        )
    }

    builder.append("](using ")
    val contextualStart = builder.length
    builder.append(plan.contextualDisplayName)
    builder.append(": ")
    val contextualTypePlan = renderApplied(plan.contextualType, builder)
    val contextualPlan = TreePlan(
      PlanKind.ContextualParameter,
      contextualStart,
      contextualTypePlan.end,
      contextualStart,
      Vector(contextualTypePlan)
    )

    builder.append("): ")
    val resultPlan = renderRefinement(plan, builder)
    builder.append(" = ")
    val bodyPlan = appendIdentifier(
      builder,
      plan.contextualDisplayName,
      PlanKind.TermIdentifier
    )
    val root = TreePlan(
      PlanKind.Method,
      0,
      builder.length,
      methodPoint,
      parameterPlans ++ Vector(contextualPlan, resultPlan, bodyPlan)
    )
    val generated = GeneratedPlan(builder.toString, root)
    validatePlan(generated).map(_ => generated)

  private def renderApplied(
      applied: Applied,
      builder: StringBuilder
  ): TreePlan =
    val start = builder.length
    val constructor = applied.constructor.asInstanceOf[SourceName]
    val constructorPlan = appendIdentifier(
      builder,
      constructor.value,
      PlanKind.TypeIdentifier
    )
    builder.append('[')
    val arguments = applied.arguments.zipWithIndex.map { case (argument, index) =>
      if index > 0 then builder.append(", ")
      val reference = argument.asInstanceOf[TypeParameterReference]
      appendIdentifier(builder, reference.displayName, PlanKind.TypeIdentifier)
    }
    builder.append(']')
    TreePlan(
      PlanKind.AppliedType,
      start,
      builder.length,
      constructorPlan.point,
      constructorPlan +: arguments
    )

  private def renderRefinement(
      plan: ScopedContextualMethodPlan,
      builder: StringBuilder
  ): TreePlan =
    val base = plan.resultType.base.asInstanceOf[Applied]
    val basePlan = renderApplied(base, builder)
    val start = basePlan.start
    builder.append(" { ")
    val memberStart = builder.length
    builder.append("type ")
    val memberPoint = builder.length
    builder.append(plan.refinementMember.memberName)
    builder.append(" = ")
    val selectedStart = builder.length
    val prefixPlan = appendIdentifier(
      builder,
      plan.contextualDisplayName,
      PlanKind.TermIdentifier
    )
    builder.append('.')
    val selectedPoint = builder.length
    builder.append(plan.selectedResult.memberExpectation)
    val selectedPlan = TreePlan(
      PlanKind.DirectSelectedType,
      selectedStart,
      builder.length,
      selectedPoint,
      Vector(prefixPlan)
    )
    val memberPlan = TreePlan(
      PlanKind.RefinementAlias,
      memberStart,
      selectedPlan.end,
      memberPoint,
      Vector(selectedPlan)
    )
    builder.append(" }")
    TreePlan(
      PlanKind.Refinement,
      start,
      builder.length,
      basePlan.point,
      Vector(basePlan, memberPlan)
    )

  private def appendIdentifier(
      builder: StringBuilder,
      source: String,
      kind: PlanKind
  ): TreePlan =
    val start = builder.length
    builder.append(source)
    TreePlan(kind, start, builder.length, start, Vector.empty)

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[ScopedContextualMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validatePlanNode(generated.root, generated.source.length, errors)
    val result = errors.result()
    Either.cond(result.isEmpty, (), error(result.mkString("; ")))

  private def validatePlanNode(
      plan: TreePlan,
      sourceLength: Int,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  ): Unit =
    if plan.start < 0 || plan.start > plan.point || plan.point > plan.end ||
        plan.end > sourceLength
    then errors += s"${plan.kind} has an invalid planned span"
    plan.children.foreach { child =>
      if child.start < plan.start || child.end > plan.end then
        errors += s"${plan.kind} does not contain ${child.kind}"
      validatePlanNode(child, sourceLength, errors)
    }
    plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
      if left.end > right.start then
        errors += s"${plan.kind} children overlap or are out of source order"
    }

  private def position(
      raw: untpd.Tree,
      plan: TreePlan,
      source: SourceFile
  )(using Context): Either[ScopedContextualMethodGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Method)
          if plan.children.size == 5 =>
        val rawTypeParameters = definition.leadingTypeParams
        val rawContextual = definition.trailingParamss match
          case List(List(value: untpd.ValDef)) => Some(value)
          case _ => None
        for
          _ <- Either.cond(
            rawTypeParameters.size == 2 && rawContextual.nonEmpty,
            (),
            error("raw method clauses do not match the generated-source plan.")
          )
          first <- position(rawTypeParameters(0), plan.children(0), source)
          second <- position(rawTypeParameters(1), plan.children(1), source)
          contextual <- position(rawContextual.get, plan.children(2), source)
          result <- position(definition.tpt, plan.children(3), source)
          body <- position(definition.rhs, plan.children(4), source)
        yield untpd
          .DefDef(
            definition.name,
            List(
              List(first.asInstanceOf[untpd.TypeDef], second.asInstanceOf[untpd.TypeDef]),
              List(contextual.asInstanceOf[untpd.ValDef])
            ),
            result,
            body
          )
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.TypeDef, PlanKind.TypeParameter)
          if plan.children.size == 1 =>
        position(parameter.rhs, plan.children.head, source).map { bounds =>
          untpd
            .TypeDef(parameter.name, bounds)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (bounds: untpd.TypeBoundsTree, PlanKind.TypeBounds)
          if plan.children.size == 1 && bounds.lo.isEmpty && bounds.alias.isEmpty =>
        position(bounds.hi, plan.children.head, source).map { high =>
          untpd
            .TypeBoundsTree(untpd.EmptyTree, high)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (parameter: untpd.ValDef, PlanKind.ContextualParameter)
          if plan.children.size == 1 && parameter.rhs.isEmpty =>
        position(parameter.tpt, plan.children.head, source).map { contextualType =>
          untpd
            .ValDef(parameter.name, contextualType, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (applied: untpd.AppliedTypeTree, PlanKind.AppliedType)
          if plan.children.size == applied.args.size + 1 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          arguments <- applied.args.zip(plan.children.tail).foldLeft(
            Right(List.empty): Either[
              ScopedContextualMethodGeneratedOriginError,
              List[untpd.Tree]
            ]
          ) { case (accumulated, (argument, argumentPlan)) =>
            for
              values <- accumulated
              positioned <- position(argument, argumentPlan, source)
            yield values :+ positioned
          }
        yield untpd
          .AppliedTypeTree(constructor, arguments)
          .cloneIn(source)
          .withSpan(plan.span)
      case (refinement: untpd.RefinedTypeTree, PlanKind.Refinement)
          if plan.children.size == 2 && refinement.refinements.size == 1 =>
        for
          base <- position(refinement.tpt, plan.children.head, source)
          member <- position(
            refinement.refinements.head,
            plan.children(1),
            source
          )
        yield untpd
          .RefinedTypeTree(base, member.asInstanceOf[untpd.TypeDef] :: Nil)
          .cloneIn(source)
          .withSpan(plan.span)
      case (member: untpd.TypeDef, PlanKind.RefinementAlias)
          if plan.children.size == 1 =>
        position(member.rhs, plan.children.head, source).map { rhs =>
          untpd
            .TypeDef(member.name, rhs)
            .withMods(member.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (selected: untpd.Select, PlanKind.DirectSelectedType)
          if plan.children.size == 1 =>
        position(selected.qualifier, plan.children.head, source).map { prefix =>
          untpd
            .Select(prefix, selected.name)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (identifier: untpd.Ident, PlanKind.TypeIdentifier | PlanKind.TermIdentifier)
          if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ =>
        Left(
          error(
            s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
          )
        )

  private def validatePositioned(
      tree: untpd.DefDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[ScopedContextualMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    if tree.source.path != source.path ||
        tree.source.content.mkString != generated.source
    then errors += "root virtual source identity or content diverged"
    validateTreeAgainstPlan(tree, generated.root, source, errors)
    allTrees(tree).foreach { current =>
      if !current.source.exists || current.source.path != source.path then
        errors += s"${current.getClass.getSimpleName} does not use the virtual source"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point ||
          current.span.point > current.span.end ||
          current.span.end > generated.source.length
      then errors += s"${current.getClass.getSimpleName} has an invalid span"
      if current.symbol != NoSymbol then
        errors += s"${current.getClass.getSimpleName} gained a symbol before typing"
      if current.isInstanceOf[untpd.TypedSplice] then
        errors += "generated-origin tree contains TypedSplice"
      directChildren(current).foreach { child =>
        if child.span.start < current.span.start || child.span.end > current.span.end then
          errors += s"${current.getClass.getSimpleName} does not contain a child span"
      }
      directChildren(current).zip(directChildren(current).drop(1)).foreach {
        case (left, right) =>
          if left.span.end > right.span.start then
            errors += s"${current.getClass.getSimpleName} child spans overlap"
      }
    }
    val result = errors.result()
    Either.cond(result.isEmpty, (), error(result.mkString("; ")))

  private def validateTreeAgainstPlan(
      tree: untpd.Tree,
      plan: TreePlan,
      source: SourceFile,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  )(using Context): Unit =
    if tree.source.path != source.path || tree.span != plan.span then
      errors += s"${plan.kind} tree does not match its exact source/span plan"
    val children = directChildren(tree)
    if children.size != plan.children.size then
      errors += s"${plan.kind} tree/plan child counts differ"
    children.zip(plan.children).foreach { case (child, childPlan) =>
      validateTreeAgainstPlan(child, childPlan, source, errors)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(
      detail: String
  ): ScopedContextualMethodGeneratedOriginError =
    ScopedContextualMethodGeneratedOriginError("GENERATED_ORIGIN_FAILED", detail)
