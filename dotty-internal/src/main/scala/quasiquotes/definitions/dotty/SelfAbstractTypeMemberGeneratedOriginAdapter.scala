package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[quasiquotes] final case class SelfAbstractTypeMemberGeneratedOriginError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Deterministic generated source and complete nine-node positioning for 046. */
private[quasiquotes] object SelfAbstractTypeMemberGeneratedOriginAdapter:
  private enum PlanKind:
    case OuterTypeDef
    case TypeBounds
    case SingletonType
    case Refinement
    case RefinementAlias
    case SelectedType
    case TypeIdentifier
    case TermIdentifier

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
      plan: SelfAbstractTypeMemberPlan,
      virtualSourceName: String
  )(using Context): Either[
    SelfAbstractTypeMemberGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(
        error("INTERNAL_INVARIANT_FAILED", "the validated plan was null.")
      )
      _ <- Option(virtualSourceName).toRight(
        error(
          "INVALID_VIRTUAL_SOURCE_NAME",
          "the virtual source name must be present."
        )
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
        .left
        .map(problem => error("INVALID_VIRTUAL_SOURCE_NAME", problem.message))
      generated <- render(present)
      raw <- SelfAbstractTypeMemberUntypedLowerer
        .lower(present)
        .left
        .map(problem => error("EXACT_RAW_LOWERING_FAILED", problem.message))
      source = SourceFile.virtual(virtualSourceName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.TypeDef => Right(value)
        case other =>
          Left(
            error(
              "INTERNAL_INVARIANT_FAILED",
              s"positioning returned ${other.getClass.getSimpleName}, not TypeDef."
            )
          )
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(
      plan: SelfAbstractTypeMemberPlan
  ): Either[SelfAbstractTypeMemberGeneratedOriginError, GeneratedPlan] =
    val builder = new StringBuilder("type ")
    val outerPoint = builder.length
    builder.append(plan.memberName)
    builder.append(" >: ")
    val lowerStart = builder.length
    val lowerIdentifier = appendIdentifier(
      builder,
      plan.selfAlias.source,
      PlanKind.TermIdentifier
    )
    builder.append(".type")
    val lower = TreePlan(
      PlanKind.SingletonType,
      lowerStart,
      builder.length,
      lowerStart,
      Vector(lowerIdentifier)
    )
    builder.append(" <: ")
    val base = appendIdentifier(
      builder,
      plan.upperBound.baseName,
      PlanKind.TypeIdentifier
    )
    val refinementStart = base.start
    builder.append(" { type ")
    val aliasPoint = builder.length
    val aliasStart = aliasPoint - "type ".length
    builder.append(plan.upperBound.aliasName)
    builder.append(" = ")
    val selectedStart = builder.length
    val selectedPrefix = appendIdentifier(
      builder,
      plan.upperBound.rhs.alias.source,
      PlanKind.TermIdentifier
    )
    builder.append('.')
    val selectedPoint = builder.length
    builder.append(plan.upperBound.rhs.memberName)
    val selected = TreePlan(
      PlanKind.SelectedType,
      selectedStart,
      builder.length,
      selectedPoint,
      Vector(selectedPrefix)
    )
    val alias = TreePlan(
      PlanKind.RefinementAlias,
      aliasStart,
      selected.end,
      aliasPoint,
      Vector(selected)
    )
    builder.append(" }")
    val refinement = TreePlan(
      PlanKind.Refinement,
      refinementStart,
      builder.length,
      refinementStart,
      Vector(base, alias)
    )
    val bounds = TreePlan(
      PlanKind.TypeBounds,
      lower.start,
      refinement.end,
      lower.point,
      Vector(lower, refinement)
    )
    val root = TreePlan(
      PlanKind.OuterTypeDef,
      0,
      builder.length,
      outerPoint,
      Vector(bounds)
    )
    val generated = GeneratedPlan(builder.toString, root)
    validatePlan(generated).map(_ => generated)

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
  ): Either[SelfAbstractTypeMemberGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validatePlanNode(generated.root, generated.source.length, errors)
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("GENERATED_ORIGIN_FAILED", result.mkString("; "))
    )

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
  )(using Context): Either[SelfAbstractTypeMemberGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.TypeDef, PlanKind.OuterTypeDef)
          if plan.children.size == 1 =>
        position(definition.rhs, plan.children.head, source).map { rhs =>
          untpd
            .TypeDef(definition.name, rhs)
            .withMods(definition.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (bounds: untpd.TypeBoundsTree, PlanKind.TypeBounds)
          if plan.children.size == 2 && bounds.alias.isEmpty =>
        for
          lower <- position(bounds.lo, plan.children.head, source)
          upper <- position(bounds.hi, plan.children(1), source)
        yield untpd
          .TypeBoundsTree(lower, upper)
          .cloneIn(source)
          .withSpan(plan.span)
      case (singleton: untpd.SingletonTypeTree, PlanKind.SingletonType)
          if plan.children.size == 1 =>
        position(singleton.ref, plan.children.head, source).map { reference =>
          untpd
            .SingletonTypeTree(reference)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (refinement: untpd.RefinedTypeTree, PlanKind.Refinement)
          if plan.children.size == 2 && refinement.refinements.size == 1 =>
        for
          base <- position(refinement.tpt, plan.children.head, source)
          member <- position(refinement.refinements.head, plan.children(1), source)
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
      case (selected: untpd.Select, PlanKind.SelectedType)
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
            "GENERATED_ORIGIN_FAILED",
            s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
          )
        )

  private def validatePositioned(
      tree: untpd.TypeDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[SelfAbstractTypeMemberGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 9 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 9"
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
      val children = directChildren(current)
      children.foreach { child =>
        if child.span.start < current.span.start || child.span.end > current.span.end then
          errors += s"${current.getClass.getSimpleName} does not contain a child span"
      }
      children.zip(children.drop(1)).foreach { case (left, right) =>
        if left.span.end > right.span.start then
          errors += s"${current.getClass.getSimpleName} child spans overlap"
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
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): SelfAbstractTypeMemberGeneratedOriginError =
    SelfAbstractTypeMemberGeneratedOriginError(code, detail)
