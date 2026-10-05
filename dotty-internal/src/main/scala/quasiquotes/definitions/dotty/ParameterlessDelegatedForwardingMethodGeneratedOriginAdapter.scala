package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan
import quasiquotes.definitions.ScopedType.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Deterministic generated source and complete positioning for AUXify-083. */
private[quasiquotes] object ParameterlessDelegatedForwardingMethodGeneratedOriginAdapter:
  private enum PlanKind:
    case Definition
    case TypeParameter
    case TypeBounds
    case ContextualParameter
    case AppliedType
    case TypeIdentifier
    case ResultType
    case Selection
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
      plan: Plan,
      virtualSourceName: String
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(
        error("PLAN_REQUIRED", "the parameterless delegated-forwarding plan must be present.")
      )
      _ <- ParameterlessDelegatedForwardingPlan
        .validate(present)
        .left
        .map(problem => error("PLAN_INVALID", problem.message))
      virtualName <- Option(virtualSourceName).toRight(
        error("GENERATED_ORIGIN_INVALID", "the virtual source name must be present.")
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      generated <- render(present)
      raw <- ParameterlessDelegatedForwardingMethodUntypedLowerer
        .lower(present)
        .left
        .map(problem => error(problem.code, problem.detail))
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.DefDef => Right(value)
        case other =>
          Left(
            error(
              "GENERATED_ORIGIN_MISMATCH",
              s"positioning returned ${other.getClass.getSimpleName}, not DefDef."
            )
          )
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(
      plan: Plan
  ): Either[ParameterlessDelegatedForwardingMethodGeneratedOriginError, GeneratedPlan] =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference))
          if reference.eq(plan.evidenceTypeArgument) =>
        val builder = new StringBuilder("def ")
        val methodPoint = append(builder, plan.methodIdentity.sourceName)._1
        builder.append('[')
        val typeName = append(builder, plan.typeParameter.displayName)
        val typeBounds = leaf(PlanKind.TypeBounds, typeName)
        val typeParameter = TreePlan(
          PlanKind.TypeParameter,
          typeName._1,
          typeName._2,
          typeName._1,
          Vector(typeBounds)
        )
        builder.append("](using ")
        val contextualStart = builder.length
        val contextualPoint = append(builder, plan.contextualParameter.displayName)._1
        builder.append(": ")
        val appliedStart = builder.length
        val constructorName = append(builder, constructor)
        val constructorNode = leaf(PlanKind.TypeIdentifier, constructorName)
        builder.append('[')
        val contextualTypeName = append(builder, plan.typeParameter.displayName)
        val contextualTypeArgument = leaf(PlanKind.TypeIdentifier, contextualTypeName)
        builder.append(']')
        val applied = TreePlan(
          PlanKind.AppliedType,
          appliedStart,
          builder.length,
          constructorName._1,
          Vector(constructorNode, contextualTypeArgument)
        )
        val contextual = TreePlan(
          PlanKind.ContextualParameter,
          contextualStart,
          builder.length,
          contextualPoint,
          Vector(applied)
        )
        builder.append("): ")
        val resultName = append(builder, plan.typeParameter.displayName)
        val result = leaf(PlanKind.ResultType, resultName)
        builder.append(" = ")
        val selectionStart = builder.length
        val receiverName = append(builder, plan.contextualParameter.displayName)
        val receiver = leaf(PlanKind.TermIdentifier, receiverName)
        builder.append('.')
        val selectedPoint = append(builder, plan.methodIdentity.sourceName)._1
        val selection = TreePlan(
          PlanKind.Selection,
          selectionStart,
          builder.length,
          selectedPoint,
          Vector(receiver)
        )
        val root = TreePlan(
          PlanKind.Definition,
          0,
          builder.length,
          methodPoint,
          Vector(typeParameter, contextual, result, selection)
        )
        val generated = GeneratedPlan(builder.toString, root)
        validatePlan(generated).map(_ => generated)
      case _ =>
        Left(
          error(
            "PLAN_INVALID",
            "the contextual Type lost its exact unary evidence edge."
          )
        )

  private def append(builder: StringBuilder, value: String): (Int, Int) =
    val start = builder.length
    builder.append(value)
    start -> builder.length

  private def leaf(kind: PlanKind, interval: (Int, Int)): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, Vector.empty)

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[ParameterlessDelegatedForwardingMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validatePlanNode(generated.root, generated.source.length, errors)
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("GENERATED_ORIGIN_INVALID", result.mkString("; "))
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
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodGeneratedOriginError,
    untpd.Tree
  ] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Definition)
          if definition.paramss.size == 2 &&
            definition.paramss.forall(_.size == 1) &&
            plan.children.size == 4 =>
        for
          typeParameter <- position(definition.paramss.head.head, plan.children.head, source)
          contextual <- position(definition.paramss(1).head, plan.children(1), source)
          result <- position(definition.tpt, plan.children(2), source)
          body <- position(definition.rhs, plan.children(3), source)
        yield untpd
          .DefDef(
            definition.name,
            List(
              typeParameter.asInstanceOf[untpd.TypeDef] :: Nil,
              contextual.asInstanceOf[untpd.ValDef] :: Nil
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
          if plan.children.isEmpty && bounds.lo.isEmpty && bounds.hi.isEmpty &&
            bounds.alias.isEmpty =>
        Right(
          untpd
            .TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (parameter: untpd.ValDef, PlanKind.ContextualParameter)
          if plan.children.size == 1 && parameter.rhs.isEmpty =>
        position(parameter.tpt, plan.children.head, source).map { tpt =>
          untpd
            .ValDef(parameter.name, tpt, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (applied: untpd.AppliedTypeTree, PlanKind.AppliedType)
          if applied.args.size == 1 && plan.children.size == 2 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          argument <- position(applied.args.head, plan.children(1), source)
        yield untpd
          .AppliedTypeTree(constructor, argument :: Nil)
          .cloneIn(source)
          .withSpan(plan.span)
      case (selection: untpd.Select, PlanKind.Selection)
          if plan.children.size == 1 =>
        position(selection.qualifier, plan.children.head, source).map { receiver =>
          untpd
            .Select(receiver, selection.name)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (
            identifier: untpd.Ident,
            PlanKind.TypeIdentifier | PlanKind.ResultType | PlanKind.TermIdentifier
          ) if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ =>
        Left(
          error(
            "GENERATED_ORIGIN_MISMATCH",
            s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
          )
        )

  private def validatePositioned(
      tree: untpd.DefDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[
    ParameterlessDelegatedForwardingMethodGeneratedOriginError,
    Unit
  ] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 10 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 10"
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
      error("GENERATED_ORIGIN_INVALID", result.mkString("; "))
    )

  private def validateTreeAgainstPlan(
      tree: untpd.Tree,
      plan: TreePlan,
      source: SourceFile,
      generatedSource: String,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  )(using Context): Unit =
    if tree.source.path != source.path || tree.source.content.mkString != generatedSource ||
        tree.span != plan.span
    then errors += s"${plan.kind} tree does not match its exact source/span plan"
    val children = directChildren(tree)
    if children.size != plan.children.size then
      errors += s"${plan.kind} tree/plan child counts differ"
    children.zip(plan.children).foreach { case (child, childPlan) =>
      validateTreeAgainstPlan(child, childPlan, source, generatedSource, errors)
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ParameterlessDelegatedForwardingMethodUntypedLowerer.allTrees(tree)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ParameterlessDelegatedForwardingMethodUntypedLowerer.directChildren(tree)

  private def error(
      code: String,
      detail: String
  ): ParameterlessDelegatedForwardingMethodGeneratedOriginError =
    ParameterlessDelegatedForwardingMethodGeneratedOriginError(code, detail)
