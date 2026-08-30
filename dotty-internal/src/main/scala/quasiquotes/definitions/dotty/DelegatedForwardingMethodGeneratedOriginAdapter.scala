package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.DelegatedForwardingMethodPlan.Plan
import quasiquotes.definitions.ScopedType.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[quasiquotes] final case class DelegatedForwardingMethodGeneratedOriginError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Deterministic generated source and complete positioning for AUXify-043. */
private[quasiquotes] object DelegatedForwardingMethodGeneratedOriginAdapter:
  private enum PlanKind:
    case Definition
    case TypeParameter
    case TypeBounds
    case OrdinaryParameter
    case ContextualParameter
    case AppliedType
    case TypeIdentifier
    case TermIdentifier
    case ResultType
    case Application
    case Selection

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
    DelegatedForwardingMethodGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(
        error("INTERNAL_INVARIANT_FAILED", "the validated 043 plan was null.")
      )
      virtualName <- Option(virtualSourceName).toRight(
        error(
          "GENERATED_ORIGIN_INVALID",
          "the virtual source name must be present."
        )
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      generated <- render(present)
      raw <- DelegatedForwardingMethodUntypedLowerer
        .lower(present)
        .left
        .map(problem => error(problem.code, problem.detail))
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.DefDef => Right(value)
        case other =>
          Left(
            error(
              "INTERNAL_INVARIANT_FAILED",
              s"positioning returned ${other.getClass.getSimpleName}, not DefDef."
            )
          )
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(
      plan: Plan
  ): Either[DelegatedForwardingMethodGeneratedOriginError, GeneratedPlan] =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(constructor), Vector(_: TypeParameterReference)) =>
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
        builder.append("](")
        val ordinaryStart = builder.length
        val ordinaryPoint = append(builder, plan.ordinaryParameter.displayName)._1
        builder.append(": ")
        val ordinaryTypeName = append(builder, plan.typeParameter.displayName)
        val ordinaryType = leaf(PlanKind.TypeIdentifier, ordinaryTypeName)
        val ordinary = TreePlan(
          PlanKind.OrdinaryParameter,
          ordinaryStart,
          builder.length,
          ordinaryPoint,
          Vector(ordinaryType)
        )
        builder.append(")(using ")
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
        val resultName = append(builder, plan.resultType.value)
        val result = leaf(PlanKind.ResultType, resultName)
        builder.append(" = ")
        val applicationStart = builder.length
        val receiverName = append(builder, plan.contextualParameter.displayName)
        val receiver = leaf(PlanKind.TermIdentifier, receiverName)
        builder.append('.')
        val selectedPoint = append(builder, plan.methodIdentity.sourceName)._1
        val selection = TreePlan(
          PlanKind.Selection,
          applicationStart,
          builder.length,
          selectedPoint,
          Vector(receiver)
        )
        builder.append('(')
        val argumentName = append(builder, plan.ordinaryParameter.displayName)
        val argument = leaf(PlanKind.TermIdentifier, argumentName)
        builder.append(')')
        val application = TreePlan(
          PlanKind.Application,
          applicationStart,
          builder.length,
          applicationStart,
          Vector(selection, argument)
        )
        val root = TreePlan(
          PlanKind.Definition,
          0,
          builder.length,
          methodPoint,
          Vector(typeParameter, ordinary, contextual, result, application)
        )
        val generated = GeneratedPlan(builder.toString, root)
        validatePlan(generated).map(_ => generated)
      case _ =>
        Left(
          error(
            "INTERNAL_INVARIANT_FAILED",
            "the validated contextual Type lost its unary source-name shape."
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
  ): Either[DelegatedForwardingMethodGeneratedOriginError, Unit] =
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
  )(using Context): Either[DelegatedForwardingMethodGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Definition)
          if definition.paramss.size == 3 &&
            definition.paramss.forall(_.size == 1) &&
            plan.children.size == 5 =>
        for
          typeParameter <- position(
            definition.paramss.head.head,
            plan.children.head,
            source
          )
          ordinary <- position(
            definition.paramss(1).head,
            plan.children(1),
            source
          )
          contextual <- position(
            definition.paramss(2).head,
            plan.children(2),
            source
          )
          result <- position(definition.tpt, plan.children(3), source)
          body <- position(definition.rhs, plan.children(4), source)
        yield untpd
          .DefDef(
            definition.name,
            List(
              typeParameter.asInstanceOf[untpd.TypeDef] :: Nil,
              ordinary.asInstanceOf[untpd.ValDef] :: Nil,
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
      case (parameter: untpd.ValDef, PlanKind.OrdinaryParameter | PlanKind.ContextualParameter)
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
      case (application: untpd.Apply, PlanKind.Application)
          if application.args.size == 1 && plan.children.size == 2 =>
        for
          function <- position(application.fun, plan.children.head, source)
          argument <- position(application.args.head, plan.children(1), source)
        yield untpd
          .Apply(function, argument :: Nil)
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
      case (identifier: untpd.Ident, PlanKind.TypeIdentifier | PlanKind.TermIdentifier | PlanKind.ResultType)
          if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ =>
        Left(
          error(
            "GENERATED_ORIGIN_INVALID",
            s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
          )
        )

  private def validatePositioned(
      tree: untpd.DefDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[DelegatedForwardingMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 14 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 14"
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
        if child.span.start < current.span.start || child.span.end > current.span.end then
          errors += s"${current.getClass.getSimpleName} does not contain a child span"
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
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): DelegatedForwardingMethodGeneratedOriginError =
    DelegatedForwardingMethodGeneratedOriginError(code, detail)
