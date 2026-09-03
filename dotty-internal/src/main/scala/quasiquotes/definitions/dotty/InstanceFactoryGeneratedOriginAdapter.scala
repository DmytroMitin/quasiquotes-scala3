package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.InstanceFactoryPlan.Plan
import quasiquotes.definitions.ScopedType.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Deterministic generated origin for the admitted N017 instance-factory plan. */
private[quasiquotes] object InstanceFactoryGeneratedOriginAdapter:
  private enum PlanKind:
    case Definition
    case TypeParameter
    case TypeBounds
    case EmptyCarrier
    case FunctionCarrier
    case NestedParameter
    case ByNameType
    case FunctionType
    case AppliedType
    case TypeIdentifier
    case TermIdentifier
    case NewExpression
    case Template
    case Constructor
    case EmptyOverride
    case CombineOverride
    case Application

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
    InstanceFactoryGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(
        error("PLAN_REQUIRED", "the accepted InstanceFactoryPlan must be present.")
      )
      virtualName <- Option(virtualSourceName).toRight(
        error("GENERATED_ORIGIN_INVALID", "the virtual source name must be present.")
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      generated <- render(present)
      raw <- InstanceFactoryPlanUntypedLowerer
        .lower(present)
        .left
        .map(problem => error(problem.code, problem.detail))
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.DefDef => Right(value)
        case other =>
          Left(error(
            "GENERATED_ORIGIN_MISMATCH",
            s"positioning returned ${other.getClass.getSimpleName}, not DefDef."
          ))
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(
      plan: Plan
  ): Either[InstanceFactoryGeneratedOriginError, GeneratedPlan] =
    plan.targetType match
      case Applied(SourceName(targetName), Vector(_: TypeParameterReference)) =>
        val builder = new StringBuilder("def ")
        val factory = append(builder, plan.factoryDisplayName)
        builder.append('[')
        val typeName = append(builder, plan.typeParameter.displayName)
        val bounds = leaf(PlanKind.TypeBounds, typeName)
        val typeParameter = node(PlanKind.TypeParameter, typeName, Vector(bounds))
        builder.append("](")

        val emptyStart = builder.length
        val emptyName = append(builder, plan.emptyValue.displayName)
        builder.append(": ")
        val byNameStart = builder.length
        builder.append("=> ")
        val byNameIdentifier = typedIdentifier(builder, plan.typeParameter.displayName)
        val byName = TreePlan(
          PlanKind.ByNameType,
          byNameStart,
          byNameIdentifier.end,
          byNameStart,
          Vector(byNameIdentifier)
        )
        val emptyCarrier = TreePlan(
          PlanKind.EmptyCarrier,
          emptyStart,
          byNameIdentifier.end,
          emptyName._1,
          Vector(byName)
        )

        builder.append(", ")
        val functionStart = builder.length
        val functionName = append(builder, plan.combineFunction.displayName)
        builder.append(": ")
        val functionTypeStart = builder.length
        builder.append('(')
        val firstFunctionArgument = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(", ")
        val secondFunctionArgument = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(") => ")
        val functionResult = typedIdentifier(builder, plan.typeParameter.displayName)
        val functionType = TreePlan(
          PlanKind.FunctionType,
          functionTypeStart,
          functionResult.end,
          functionTypeStart,
          Vector(firstFunctionArgument, secondFunctionArgument, functionResult)
        )
        val functionCarrier = TreePlan(
          PlanKind.FunctionCarrier,
          functionStart,
          functionResult.end,
          functionName._1,
          Vector(functionType)
        )

        builder.append("): ")
        val resultType = appliedType(builder, targetName, plan.typeParameter.displayName)
        builder.append(" = ")
        val newStart = builder.length
        builder.append("new ")
        val templateStart = builder.length
        val parent = appliedType(builder, targetName, plan.typeParameter.displayName)
        val constructor = TreePlan(
          PlanKind.Constructor,
          templateStart,
          templateStart,
          templateStart,
          Vector.empty
        )
        builder.append(" { ")

        val emptyOverrideStart = builder.length
        builder.append("override def ")
        val emptyMember = append(builder, plan.emptyOverride.memberDisplayName)
        builder.append(": ")
        val emptyResult = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(" = ")
        val emptyBody = termIdentifier(builder, plan.emptyValue.displayName)
        val emptyOverride = TreePlan(
          PlanKind.EmptyOverride,
          emptyOverrideStart,
          emptyBody.end,
          emptyMember._1,
          Vector(emptyResult, emptyBody)
        )

        builder.append("; ")
        val combineOverrideStart = builder.length
        builder.append("override def ")
        val combineMember = append(builder, plan.combineOverride.memberDisplayName)
        builder.append('(')
        val firstNested = nestedParameter(
          builder,
          plan.combineOverride.firstParameter.displayName,
          plan.typeParameter.displayName
        )
        builder.append(", ")
        val secondNested = nestedParameter(
          builder,
          plan.combineOverride.secondParameter.displayName,
          plan.typeParameter.displayName
        )
        builder.append("): ")
        val combineResult = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(" = ")
        val applicationStart = builder.length
        val callee = termIdentifier(builder, plan.combineFunction.displayName)
        builder.append('(')
        val firstArgument = termIdentifier(
          builder,
          plan.combineOverride.firstParameter.displayName
        )
        builder.append(", ")
        val secondArgument = termIdentifier(
          builder,
          plan.combineOverride.secondParameter.displayName
        )
        builder.append(')')
        val application = TreePlan(
          PlanKind.Application,
          applicationStart,
          builder.length,
          applicationStart,
          Vector(callee, firstArgument, secondArgument)
        )
        val combineOverride = TreePlan(
          PlanKind.CombineOverride,
          combineOverrideStart,
          builder.length,
          combineMember._1,
          Vector(firstNested, secondNested, combineResult, application)
        )
        builder.append(" }")

        val template = TreePlan(
          PlanKind.Template,
          templateStart,
          builder.length,
          templateStart,
          Vector(constructor, parent, emptyOverride, combineOverride)
        )
        val fresh = TreePlan(
          PlanKind.NewExpression,
          newStart,
          builder.length,
          newStart,
          Vector(template)
        )
        val root = TreePlan(
          PlanKind.Definition,
          0,
          builder.length,
          factory._1,
          Vector(typeParameter, emptyCarrier, functionCarrier, resultType, fresh)
        )
        val generated = GeneratedPlan(builder.toString, root)
        validatePlan(generated).map(_ => generated)
      case _ =>
        Left(error(
          "GENERATED_ORIGIN_INVALID",
          "the accepted target Type lost its direct unary source-name shape."
        ))

  private def nestedParameter(
      builder: StringBuilder,
      name: String,
      typeName: String
  ): TreePlan =
    val start = builder.length
    val declaration = append(builder, name)
    builder.append(": ")
    val tpt = typedIdentifier(builder, typeName)
    TreePlan(
      PlanKind.NestedParameter,
      start,
      tpt.end,
      declaration._1,
      Vector(tpt)
    )

  private def appliedType(
      builder: StringBuilder,
      constructorName: String,
      argumentName: String
  ): TreePlan =
    val start = builder.length
    val constructor = typedIdentifier(builder, constructorName)
    builder.append('[')
    val argument = typedIdentifier(builder, argumentName)
    builder.append(']')
    TreePlan(
      PlanKind.AppliedType,
      start,
      builder.length,
      start,
      Vector(constructor, argument)
    )

  private def typedIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TypeIdentifier, append(builder, value))

  private def termIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TermIdentifier, append(builder, value))

  private def append(builder: StringBuilder, value: String): (Int, Int) =
    val start = builder.length
    builder.append(value)
    start -> builder.length

  private def leaf(kind: PlanKind, interval: (Int, Int)): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, Vector.empty)

  private def node(
      kind: PlanKind,
      interval: (Int, Int),
      children: Vector[TreePlan]
  ): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, children)

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[InstanceFactoryGeneratedOriginError, Unit] =
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
      error("GENERATED_ORIGIN_INVALID", result.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: TreePlan,
      source: SourceFile
  )(using Context): Either[InstanceFactoryGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Definition)
          if definition.paramss.size == 2 &&
            definition.paramss.head.size == 1 &&
            definition.paramss(1).size == 2 && plan.children.size == 5 =>
        for
          typeParameter <- position(definition.paramss.head.head, plan.children.head, source)
          emptyCarrier <- position(definition.paramss(1).head, plan.children(1), source)
          functionCarrier <- position(definition.paramss(1)(1), plan.children(2), source)
          resultType <- position(definition.tpt, plan.children(3), source)
          body <- position(definition.rhs, plan.children(4), source)
        yield untpd
          .DefDef(
            definition.name,
            List(
              typeParameter.asInstanceOf[untpd.TypeDef] :: Nil,
              List(
                emptyCarrier.asInstanceOf[untpd.ValDef],
                functionCarrier.asInstanceOf[untpd.ValDef]
              )
            ),
            resultType,
            body
          )
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (definition: untpd.DefDef, PlanKind.EmptyOverride)
          if definition.paramss.isEmpty && plan.children.size == 2 =>
        for
          resultType <- position(definition.tpt, plan.children.head, source)
          body <- position(definition.rhs, plan.children(1), source)
        yield untpd
          .DefDef(definition.name, Nil, resultType, body)
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (definition: untpd.DefDef, PlanKind.CombineOverride)
          if definition.paramss.size == 1 && definition.paramss.head.size == 2 &&
            plan.children.size == 4 =>
        for
          first <- position(definition.paramss.head.head, plan.children.head, source)
          second <- position(definition.paramss.head(1), plan.children(1), source)
          resultType <- position(definition.tpt, plan.children(2), source)
          body <- position(definition.rhs, plan.children(3), source)
        yield untpd
          .DefDef(
            definition.name,
            List(List(
              first.asInstanceOf[untpd.ValDef],
              second.asInstanceOf[untpd.ValDef]
            )),
            resultType,
            body
          )
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (definition: untpd.DefDef, PlanKind.Constructor)
          if plan.children.isEmpty =>
        Right(definition.cloneIn(source).withSpan(plan.span))
      case (parameter: untpd.TypeDef, PlanKind.TypeParameter)
          if plan.children.size == 1 =>
        position(parameter.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(parameter.name, rhs)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (bounds: untpd.TypeBoundsTree, PlanKind.TypeBounds)
          if plan.children.isEmpty && bounds.lo.isEmpty && bounds.hi.isEmpty &&
            bounds.alias.isEmpty =>
        Right(untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
          .cloneIn(source)
          .withSpan(plan.span))
      case (parameter: untpd.ValDef,
            PlanKind.EmptyCarrier | PlanKind.FunctionCarrier | PlanKind.NestedParameter)
          if plan.children.size == 1 && parameter.rhs.isEmpty =>
        position(parameter.tpt, plan.children.head, source).map(tpt =>
          untpd.ValDef(parameter.name, tpt, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (byName: untpd.ByNameTypeTree, PlanKind.ByNameType)
          if plan.children.size == 1 =>
        position(byName.result, plan.children.head, source).map(result =>
          untpd.ByNameTypeTree(result).cloneIn(source).withSpan(plan.span)
        )
      case (function: untpd.Function, PlanKind.FunctionType)
          if function.args.size == 2 && plan.children.size == 3 =>
        for
          first <- position(function.args.head, plan.children.head, source)
          second <- position(function.args(1), plan.children(1), source)
          result <- position(function.body, plan.children(2), source)
        yield untpd.Function(List(first, second), result)
          .cloneIn(source)
          .withSpan(plan.span)
      case (applied: untpd.AppliedTypeTree, PlanKind.AppliedType)
          if applied.args.size == 1 && plan.children.size == 2 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          argument <- position(applied.args.head, plan.children(1), source)
        yield untpd.AppliedTypeTree(constructor, argument :: Nil)
          .cloneIn(source)
          .withSpan(plan.span)
      case (fresh: untpd.New, PlanKind.NewExpression)
          if plan.children.size == 1 =>
        position(fresh.tpt, plan.children.head, source).map(tpt =>
          untpd.New(tpt).cloneIn(source).withSpan(plan.span)
        )
      case (template: untpd.Template, PlanKind.Template)
          if template.parentsOrDerived.size == 1 && template.derived.isEmpty &&
            template.self.isEmpty && template.body.size == 2 && plan.children.size == 4 =>
        for
          constructor <- position(template.constr, plan.children.head, source)
          parent <- position(template.parentsOrDerived.head, plan.children(1), source)
          emptyOverride <- position(template.body.head, plan.children(2), source)
          combineOverride <- position(template.body(1), plan.children(3), source)
        yield untpd.Template(
          constructor.asInstanceOf[untpd.DefDef],
          parent :: Nil,
          Nil,
          untpd.EmptyValDef,
          List(emptyOverride, combineOverride)
        ).cloneIn(source).withSpan(plan.span)
      case (application: untpd.Apply, PlanKind.Application)
          if application.args.size == 2 && plan.children.size == 3 =>
        for
          function <- position(application.fun, plan.children.head, source)
          first <- position(application.args.head, plan.children(1), source)
          second <- position(application.args(1), plan.children(2), source)
        yield untpd.Apply(function, List(first, second))
          .cloneIn(source)
          .withSpan(plan.span)
      case (identifier: untpd.Ident,
            PlanKind.TypeIdentifier | PlanKind.TermIdentifier)
          if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ =>
        Left(error(
          "GENERATED_ORIGIN_MISMATCH",
          s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
        ))

  private def validatePositioned(
      tree: untpd.DefDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[InstanceFactoryGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 33 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 33"
    trees.foreach { current =>
      if !current.source.exists || current.source.path != source.path ||
          current.source.content.mkString != generated.source
      then errors += s"${current.getClass.getSimpleName} has divergent source provenance"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point || current.span.point > current.span.end ||
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
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val children = tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias)
      case value: untpd.ByNameTypeTree => Vector(value.result)
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty
    children.filterNot(_.isEmpty)

  private def error(
      code: String,
      detail: String
  ): InstanceFactoryGeneratedOriginError =
    InstanceFactoryGeneratedOriginError(code, detail)
