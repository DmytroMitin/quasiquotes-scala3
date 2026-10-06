package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Deterministic generated origin for the accepted N056 curried-method factory plan. */
private[quasiquotes] object CurriedMethodInstanceFactoryGeneratedOriginAdapter:
  private enum PlanKind:
    case Definition
    case TypeParameter
    case TypeBounds
    case Carrier
    case FunctionType
    case AppliedType
    case TypeIdentifier
    case TermIdentifier
    case NewExpression
    case Template
    case Constructor
    case MethodOverride
    case NestedParameter
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
    CurriedMethodInstanceFactoryGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the accepted CurriedMethodInstanceFactoryPlan must be present."
      ))
      virtualName <- Option(virtualSourceName).toRight(error(
        "GENERATED_ORIGIN_INVALID",
        "the virtual source name must be present."
      ))
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      raw <- CurriedMethodInstanceFactoryPlanUntypedLowerer
        .lower(present)
        .left
        .map(problem => error(problem.code, problem.detail))
      generated <- render(present)
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.DefDef => Right(value)
        case other => Left(error(
          "GENERATED_ORIGIN_MISMATCH",
          s"positioning returned ${other.getClass.getSimpleName}, not DefDef."
        ))
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(plan: Plan): Either[CurriedMethodInstanceFactoryGeneratedOriginError, GeneratedPlan] =
    val roles = plan.roleSnapshot
    val builder = new StringBuilder("def ")
    val factory = append(builder, roles.factorySourceName)
    builder.append('[')
    val typeName = append(builder, roles.typeParameterSourceName)
    val bounds = leaf(PlanKind.TypeBounds, typeName)
    val typeParameter = node(PlanKind.TypeParameter, typeName, Vector(bounds))
    builder.append("](")
    val carrierStart = builder.length
    val carrierName = append(builder, roles.carrierSourceName)
    builder.append(": ")
    val carrierType = nestedUnaryFunctionType(builder, roles.typeParameterSourceName)
    val carrier = TreePlan(
      PlanKind.Carrier,
      carrierStart,
      carrierType.end,
      carrierName._1,
      Vector(carrierType)
    )
    builder.append("): ")
    val resultType = appliedType(builder, roles.targetConstructorSourceName, roles.typeParameterSourceName)
    builder.append(" = ")
    val newStart = builder.length
    builder.append("new ")
    val templateStart = builder.length
    val parent = appliedType(builder, roles.targetConstructorSourceName, roles.typeParameterSourceName)
    val constructor = TreePlan(
      PlanKind.Constructor,
      templateStart,
      templateStart,
      templateStart,
      Vector.empty
    )
    builder.append(" { ")
    val memberStart = builder.length
    builder.append("override def ")
    val memberName = append(builder, roles.memberSourceName)
    builder.append('(')
    val firstParameter = nestedParameter(
      builder,
      roles.firstParameterSourceName,
      roles.typeParameterSourceName
    )
    builder.append(")(")
    val secondParameter = nestedParameter(
      builder,
      roles.secondParameterSourceName,
      roles.typeParameterSourceName
    )
    builder.append("): ")
    val memberType = typeIdentifier(builder, roles.typeParameterSourceName)
    builder.append(" = ")
    val memberBody = nestedApplication(
      builder,
      roles.carrierSourceName,
      roles.firstParameterSourceName,
      roles.secondParameterSourceName
    )
    val member = TreePlan(
      PlanKind.MethodOverride,
      memberStart,
      memberBody.end,
      memberName._1,
      Vector(firstParameter, secondParameter, memberType, memberBody)
    )
    builder.append(" }")
    val template = TreePlan(
      PlanKind.Template,
      templateStart,
      builder.length,
      templateStart,
      Vector(constructor, parent, member)
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
      Vector(typeParameter, carrier, resultType, fresh)
    )
    val generated = GeneratedPlan(builder.toString, root)
    validatePlan(generated).map(_ => generated)

  private def nestedUnaryFunctionType(builder: StringBuilder, typeName: String): TreePlan =
    val outerStart = builder.length
    val first = typeIdentifier(builder, typeName)
    builder.append(" => ")
    val innerStart = builder.length
    val second = typeIdentifier(builder, typeName)
    builder.append(" => ")
    val result = typeIdentifier(builder, typeName)
    val inner = TreePlan(
      PlanKind.FunctionType,
      innerStart,
      result.end,
      innerStart,
      Vector(second, result)
    )
    TreePlan(
      PlanKind.FunctionType,
      outerStart,
      result.end,
      outerStart,
      Vector(first, inner)
    )

  private def nestedParameter(
      builder: StringBuilder,
      name: String,
      typeName: String
  ): TreePlan =
    val start = builder.length
    val parameterName = append(builder, name)
    builder.append(": ")
    val parameterType = typeIdentifier(builder, typeName)
    TreePlan(
      PlanKind.NestedParameter,
      start,
      parameterType.end,
      parameterName._1,
      Vector(parameterType)
    )

  private def nestedApplication(
      builder: StringBuilder,
      carrierName: String,
      firstName: String,
      secondName: String
  ): TreePlan =
    val start = builder.length
    val callee = termIdentifier(builder, carrierName)
    builder.append('(')
    val first = termIdentifier(builder, firstName)
    builder.append(')')
    val inner = TreePlan(
      PlanKind.Application,
      start,
      builder.length,
      start,
      Vector(callee, first)
    )
    builder.append('(')
    val second = termIdentifier(builder, secondName)
    builder.append(')')
    TreePlan(
      PlanKind.Application,
      start,
      builder.length,
      start,
      Vector(inner, second)
    )

  private def appliedType(
      builder: StringBuilder,
      constructorName: String,
      argumentName: String
  ): TreePlan =
    val start = builder.length
    val constructor = typeIdentifier(builder, constructorName)
    builder.append('[')
    val argument = typeIdentifier(builder, argumentName)
    builder.append(']')
    TreePlan(
      PlanKind.AppliedType,
      start,
      builder.length,
      constructor.point,
      Vector(constructor, argument)
    )

  private def typeIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TypeIdentifier, append(builder, value))

  private def termIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TermIdentifier, append(builder, value))

  private def append(builder: StringBuilder, value: String): (Int, Int) =
    val start = builder.length
    builder.append(value)
    start -> builder.length

  private def leaf(kind: PlanKind, interval: (Int, Int)): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, Vector.empty)

  private def node(kind: PlanKind, interval: (Int, Int), children: Vector[TreePlan]): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, children)

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[CurriedMethodInstanceFactoryGeneratedOriginError, Unit] =
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
    Either.cond(result.isEmpty, (), error("GENERATED_ORIGIN_INVALID", result.mkString("; ")))

  private def position(
      raw: untpd.Tree,
      plan: TreePlan,
      source: SourceFile
  )(using Context): Either[CurriedMethodInstanceFactoryGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Definition)
          if definition.paramss.size == 2 && definition.paramss.head.size == 1 &&
            definition.paramss(1).size == 1 && plan.children.size == 4 =>
        for
          typeParameter <- position(definition.paramss.head.head, plan.children.head, source)
          carrier <- position(definition.paramss(1).head, plan.children(1), source)
          resultType <- position(definition.tpt, plan.children(2), source)
          body <- position(definition.rhs, plan.children(3), source)
        yield untpd
          .DefDef(
            definition.name,
            List(
              typeParameter.asInstanceOf[untpd.TypeDef] :: Nil,
              carrier.asInstanceOf[untpd.ValDef] :: Nil
            ),
            resultType,
            body
          )
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.TypeDef, PlanKind.TypeParameter) if plan.children.size == 1 =>
        position(parameter.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(parameter.name, rhs)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (bounds: untpd.TypeBoundsTree, PlanKind.TypeBounds)
          if plan.children.isEmpty && bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty =>
        Right(
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (carrier: untpd.ValDef, PlanKind.Carrier)
          if plan.children.size == 1 && carrier.rhs.isEmpty =>
        position(carrier.tpt, plan.children.head, source).map(tpt =>
          untpd.ValDef(carrier.name, tpt, untpd.EmptyTree)
            .withMods(carrier.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (function: untpd.Function, PlanKind.FunctionType)
          if function.args.size == 1 && plan.children.size == 2 =>
        for
          argument <- position(function.args.head, plan.children.head, source)
          body <- position(function.body, plan.children(1), source)
        yield untpd.Function(argument :: Nil, body).cloneIn(source).withSpan(plan.span)
      case (member: untpd.DefDef, PlanKind.MethodOverride)
          if member.paramss.size == 2 && member.paramss.forall(_.size == 1) &&
            plan.children.size == 4 =>
        for
          first <- position(member.paramss.head.head, plan.children.head, source)
          second <- position(member.paramss(1).head, plan.children(1), source)
          resultType <- position(member.tpt, plan.children(2), source)
          body <- position(member.rhs, plan.children(3), source)
        yield untpd
          .DefDef(
            member.name,
            List(
              first.asInstanceOf[untpd.ValDef] :: Nil,
              second.asInstanceOf[untpd.ValDef] :: Nil
            ),
            resultType,
            body
          )
          .withMods(member.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.ValDef, PlanKind.NestedParameter)
          if plan.children.size == 1 && parameter.rhs.isEmpty =>
        position(parameter.tpt, plan.children.head, source).map(tpt =>
          untpd.ValDef(parameter.name, tpt, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (applied: untpd.AppliedTypeTree, PlanKind.AppliedType)
          if applied.args.size == 1 && plan.children.size == 2 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          argument <- position(applied.args.head, plan.children(1), source)
        yield untpd.AppliedTypeTree(constructor, argument :: Nil)
          .cloneIn(source)
          .withSpan(plan.span)
      case (fresh: untpd.New, PlanKind.NewExpression) if plan.children.size == 1 =>
        position(fresh.tpt, plan.children.head, source).map(tpt =>
          untpd.New(tpt).cloneIn(source).withSpan(plan.span)
        )
      case (template: untpd.Template, PlanKind.Template)
          if template.parentsOrDerived.size == 1 && template.derived.isEmpty &&
            template.self.isEmpty && template.body.size == 1 && plan.children.size == 3 =>
        for
          constructor <- position(template.constr, plan.children.head, source)
          parent <- position(template.parentsOrDerived.head, plan.children(1), source)
          member <- position(template.body.head, plan.children(2), source)
        yield untpd.Template(
          constructor.asInstanceOf[untpd.DefDef],
          parent :: Nil,
          Nil,
          untpd.EmptyValDef,
          member :: Nil
        ).cloneIn(source).withSpan(plan.span)
      case (constructor: untpd.DefDef, PlanKind.Constructor) if plan.children.isEmpty =>
        Right(constructor.cloneIn(source).withSpan(plan.span))
      case (application: untpd.Apply, PlanKind.Application)
          if application.args.size == 1 && plan.children.size == 2 =>
        for
          function <- position(application.fun, plan.children.head, source)
          argument <- position(application.args.head, plan.children(1), source)
        yield untpd.Apply(function, argument :: Nil).cloneIn(source).withSpan(plan.span)
      case (identifier: untpd.Ident, PlanKind.TypeIdentifier | PlanKind.TermIdentifier)
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
  )(using Context): Either[CurriedMethodInstanceFactoryGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = CurriedMethodInstanceFactoryPlanUntypedLowerer.allTrees(tree)
    if trees.size != 29 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 29"
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
      CurriedMethodInstanceFactoryPlanUntypedLowerer.directChildren(current).foreach { child =>
        if child.span.start < current.span.start || child.span.end > current.span.end then
          errors += s"${current.getClass.getSimpleName} does not contain a child span"
      }
    }
    val result = errors.result()
    Either.cond(result.isEmpty, (), error("GENERATED_ORIGIN_INVALID", result.mkString("; ")))

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
    val children = CurriedMethodInstanceFactoryPlanUntypedLowerer.directChildren(tree)
    if children.size != plan.children.size then
      errors += s"${plan.kind} tree/plan child counts differ"
    children.zip(plan.children).foreach { case (child, childPlan) =>
      validateTreeAgainstPlan(child, childPlan, source, generatedSource, errors)
    }

  private def error(
      code: String,
      detail: String
  ): CurriedMethodInstanceFactoryGeneratedOriginError =
    CurriedMethodInstanceFactoryGeneratedOriginError(code, detail)
