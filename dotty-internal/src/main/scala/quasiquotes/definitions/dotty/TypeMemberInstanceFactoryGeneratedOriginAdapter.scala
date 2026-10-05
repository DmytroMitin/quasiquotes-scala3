package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Deterministic generated origin for the accepted N053 Type-member factory plan. */
private[quasiquotes] object TypeMemberInstanceFactoryGeneratedOriginAdapter:
  private enum PlanKind:
    case Definition
    case TypeParameter
    case TypeBounds
    case Refinement
    case AppliedType
    case TypeIdentifier
    case NewExpression
    case Template
    case Constructor
    case ConcreteAlias

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
    TypeMemberInstanceFactoryGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the accepted TypeMemberInstanceFactoryPlan must be present."
      ))
      virtualName <- Option(virtualSourceName).toRight(error(
        "GENERATED_ORIGIN_INVALID",
        "the virtual source name must be present."
      ))
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      raw <- TypeMemberInstanceFactoryPlanUntypedLowerer
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

  private def render(
      plan: Plan
  ): Either[TypeMemberInstanceFactoryGeneratedOriginError, GeneratedPlan] =
    val roles = plan.roleSnapshot
    val builder = new StringBuilder("def ")
    val factory = append(builder, roles.factorySourceName)
    builder.append('[')
    val first = typeParameter(builder, roles.firstTypeParameterSourceName)
    builder.append(", ")
    val second = typeParameter(builder, roles.secondTypeParameterSourceName)
    builder.append("]: ")
    val result = refinement(
      builder,
      roles.targetConstructorSourceName,
      roles.firstTypeParameterSourceName,
      roles.memberSourceName,
      roles.secondTypeParameterSourceName
    )
    builder.append(" = ")
    val newStart = builder.length
    builder.append("new ")
    val templateStart = builder.length
    val parent = appliedType(
      builder,
      roles.targetConstructorSourceName,
      roles.firstTypeParameterSourceName
    )
    val constructor = TreePlan(
      PlanKind.Constructor,
      templateStart,
      templateStart,
      templateStart,
      Vector.empty
    )
    builder.append(" { ")
    val bodyAlias = concreteAlias(
      builder,
      roles.memberSourceName,
      roles.secondTypeParameterSourceName
    )
    builder.append(" }")
    val template = TreePlan(
      PlanKind.Template,
      templateStart,
      builder.length,
      templateStart,
      Vector(constructor, parent, bodyAlias)
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
      Vector(first, second, result, fresh)
    )
    val generated = GeneratedPlan(builder.toString, root)
    validatePlan(generated).map(_ => generated)

  private def typeParameter(builder: StringBuilder, sourceName: String): TreePlan =
    val interval = append(builder, sourceName)
    val bounds = leaf(PlanKind.TypeBounds, interval)
    node(PlanKind.TypeParameter, interval, Vector(bounds))

  private def refinement(
      builder: StringBuilder,
      target: String,
      firstTypeParameter: String,
      member: String,
      secondTypeParameter: String
  ): TreePlan =
    val start = builder.length
    val base = appliedType(builder, target, firstTypeParameter)
    builder.append(" { ")
    val alias = concreteAlias(builder, member, secondTypeParameter)
    builder.append(" }")
    TreePlan(
      PlanKind.Refinement,
      start,
      builder.length,
      start,
      Vector(base, alias)
    )

  private def concreteAlias(
      builder: StringBuilder,
      member: String,
      rhs: String
  ): TreePlan =
    val start = builder.length
    builder.append("type ")
    val memberInterval = append(builder, member)
    builder.append(" = ")
    val rhsPlan = typeIdentifier(builder, rhs)
    TreePlan(
      PlanKind.ConcreteAlias,
      start,
      rhsPlan.end,
      memberInterval._1,
      Vector(rhsPlan)
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
  ): Either[TypeMemberInstanceFactoryGeneratedOriginError, Unit] =
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
  )(using Context): Either[TypeMemberInstanceFactoryGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.DefDef, PlanKind.Definition)
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
              first.asInstanceOf[untpd.TypeDef],
              second.asInstanceOf[untpd.TypeDef]
            )),
            resultType,
            body
          )
          .withMods(definition.mods)
          .cloneIn(source)
          .withSpan(plan.span)
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
        Right(
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (refinement: untpd.RefinedTypeTree, PlanKind.Refinement)
          if plan.children.size == 2 && refinement.refinements.size == 1 =>
        for
          base <- position(refinement.tpt, plan.children.head, source)
          alias <- position(refinement.refinements.head, plan.children(1), source)
        yield untpd
          .RefinedTypeTree(base, alias.asInstanceOf[untpd.TypeDef] :: Nil)
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
      case (constructor: untpd.DefDef, PlanKind.Constructor)
          if plan.children.isEmpty =>
        Right(constructor.cloneIn(source).withSpan(plan.span))
      case (alias: untpd.TypeDef, PlanKind.ConcreteAlias)
          if plan.children.size == 1 =>
        position(alias.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(alias.name, rhs)
            .withMods(alias.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (identifier: untpd.Ident, PlanKind.TypeIdentifier)
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
  )(using Context): Either[TypeMemberInstanceFactoryGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    validateTreeAgainstPlan(tree, generated.root, source, generated.source, errors)
    val trees = allTrees(tree)
    if trees.size != 19 then
      errors += s"positioned tree has ${trees.size} nonempty nodes instead of 19"
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
    TypeMemberInstanceFactoryPlanUntypedLowerer.allTrees(tree)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    TypeMemberInstanceFactoryPlanUntypedLowerer.directChildren(tree)

  private def error(
      code: String,
      detail: String
  ): TypeMemberInstanceFactoryGeneratedOriginError =
    TypeMemberInstanceFactoryGeneratedOriginError(code, detail)
