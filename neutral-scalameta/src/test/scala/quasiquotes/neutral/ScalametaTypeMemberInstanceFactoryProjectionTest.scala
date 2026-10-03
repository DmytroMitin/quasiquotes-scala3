package quasiquotes.neutral

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypeMemberInstanceFactoryProjectionTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A, Out0]: HasOut[A] { type Out = Out0 } =
      |  new HasOut[A]:
      |    type Out = Out0""".stripMargin

  test("projects canonical and fully renamed parsed definitions with truthful spans"):
    val canonicalDefinition = parse(Canonical)
    val renamedDefinition = parse(
      """def make[Element, Result0]: Container[Element] { type Result = Result0 } =
        |  new Container[Element]:
        |    type Result = Result0""".stripMargin
    )
    val canonical = project(canonicalDefinition)
    val renamed = project(renamedDefinition)

    assertPlan(canonical.plan, "instance", "A", "Out0", "HasOut", "Out")
    assertPlan(renamed.plan, "make", "Element", "Result0", "Container", "Result")
    assertEquals(canonical.plan.roleSnapshot.topology, renamed.plan.roleSnapshot.topology)
    assertEquals(
      canonical.sourceSpan,
      Some(NeutralSourceSpan(canonicalDefinition.pos.start, canonicalDefinition.pos.end))
    )
    assertEquals(
      renamed.sourceSpan,
      Some(NeutralSourceSpan(renamedDefinition.pos.start, renamedDefinition.pos.end))
    )

  test("projects an exact direct fresh tree with no source span"):
    val fresh = direct("instance", "A", "Out0", "HasOut", "Out")
    val projected = project(fresh)

    assert(allTrees(fresh).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertPlan(projected.plan, "instance", "A", "Out0", "HasOut", "Out")

  test("admits compiler-proven safe same-spelling controls"):
    val memberFirst = project(
      parse(
        """def instance[A, Out0]: HasOut[A] { type A = Out0 } =
          |  new HasOut[A]:
          |    type A = Out0""".stripMargin
      )
    )
    val factoryTypesAndMember = project(
      parse(
        """def same[same, Other]: Target[same] { type same = Other } =
          |  new Target[same]:
          |    type same = Other""".stripMargin
      )
    )

    assertEquals(memberFirst.plan.resultAlias.memberName, "A")
    assertEquals(factoryTypesAndMember.plan.factoryDisplayName, "same")
    assertEquals(factoryTypesAndMember.plan.firstTypeParameter.displayName, "same")
    assertEquals(factoryTypesAndMember.plan.resultAlias.memberName, "same")

  test("rejects null outer modifiers and parameter-clause topology near misses"):
    assertCode(
      ScalametaTypeMemberInstanceFactoryProjection.project(null),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_MISSING"
    )
    val rows = List(
      ("private " + Canonical) ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A, Out0]", "[A]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A, Out0]", "[A, Out0, Extra]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A, Out0]", "[+A, Out0]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A, Out0]", "[A <: Any, Out0]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A, Out0]", "[A[_], Out0]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      Canonical.replace("]: HasOut", "](value: A): HasOut") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_VALUE_CLAUSES_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val base = direct("instance", "A", "Out0", "HasOut", "Out")
    val group = base.paramClauseGroups.head
    val first = group.tparamClause.values.head
    val contextBound = first.copy(
      tbounds = Type.Bounds.empty,
      mods = List(Mod.Annot(Init(Type.Name("ann"), Name.Anonymous(), List.empty[Term.ArgClause])))
    )
    assertProjectedRejected(
      regroup(base, List(exactGroup(List(contextBound, group.tparamClause.values(1)), Nil))),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED"
    )

  test("rejects result refinement target and alias near misses"):
    val rows = List(
      Canonical.replace(": HasOut[A] { type Out = Out0 } =", "=") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_REFINEMENT_UNSUPPORTED",
      Canonical.replace("HasOut[A] { type Out = Out0 }", "HasOut[A]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_REFINEMENT_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "HasOut[A]", "HasOut") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "HasOut[A]", "HasOut[A, Out0]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "HasOut[A]", "HasOut[Out0]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH",
      Canonical.replace("{ type Out = Out0 }", "{ type Out }") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED",
      Canonical.replace("{ type Out = Out0 }", "{ type Out[X] = Out0 }") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED",
      Canonical.replace("{ type Out = Out0 }", "{ type Out = A }") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_RHS_MISMATCH",
      Canonical.replace("{ type Out = Out0 }", "{ type Out = other.Out0 }") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_RHS_MISMATCH",
      Canonical.replace("{ type Out = Out0 }", "{ type Out = Out0; type Extra = Out0 }") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val base = direct("instance", "A", "Out0", "HasOut", "Out")
    val Type.Refine(Some(target), List(alias: Defn.Type)) = base.decltpe.get: @unchecked
    assertProjectedRejected(
      base.copy(
        decltpe = Some(
          Type.Refine(
            Some(target),
            Stat.Block(List(alias.copy(mods = List(Mod.Protected(Name.Anonymous())))))
          )
        )
      ),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED"
    )

  test("rejects anonymous parent and template topology near misses"):
    val base = direct("instance", "A", "Out0", "HasOut", "Out")
    val template = anonymousTemplate(base)
    val target = template.inits.head.tpe

    assertProjectedRejected(
      base.copy(body = Term.Name("value")),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_REQUIRED"
    )
    List(
      template.copy(inits = Nil),
      template.copy(inits = template.inits ++ template.inits),
      template.copy(self = Self(Term.Name("self"), None)),
      template.copy(derives = List(Type.Name("Derived"))),
      template.copy(early = List(q"val early: Int = 1"))
    ).foreach { changed =>
      assertProjectedRejected(
        withTemplate(base, changed),
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
      )
    }
    assertProjectedRejected(
      withTemplate(
        base,
        template.copy(inits = List(Init(target, Name.Anonymous(), List(Term.ArgClause(List(q"1"))))))
      ),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(
        base,
        template.copy(
          inits = List(
            Init(
              Type.Apply(Type.Name("Other"), Type.ArgClause(List(Type.Name("A")))),
              Name.Anonymous(),
              List.empty[Term.ArgClause]
            )
          )
        )
      ),
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH"
    )

  test("rejects anonymous concrete-alias near misses"):
    val rows = List(
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Different = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_NAME_MISMATCH",
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Out = A") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_RHS_MISMATCH",
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Out = other.Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_RHS_MISMATCH",
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Out") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED",
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Out[X] = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED",
      replaceSecondLiteral(Canonical, "type Out = Out0", "protected type Out = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED",
      replaceSecondLiteral(Canonical, "type Out = Out0", "val Out = ???") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED",
      replaceSecondLiteral(Canonical, "type Out = Out0", "type Out = Out0\n    type Extra = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects parsed backticks and compiler-proven lexical collapses"):
    val nameRows = List(
      Canonical.replace("def instance", "def `def`") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      Canonical.replace("HasOut", "`type`") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      Canonical.replace("A, Out0", "`type`, Out0").replace("HasOut[A]", "HasOut[`type`]") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      Canonical.replace("Out0", "`val`") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      Canonical.replace("type Out = Out0", "type `object` = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    nameRows.foreach { case (source, code) => assertRejected(source, code) }

    val lexicalRows = List(
      Canonical.replace("HasOut", "A") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("HasOut", "Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("type Out = Out0", "type Out0 = Out0") ->
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
    )
    lexicalRows.foreach { case (source, code) => assertRejected(source, code) }

    val duplicate = direct("instance", "A", "A", "HasOut", "Out")
    assertProjectedRejected(
      duplicate,
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
    )

  private def assertPlan(
      plan: Plan,
      factory: String,
      first: String,
      second: String,
      target: String,
      member: String
  ): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.firstTypeParameter.displayName, first)
    assertEquals(plan.secondTypeParameter.displayName, second)
    assertEquals(plan.resultAlias.memberName, member)
    assertEquals(plan.anonymousAlias.memberName, member)
    plan.resultTarget match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, target)
        assertEquals(reference.binderId, plan.firstTypeParameter.binderId)
      case other => fail(s"unexpected target $other")

  private def project(definition: Defn.Def): ProjectedTypeMemberInstanceFactory =
    ScalametaTypeMemberInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertRejected(source: String, code: String): Unit =
    assertProjectedRejected(parse(source), code)

  private def assertProjectedRejected(definition: Defn.Def, code: String): Unit =
    assertCode(ScalametaTypeMemberInstanceFactoryProjection.project(definition), code)

  private def assertCode[A](result: Either[NeutralProjectionError, A], code: String): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, code, clues(problem.detail))
      case Right(_) => fail(s"expected $code")

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def anonymousTemplate(definition: Defn.Def): Template =
    definition.body.asInstanceOf[Term.NewAnonymous].templ

  private def withTemplate(definition: Defn.Def, template: Template): Defn.Def =
    definition.copy(body = Term.NewAnonymous(template))

  private def regroup(
      definition: Defn.Def,
      groups: List[Member.ParamClauseGroup]
  ): Defn.Def =
    Defn.Def(definition.mods, definition.name, groups, definition.decltpe, definition.body)

  private def exactGroup(
      parameters: List[Type.Param],
      clauses: List[Term.ParamClause]
  ): Member.ParamClauseGroup =
    Member.ParamClauseGroup(Type.ParamClause(parameters), clauses)

  private def replaceFirstLiteral(source: String, from: String, to: String): String =
    val index = source.indexOf(from)
    require(index >= 0, s"missing literal $from")
    source.substring(0, index) + to + source.substring(index + from.length)

  private def replaceSecondLiteral(source: String, from: String, to: String): String =
    val first = source.indexOf(from)
    val second = source.indexOf(from, first + from.length)
    require(second >= 0, s"missing second literal $from")
    source.substring(0, second) + to + source.substring(second + from.length)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)

  private def direct(
      factory: String,
      firstName: String,
      secondName: String,
      targetName: String,
      memberName: String
  ): Defn.Def =
    val first = typeParameter(firstName)
    val second = typeParameter(secondName)
    val target = Type.Apply(
      Type.Name(targetName),
      Type.ArgClause(List(Type.Name(firstName)))
    )
    val resultAlias = concreteAlias(memberName, secondName)
    val bodyAlias = concreteAlias(memberName, secondName)
    Defn.Def(
      Nil,
      Term.Name(factory),
      List(exactGroup(List(first, second), Nil)),
      Some(Type.Refine(Some(target), Stat.Block(List(resultAlias)))),
      Term.NewAnonymous(
        Template(
          Nil,
          List(Init(target, Name.Anonymous(), List.empty[Term.ArgClause])),
          Self(Name.Anonymous(), None),
          List(bodyAlias),
          Nil
        )
      )
    )

  private def typeParameter(name: String): Type.Param =
    Type.Param(Nil, Type.Name(name), Type.ParamClause(Nil), Type.Bounds.empty)

  private def concreteAlias(member: String, rhs: String): Defn.Type =
    Defn.Type(
      Nil,
      Type.Name(member),
      Type.ParamClause(Nil),
      Type.Name(rhs),
      Type.Bounds.empty
    )
