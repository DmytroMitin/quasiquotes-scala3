package quasiquotes.neutral

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaValueInstanceFactoryProjectionTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](valueValue: A): HasValue[A] =
      |  new HasValue[A]:
      |    override val value: A = valueValue""".stripMargin

  test("projects canonical and fully renamed parsed definitions with truthful spans"):
    val canonicalDefinition = parse(Canonical)
    val renamedDefinition = parse(
      """def make[Element](elementCarrier: Element): Container[Element] =
        |  new Container[Element]:
        |    override val element: Element = elementCarrier""".stripMargin
    )
    val canonical = project(canonicalDefinition)
    val renamed = project(renamedDefinition)

    assertPlan(canonical.plan, "instance", "A", "valueValue", "HasValue", "value")
    assertPlan(renamed.plan, "make", "Element", "elementCarrier", "Container", "element")
    assertEquals(canonical.plan.roleSnapshot.topology, renamed.plan.roleSnapshot.topology)
    assertEquals(
      canonical.sourceSpan,
      Some(NeutralSourceSpan(canonicalDefinition.pos.start, canonicalDefinition.pos.end))
    )
    assertEquals(
      renamed.sourceSpan,
      Some(NeutralSourceSpan(renamedDefinition.pos.start, renamedDefinition.pos.end))
    )

  test("projects an exact fresh direct tree with no source span"):
    val fresh = direct("instance", "A", "valueValue", "HasValue", "value")
    val projected = project(fresh)

    assert(allTrees(fresh).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertPlan(projected.plan, "instance", "A", "valueValue", "HasValue", "value")

  test("admits safe cross-namespace same-spelling controls"):
    val factoryTypeCarrier = project(
      parse(
        """def same[same](same: same): Target[same] =
          |  new Target[same]:
          |    override val member: same = same""".stripMargin
      )
    )
    val factoryMember = project(
      parse(
        """def same[A](carrier: A): Target[A] =
          |  new Target[A]:
          |    override val same: A = carrier""".stripMargin
      )
    )

    assertEquals(factoryTypeCarrier.plan.typeParameter.displayName, "same")
    assertEquals(factoryTypeCarrier.plan.strictCarrier.displayName, "same")
    assertEquals(factoryMember.plan.valueOverride.memberDisplayName, "same")

  test("rejects null outer modifiers and parameter-clause topology near misses"):
    assertCode(
      ScalametaValueInstanceFactoryProjection.project(null),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_MISSING"
    )
    val rows = List(
      ("private " + Canonical) -> "NEUTRAL_VALUE_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A, B]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[+A]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A <: Any]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A[_]]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      Canonical.replace("(valueValue: A)", "()") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("(valueValue: A)", "(valueValue: A, other: A)") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("(valueValue: A)", "(using valueValue: A)") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("valueValue: A", "valueValue: A = ???") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("valueValue: A", "valueValue: => A") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("valueValue: A", "valueValue: A*") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("valueValue: A", "valueValue: String") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_TYPE_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val canonical = parse(Canonical)
    val carrier = canonical.paramss.head.head
    assertProjectedRejected(
      canonical.copy(paramss = List(List(carrier.copy(decltpe = None)))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_TYPE_MISMATCH"
    )

  test("rejects result target and anonymous-body near misses"):
    val rows = List(
      Canonical.replace(": HasValue[A] =", "=") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
      Canonical.replace("HasValue[A]", "HasValue") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
      Canonical.replace("HasValue[A]", "HasValue[A, A]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
      Canonical.replace("HasValue[A]", "HasValue[String]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH",
      Canonical.replace("new HasValue[A]:\n    override val value: A = valueValue", "valueValue") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_ANONYMOUS_REQUIRED",
      Canonical.replace("HasValue[A]", "A[A]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects anonymous template parent self derives and body cardinality near misses"):
    val canonical = parse(Canonical)
    val template = anonymousTemplate(canonical)
    val target = template.inits.head.tpe

    assertProjectedRejected(
      withTemplate(canonical, template.copy(inits = Nil)),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(
        canonical,
        template.copy(
          inits = template.inits :+ Init(
            Type.Name("Other"),
            Name.Anonymous(),
            List.empty[Term.ArgClause]
          )
        )
      ),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(canonical, template.copy(self = Self(Term.Name("self"), None))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(canonical, template.copy(derives = List(Type.Name("Derived")))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(canonical, template.copy(early = List(q"val early: Int = 1"))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(canonical, template.copy(stats = Nil)),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(canonical, template.copy(stats = template.stats ++ template.stats)),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(
        canonical,
        template.copy(inits = List(Init(target, Name.Anonymous(), List(Term.ArgClause(List(q"1"))))))
      ),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_UNSUPPORTED"
    )

  test("rejects parent and override-val topology near misses"):
    val rows = List(
      replaceSecondLiteral(Canonical, "HasValue[A]", "Other[A]") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH",
      Canonical.replace("override val value", "val value") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED",
      Canonical.replace("override val value", "override lazy val value") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED",
      Canonical.replace("override val value", "override var value") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED",
      Canonical.replace("override val value: A", "override val value, other: A") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED",
      Canonical.replace("override val value: A", "override val value") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_TYPE_MISMATCH",
      Canonical.replace("override val value: A", "override val value: String") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_TYPE_MISMATCH",
      Canonical.replace("= valueValue", "= other") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH",
      Canonical.replace("= valueValue", "= this.valueValue") ->
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects independently constructed remaining topology and name boundaries"):
    val base = direct("instance", "A", "valueValue", "HasValue", "value")
    val group = base.paramClauseGroups.head
    val typeParameter = group.tparamClause.values.head
    val clause = group.paramClauses.head
    val carrier = clause.values.head
    val template = anonymousTemplate(base)
    val member = template.stats.head.asInstanceOf[Defn.Val]

    def regroup(groups: List[Member.ParamClauseGroup]): Defn.Def =
      Defn.Def(base.mods, base.name, groups, base.decltpe, base.body)

    def exactGroup(
        parameter: Type.Param,
        clauses: List[Term.ParamClause]
    ): Member.ParamClauseGroup =
      Member.ParamClauseGroup(Type.ParamClause(List(parameter)), clauses)

    assertProjectedRejected(
      regroup(List(group, group)),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED"
    )
    assertProjectedRejected(
      regroup(List(exactGroup(typeParameter, List(clause, clause)))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED"
    )
    List(
      typeParameter.copy(
        tbounds = Type.Bounds(Some(Type.Name("Nothing")), None, Nil, Nil)
      ),
      typeParameter.copy(cbounds = List(Type.Name("Ordering"))),
      typeParameter.copy(vbounds = List(Type.Name("Ordered")))
    ).foreach { bounded =>
      assertProjectedRejected(
        regroup(List(exactGroup(bounded, List(clause)))),
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED"
      )
    }
    assertProjectedRejected(
      regroup(
        List(
          exactGroup(
            typeParameter,
            List(Term.ParamClause(List(carrier.copy(mods = List(Mod.Implicit())))))
          )
        )
      ),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(
        base,
        template.copy(
          inits = List(
            Init(Type.Name("HasValue"), Name.Anonymous(), List.empty[Term.ArgClause])
          )
        )
      ),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH"
    )
    assertProjectedRejected(
      withTemplate(base, template.copy(stats = List(q"def value: A = valueValue"))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED"
    )
    assertProjectedRejected(
      withTemplate(base, template.copy(stats = List(member.copy(pats = List(Pat.Wildcard()))))),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED"
    )

    val emptyNameRows = List(
      base.copy(name = Term.Name("not legal")),
      regroup(
        List(
          exactGroup(typeParameter.copy(name = Type.Name("not legal")), List(clause))
        )
      ),
      regroup(
        List(
          exactGroup(
            typeParameter,
            List(Term.ParamClause(List(carrier.copy(name = Name.Anonymous()))))
          )
        )
      ),
      base.copy(
        decltpe = Some(
          Type.Apply(Type.Name("not legal"), Type.ArgClause(List(Type.Name("A"))))
        )
      ),
      withTemplate(
        base,
        template.copy(
          stats = List(
            member.copy(pats = List(Pat.Var(Term.Name("not legal"))))
          )
        )
      )
    )
    emptyNameRows.foreach { definition =>
      assertProjectedRejected(definition, "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED")
    }

  test("rejects parsed backticked names and the proven carrier-member shadowing case"):
    assertRejected(
      Canonical.replace("def instance", "def `def`"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical.replace("HasValue", "`type`"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical.replace("A", "`type`"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical.replace("valueValue", "`val`"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical.replace("val value:", "val `object`:"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical
        .replace("valueValue: A", "value: A")
        .replace("= valueValue", "= value"),
      "NEUTRAL_VALUE_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
    )

  private def assertPlan(
      plan: Plan,
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.strictCarrier.displayName, carrier)
    assertEquals(plan.valueOverride.memberDisplayName, member)
    assertEquals(plan.strictCarrier.parameterType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.valueOverride.valueType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.valueOverride.body.binderId, plan.strictCarrier.binderId)
    plan.resultTarget match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, target)
        assertEquals(reference.binderId, plan.typeParameter.binderId)
      case other => fail(s"unexpected target $other")

  private def project(definition: Defn.Def): ProjectedValueInstanceFactory =
    ScalametaValueInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertRejected(source: String, code: String): Unit =
    assertProjectedRejected(parse(source), code)

  private def assertProjectedRejected(definition: Defn.Def, code: String): Unit =
    assertCode(ScalametaValueInstanceFactoryProjection.project(definition), code)

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
      typeParameterName: String,
      carrierName: String,
      targetName: String,
      memberName: String
  ): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name(typeParameterName),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val carrier = Term.Param(
      Nil,
      Term.Name(carrierName),
      Some(Type.Name(typeParameterName)),
      None
    )
    val target = Type.Apply(
      Type.Name(targetName),
      Type.ArgClause(List(Type.Name(typeParameterName)))
    )
    val member = Defn.Val(
      List(Mod.Override()),
      List(Pat.Var(Term.Name(memberName))),
      Some(Type.Name(typeParameterName)),
      Term.Name(carrierName)
    )
    Defn.Def(
      Nil,
      Term.Name(factory),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(carrier)))
        )
      ),
      Some(target),
      Term.NewAnonymous(
        Template(
          Nil,
          List(Init(target, Name.Anonymous(), List.empty[Term.ArgClause])),
          Self(Name.Anonymous(), None),
          List(member),
          Nil
        )
      )
    )
