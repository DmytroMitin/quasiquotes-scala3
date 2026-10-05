package quasiquotes.neutral

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaCurriedMethodInstanceFactoryProjectionTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](combineFunction: A => A => A): Curried[A] =
      |  new Curried[A]:
      |    override def combine(a: A)(b: A): A =
      |      combineFunction(a)(b)""".stripMargin

  test("projects canonical and fully renamed parsed definitions with truthful spans"):
    val canonicalDefinition = parse(Canonical)
    val renamedDefinition = parse(
      """def make[Element](operation: Element => Element => Element): Combiner[Element] =
        |  new Combiner[Element]:
        |    override def merge(left: Element)(right: Element): Element =
        |      operation(left)(right)""".stripMargin
    )
    val canonical = project(canonicalDefinition)
    val renamed = project(renamedDefinition)
    assertPlan(canonical.plan, "instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    assertPlan(renamed.plan, "make", "Element", "operation", "Combiner", "merge", "left", "right")
    assertEquals(canonical.plan.roleSnapshot.topology, renamed.plan.roleSnapshot.topology)
    assertEquals(canonical.sourceSpan, Some(NeutralSourceSpan(canonicalDefinition.pos.start, canonicalDefinition.pos.end)))
    assertEquals(renamed.sourceSpan, Some(NeutralSourceSpan(renamedDefinition.pos.start, renamedDefinition.pos.end)))

  test("admits compiler-proven safe same-spelling controls"):
    val rows = List(
      Canonical.replace("def instance", "def combine"),
      Canonical.replace("combine(a: A)", "combine(combine: A)").replace("combineFunction(a)", "combineFunction(combine)"),
      Canonical.replace("(b: A): A", "(combine: A): A").replace("(b)", "(combine)"),
      """def same[same](same: same => same => same): Target[same] =
        |  new Target[same]:
        |    override def combine(a: same)(b: same): same =
        |      same(a)(b)""".stripMargin
    )
    rows.foreach(source => assert(ScalametaCurriedMethodInstanceFactoryProjection.project(parse(source)).isRight, clues(source)))

  test("rejects outer Type parameter and carrier topology near misses"):
    assertEquals(
      ScalametaCurriedMethodInstanceFactoryProjection.project(null).left.toOption.map(_.code),
      Some("NEUTRAL_CURRIED_METHOD_FACTORY_MISSING")
    )
    val rows = List(
      ("private " + Canonical) -> "NEUTRAL_CURRIED_METHOD_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED",
      replaceFirst(Canonical, "[A]", "[A, B]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirst(Canonical, "[A]", "[+A]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirst(Canonical, "[A]", "[A <: Any]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirst(Canonical, "[A]", "[A[_]]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      Canonical.replace("(combineFunction: A => A => A)", "()") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("(combineFunction: A => A => A)", "(combineFunction: A => A => A, other: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("(combineFunction: A => A => A)", "(using combineFunction: A => A => A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("combineFunction: A => A => A", "combineFunction: A => A => A = ???") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("combineFunction: A => A => A", "combineFunction: => (A => A => A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("combineFunction: A => A => A", "combineFunction: (A => A => A)*") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED",
      Canonical.replace("combineFunction: A => A => A", "combineFunction: (A, A) => A") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_TYPE_MISMATCH",
      Canonical.replace("combineFunction: A => A => A", "combineFunction: A => String => A") -> "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_TYPE_MISMATCH"
    )
    rows.foreach((source, code) => assertRejected(source, code))

  test("rejects result anonymous template parent and member cardinality near misses"):
    val rows = List(
      Canonical.replace(": Curried[A] =", "=") -> "NEUTRAL_CURRIED_METHOD_FACTORY_RESULT_TARGET_UNSUPPORTED",
      Canonical.replace("Curried[A]", "Curried[String]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_RESULT_TARGET_MISMATCH",
      Canonical.replace("new Curried[A]:\n    override", "combineFunction\n    //") -> "NEUTRAL_CURRIED_METHOD_FACTORY_ANONYMOUS_REQUIRED",
      replaceSecond(Canonical, "Curried[A]", "Other[A]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_PARENT_TARGET_MISMATCH",
      Canonical.replace("override def combine", "def combine") -> "NEUTRAL_CURRIED_METHOD_FACTORY_OVERRIDE_UNSUPPORTED",
      Canonical.replace(
        "override def combine(a: A)(b: A): A =\n      combineFunction(a)(b)",
        "override type combine = A"
      ) -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_UNSUPPORTED"
    )
    rows.foreach((source, code) => assertRejected(source, code))
    val base = parse(Canonical)
    val template = anonymousTemplate(base)
    assertProjectedRejected(withTemplate(base, template.copy(stats = Nil)), "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_UNSUPPORTED")
    assertProjectedRejected(withTemplate(base, template.copy(stats = template.stats ++ template.stats)), "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_UNSUPPORTED")
    assertProjectedRejected(withTemplate(base, template.copy(self = Self(Term.Name("self"), None))), "NEUTRAL_CURRIED_METHOD_FACTORY_TEMPLATE_UNSUPPORTED")

  test("rejects wrong member clause cardinality and parameter mode"):
    val rows = List(
      Canonical.replace("(a: A)(b: A)", "") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)(b: A)", "(a: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)(b: A)", "(a: A)(b: A)(c: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)(b: A)", "(a: A, b: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)", "(using a: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(b: A)", "(implicit b: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)", "(using A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a: A)", "(erased a: A)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_PARAMETER_UNSUPPORTED",
      Canonical.replace("a: A", "a: A = ???") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_PARAMETER_UNSUPPORTED",
      Canonical.replace("a: A", "a: => A") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_PARAMETER_UNSUPPORTED",
      Canonical.replace("b: A", "b: A*") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_PARAMETER_UNSUPPORTED"
    )
    rows.foreach((source, code) => assertRejected(source, code))

    assertRejected(
      Canonical.replace("def combine(a: A)", "def combine[C](a: A)"),
      "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED"
    )

  test("rejects wrong member Types and every non-curried application body"):
    val rows = List(
      Canonical.replace("a: A", "a: String") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH",
      Canonical.replace("b: A", "b: String") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH",
      Canonical.replace(")(b: A): A =", ")(b: A): String =") -> "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH",
      Canonical.replace("combineFunction(a)(b)", "combineFunction(a, b)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_UNSUPPORTED",
      Canonical.replace("combineFunction(a)(b)", "combineFunction(b)(a)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a)(b)", "other(a)(b)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a)(b)", "combineFunction(other)(b)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a)(b)", "combineFunction(a)(other)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH"
    )
    rows.foreach((source, code) => assertRejected(source, code))

  test("rejects compiler-proven lexical collapses and non-plain names"):
    val rows = List(
      Canonical.replace("Curried[A]", "A[A]") -> "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("combineFunction", "combine") -> "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("combineFunction", "a") -> "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("combineFunction", "b") -> "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("(a: A)(b: A)", "(same: A)(same: A)").replace("(a)(b)", "(same)(same)") -> "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED",
      Canonical.replace("def instance", "def `def`") -> "NEUTRAL_CURRIED_METHOD_FACTORY_NAME_UNSUPPORTED"
    )
    rows.foreach((source, code) => assertRejected(source, code))

  private def assertPlan(plan: Plan, factory: String, tpe: String, carrier: String, target: String, member: String, first: String, second: String): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.typeParameter.displayName, tpe)
    assertEquals(plan.strictCarrier.displayName, carrier)
    assertEquals(plan.methodOverride.memberDisplayName, member)
    assertEquals(plan.firstParameter.displayName, first)
    assertEquals(plan.secondParameter.displayName, second)
    assertEquals(plan.resultTarget.toString.contains(target), true)

  private def project(definition: Defn.Def): ProjectedCurriedMethodInstanceFactory =
    ScalametaCurriedMethodInstanceFactoryProjection.project(definition).fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
  private def assertRejected(source: String, code: String): Unit = assertProjectedRejected(parse(source), code)
  private def assertProjectedRejected(definition: Defn.Def, code: String): Unit =
    assertEquals(ScalametaCurriedMethodInstanceFactoryProjection.project(definition).left.toOption.map(_.code), Some(code), clues(definition.structure))
  private def parse(source: String): Defn.Def = Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]
  private def anonymousTemplate(definition: Defn.Def): Template = definition.body.asInstanceOf[Term.NewAnonymous].templ
  private def withTemplate(definition: Defn.Def, template: Template): Defn.Def = definition.copy(body = Term.NewAnonymous(template))
  private def replaceSecond(source: String, needle: String, replacement: String): String =
    val first = source.indexOf(needle)
    val second = source.indexOf(needle, first + needle.length)
    source.substring(0, second) + replacement + source.substring(second + needle.length)
  private def replaceFirst(source: String, needle: String, replacement: String): String =
    val index = source.indexOf(needle)
    source.substring(0, index) + replacement + source.substring(index + needle.length)
