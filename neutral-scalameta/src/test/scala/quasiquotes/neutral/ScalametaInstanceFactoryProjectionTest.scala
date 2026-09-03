package quasiquotes.neutral

import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaInstanceFactoryProjectionTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] {
      |  override def empty: A = emptyValue
      |  override def combine(a: A, a1: A): A = combineFunction(a, a1)
      |}""".stripMargin

  test("projects canonical and fully renamed factories to the same five-role graph"):
    val canonicalDefinition = parse(Canonical)
    val canonical = project(canonicalDefinition)
    val renamed = project(
      parse(
        """def make[Element](fallbackValue: => Element, selection: (Element, Element) => Element): Choice[Element] = new Choice[Element] {
          |  override def fallback: Element = fallbackValue
          |  override def select(left: Element, right: Element): Element = selection(left, right)
          |}""".stripMargin
      )
    )

    assertPlan(
      canonical.plan,
      "instance",
      "A",
      "emptyValue",
      "combineFunction",
      "Monoid",
      "empty",
      "combine",
      "a",
      "a1"
    )
    assertPlan(
      renamed.plan,
      "make",
      "Element",
      "fallbackValue",
      "selection",
      "Choice",
      "fallback",
      "select",
      "left",
      "right"
    )
    assertEquals(
      canonical.sourceSpan,
      Some(NeutralSourceSpan(canonicalDefinition.pos.start, canonicalDefinition.pos.end))
    )

  test("returns no source span for a semantically valid unpositioned definition"):
    val copied = parse(Canonical).copy()
    assertEquals(copied.pos, Position.None)
    assertEquals(project(copied).sourceSpan, None)

  test("rejects null outer and parameter topology near misses deterministically"):
    assertCode(ScalametaInstanceFactoryProjection.project(null), "DEFINITION_MISSING")
    val rows = List(
      "private " + Canonical -> "OUTER_DEFINITION_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "") -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A, B]") -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A <: Any]") -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[+A]") -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A[_]]") -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("emptyValue: => A, ", "") ->
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combineFunction: (A, A) => A", "other: A, combineFunction: (A, A) => A") ->
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("emptyValue: => A", "emptyValue: A") ->
        "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
      Canonical.replace("emptyValue: => A", "emptyValue: => String") ->
        "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
      Canonical.replace("emptyValue: => A", "emptyValue: => A = ???") ->
        "EMPTY_VALUE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("emptyValue: => A", "using emptyValue: => A") ->
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combineFunction: (A, A) => A", "combineFunction: A") ->
        "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
      Canonical.replace("(A, A) => A", "A => A") ->
        "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
      Canonical.replace("(A, A) => A", "(A, A, A) => A") ->
        "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
      Canonical.replace("(A, A) => A", "(A, String) => A") ->
        "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
      Canonical.replace("combineFunction: (A, A) => A", "combineFunction: (A, A) => A = ???") ->
        "COMBINE_FUNCTION_PARAMETER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combineFunction: (A, A) => A", "combineFunction: ((A, A) => A)*") ->
        "COMBINE_FUNCTION_PARAMETER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(": Monoid[A] =", ": Monoid =") -> "TARGET_TYPE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(": Monoid[A] =", ": pkg.Monoid[A] =") ->
        "TARGET_TYPE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(": Monoid[A] =", ": Monoid[A, A] =") ->
        "TARGET_TYPE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(": Monoid[A] =", ": Monoid[String] =") -> "TARGET_TYPE_ROLE_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects anonymous parent and ordered member topology near misses"):
    val rows = List(
      Canonical.substring(0, Canonical.indexOf(" = new")) + " = emptyValue" ->
        "ANONYMOUS_IMPLEMENTATION_REQUIRED",
      Canonical.replace("new Monoid[A] {", "new Choice[A] {") -> "PARENT_TARGET_ROLE_MISMATCH",
      Canonical.replace("new Monoid[A] {", "new Monoid[A]() {") ->
        "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("  override def empty: A = emptyValue\n", "") ->
        "OVERRIDE_MEMBER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(
        "  override def combine(a: A, a1: A): A = combineFunction(a, a1)\n",
        "  override def other: A = emptyValue\n  override def combine(a: A, a1: A): A = combineFunction(a, a1)\n"
      ) -> "OVERRIDE_MEMBER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def empty: A = emptyValue", "override val empty: A = emptyValue") ->
        "OVERRIDE_MEMBER_TOPOLOGY_UNSUPPORTED",
      Canonical.replace(
        "  override def empty: A = emptyValue\n  override def combine(a: A, a1: A): A = combineFunction(a, a1)",
        "  override def combine(a: A, a1: A): A = combineFunction(a, a1)\n  override def empty: A = emptyValue"
      ) -> "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def empty", "def empty") -> "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def empty", "final override def empty") ->
        "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def combine", "def combine") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def combine", "final override def combine") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val canonical = parse(Canonical)
    val anonymous = canonical.body.asInstanceOf[Term.NewAnonymous]
    val parent = anonymous.templ.inits.head
    assertProjectedRejected(
      canonical.copy(
        body = anonymous.copy(templ = anonymous.templ.copy(inits = Nil))
      ),
      "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(
        body = anonymous.copy(templ = anonymous.templ.copy(inits = List(parent, parent.copy())))
      ),
      "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(
        body = anonymous.copy(
          templ = anonymous.templ.copy(self = Self(Term.Name("self"), None))
        )
      ),
      "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(
        body = anonymous.copy(
          templ = anonymous.templ.copy(derives = List(Type.Name("Derived")))
        )
      ),
      "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(
        body = anonymous.copy(
          templ = anonymous.templ.copy(early = List(q"val early: Int = 1"))
        )
      ),
      "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED"
    )

  test("rejects empty override Type parameter and body role near misses"):
    val rows = List(
      Canonical.replace("override def empty: A", "override def empty[B]: A") ->
        "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def empty: A", "override def empty(x: A): A") ->
        "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("override def empty: A", "override def empty: String") ->
        "EMPTY_OVERRIDE_TYPE_ROLE_MISMATCH",
      Canonical.replace("override def empty: A = emptyValue", "override def empty: A = combineFunction") ->
        "EMPTY_BODY_ROLE_MISMATCH",
      Canonical.replace("override def empty: A = emptyValue", "override def empty: A = other") ->
        "EMPTY_BODY_ROLE_MISMATCH",
      Canonical.replace("override def empty: A = emptyValue", "override def empty: A = this.emptyValue") ->
        "EMPTY_BODY_ROLE_MISMATCH",
      Canonical.replace("override def empty: A = emptyValue", "override def empty: A = emptyValue()") ->
        "EMPTY_BODY_ROLE_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects combine override Type and direct body role near misses"):
    val rows = List(
      Canonical.replace("combine(a: A, a1: A)", "combine(a: A)") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combine(a: A, a1: A)", "combine(a: A)(a1: A)") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combine(a: A, a1: A)", "combine(a: A = ???, a1: A)") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combine(a: A, a1: A)", "combine(a: A, a1: A*)") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combine(a: A, a1: A)", "combine(using a: A, a1: A)") ->
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combine(a: A, a1: A)", "combine(a: String, a1: A)") ->
        "COMBINE_PARAMETER_TYPE_ROLE_MISMATCH",
      Canonical.replace("combine(a: A, a1: A): A", "combine(a: A, a1: A): String") ->
        "COMBINE_RESULT_TYPE_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction") ->
        "COMBINE_BODY_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combineFunction(a, a1)", "other(a, a1)") ->
        "COMBINE_CALLEE_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "this.combineFunction(a, a1)") ->
        "COMBINE_BODY_TOPOLOGY_UNSUPPORTED",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(a)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(a, a1, a)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(a1, a)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(a, a)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(emptyValue, a1)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("combineFunction(a, a1)", "combineFunction(obj.a, a1)") ->
        "COMBINE_ARGUMENT_ROLE_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("lexical shadowing rejects a same-spelling nested capture that name-only lookup accepts"):
    val collision = Canonical
      .replace("combine(a: A, a1: A)", "combine(combineFunction: A, a1: A)")
      .replace("combineFunction(a, a1)", "combineFunction(combineFunction, a1)")
    assertRejected(collision, "COMBINE_CALLEE_ROLE_MISMATCH")

  private def assertPlan(
      plan: Plan,
      factory: String,
      typeName: String,
      emptyCarrier: String,
      functionCarrier: String,
      target: String,
      emptyMember: String,
      combineMember: String,
      firstNested: String,
      secondNested: String
  ): Unit =
    val reference = TypeParameterReference(BinderId(0), typeName)
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.typeParameter, TypeParameter(BinderId(0), typeName))
    assertEquals(
      plan.emptyValue,
      ByNameCarrier(BinderId(1), emptyCarrier, ParameterMode.ByName, ValueType(reference))
    )
    assertEquals(
      plan.combineFunction,
      BinaryFunctionCarrier(
        BinderId(2),
        functionCarrier,
        ParameterMode.ByValue,
        BinaryFunctionType(reference, reference, reference)
      )
    )
    assertEquals(plan.targetType, Applied(SourceName(target), Vector(reference)))
    assertEquals(plan.emptyOverride, EmptyOverride(emptyMember, TermReference(BinderId(1))))
    assertEquals(
      plan.combineOverride,
      CombineOverride(
        combineMember,
        NestedParameter(BinderId(3), firstNested, reference),
        NestedParameter(BinderId(4), secondNested, reference),
        reference,
        CombineBody(
          TermReference(BinderId(2)),
          Vector(TermReference(BinderId(3)), TermReference(BinderId(4)))
        )
      )
    )

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}: $source")

  private def project(definition: Defn.Def): ProjectedInstanceFactory =
    ScalametaInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(problem.message), identity)

  private def assertRejected(source: String, code: String): Unit =
    assertProjectedRejected(parse(source), code)

  private def assertProjectedRejected(definition: Defn.Def, code: String): Unit =
    assertCode(ScalametaInstanceFactoryProjection.project(definition), code)

  private def assertCode[A](result: Either[NeutralProjectionError, A], code: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(code), clues(result))

  private def replaceFirstLiteral(source: String, target: String, replacement: String): String =
    val index = source.indexOf(target)
    if index < 0 then fail(s"missing replacement target: $target")
    source.substring(0, index) + replacement + source.substring(index + target.length)
