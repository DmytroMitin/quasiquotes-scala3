package quasiquotes.neutral

import quasiquotes.definitions.ExtensionModulePlan.*
import quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaExtensionModuleProjectionTest extends munit.FunSuite:
  private val Canonical =
    """object syntax:
      |  extension [A](a: A)
      |    def combine(a1: A)(using inst: Monoid[A]): A =
      |      inst.combine(a, a1)""".stripMargin

  test("projects canonical and fully renamed source fixtures with truthful spans"):
    val canonicalDefinition = parse(Canonical)
    val renamedDefinition = parse(
      """object operations:
        |  extension [Element](left: Element)
        |    def merge(right: Element)(using evidence: Combine[Element]): Element =
        |      evidence.merge(left, right)""".stripMargin
    )
    val canonical = project(canonicalDefinition)
    val renamed = project(renamedDefinition)

    assertPlan(canonical.plan, "syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    assertPlan(
      renamed.plan,
      "operations",
      "Element",
      "left",
      "merge",
      "right",
      "evidence",
      "Combine"
    )
    assertEquals(canonical.plan.roleSnapshot.topology, renamed.plan.roleSnapshot.topology)
    assertEquals(
      canonical.sourceSpan,
      Some(NeutralSourceSpan(canonicalDefinition.pos.start, canonicalDefinition.pos.end))
    )
    assertEquals(
      renamed.sourceSpan,
      Some(NeutralSourceSpan(renamedDefinition.pos.start, renamedDefinition.pos.end))
    )

  test("projects a fresh direct tree with no source span"):
    val fresh = direct("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val projected = project(fresh)

    assertEquals(fresh.pos, Position.None)
    assertEquals(projected.sourceSpan, None)
    assertPlan(projected.plan, "syntax", "A", "a", "combine", "a1", "inst", "Monoid")

  test("admits proven cross-namespace and method-Term same-spelling controls"):
    val crossNamespace = project(
      parse(
        """object A:
          |  extension [A](A: A)
          |    def A(a1: A)(using inst: Monoid[A]): A =
          |      inst.A(A, a1)""".stripMargin
      )
    )
    val methodTermCollision = project(
      parse(
        """object syntax:
          |  extension [A](a: A)
          |    def combine(combine: A)(using inst: Monoid[A]): A =
          |      inst.combine(a, combine)""".stripMargin
      )
    )

    assertEquals(crossNamespace.plan.moduleDisplayName, "A")
    assertEquals(crossNamespace.plan.typeParameter.displayName, "A")
    assertEquals(crossNamespace.plan.receiverParameter.displayName, "A")
    assertEquals(crossNamespace.plan.methodIdentity.sourceName, "A")
    assertEquals(methodTermCollision.plan.methodIdentity.sourceName, "combine")
    assertEquals(methodTermCollision.plan.ordinaryArgument.displayName, "combine")

  test("rejects null, object modifiers, template extras, and body cardinality"):
    assertCode(ScalametaExtensionModuleProjection.project(null), "NEUTRAL_EXTENSION_MODULE_MISSING")
    assertRejected("private " + Canonical, "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED")
    assertRejected(
      Canonical.replace("object syntax:", "object syntax extends Parent:"),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED"
    )
    assertRejected(
      Canonical + "\n  val extra = 1",
      "NEUTRAL_EXTENSION_MODULE_OBJECT_BODY_UNSUPPORTED"
    )

    val canonical = parse(Canonical)
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(stats = Nil)),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_BODY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(stats = canonical.templ.stats ++ canonical.templ.stats)),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_BODY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(self = Self(Term.Name("self"), None))),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(derives = List(Type.Name("Derived")))),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(early = List(q"val early: Int = 1"))),
      "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED"
    )
    assertProjectedRejected(
      canonical.copy(templ = canonical.templ.copy(stats = List(q"val extra: Int = 1"))),
      "NEUTRAL_EXTENSION_MODULE_EXTENSION_REQUIRED"
    )

  test("rejects extension type-parameter and receiver topology near misses"):
    val rows = List(
      replaceFirstLiteral(Canonical, "[A]", "[A, B]") ->
        "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[+A]") ->
        "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A <: Any]") ->
        "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
      replaceFirstLiteral(Canonical, "[A]", "[A[_]]") ->
        "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED",
      Canonical.replace("(a: A)", "(a: String)") ->
        "NEUTRAL_EXTENSION_MODULE_RECEIVER_TYPE_MISMATCH",
      Canonical.replace("(a: A)", "(a: A = ???)") ->
        "NEUTRAL_EXTENSION_MODULE_RECEIVER_UNSUPPORTED",
      Canonical.replace("(a: A)", "(using a: A)") ->
        "NEUTRAL_EXTENSION_MODULE_RECEIVER_UNSUPPORTED",
      Canonical.replace("(a: A)", "(a: A)(other: A)") ->
        "NEUTRAL_EXTENSION_MODULE_RECEIVER_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val canonical = parse(Canonical)
    val extension = canonical.templ.stats.head.asInstanceOf[Defn.ExtensionGroup]
    assertProjectedRejected(
      withExtension(canonical, Defn.ExtensionGroup(None, extension.body)),
      "NEUTRAL_EXTENSION_MODULE_EXTENSION_PARAMETERS_UNSUPPORTED"
    )

  test("rejects nested method modifiers, genericity, and clause topology near misses"):
    val rows = List(
      Canonical.replace("def combine", "private def combine") ->
        "NEUTRAL_EXTENSION_MODULE_METHOD_UNSUPPORTED",
      Canonical.replace("def combine", "def combine[B]") ->
        "NEUTRAL_EXTENSION_MODULE_METHOD_TYPE_PARAMETERS_UNSUPPORTED",
      Canonical.replace("(a1: A)(using inst: Monoid[A])", "(a1: A)") ->
        "NEUTRAL_EXTENSION_MODULE_METHOD_CLAUSES_UNSUPPORTED",
      Canonical.replace("(a1: A)(using inst: Monoid[A])", "(using inst: Monoid[A])(a1: A)") ->
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_CLAUSE_UNSUPPORTED",
      Canonical.replace("(using inst: Monoid[A])", "(inst: Monoid[A])") ->
        "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      Canonical.replace("(a1: A)", "(a1: A, a2: A)") ->
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_PARAMETER_UNSUPPORTED",
      Canonical.replace("(using inst: Monoid[A])", "(using inst: Monoid[A], other: Monoid[A])") ->
        "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      Canonical.replace("a1: A", "a1: A = ???") ->
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_PARAMETER_UNSUPPORTED",
      Canonical.replace("a1: A", "a1: A*") ->
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_PARAMETER_UNSUPPORTED",
      Canonical.replace("inst: Monoid[A]", "inst: Monoid[A] = ???") ->
        "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_PARAMETER_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    assertRejected(
      Canonical + "\n    def other(a1: A)(using inst: Monoid[A]): A = inst.combine(a, a1)",
      "NEUTRAL_EXTENSION_MODULE_METHOD_REQUIRED"
    )

  test("rejects ordinary, contextual, and result Type-role near misses"):
    val rows = List(
      Canonical.replace("a1: A", "a1: String") ->
        "NEUTRAL_EXTENSION_MODULE_ORDINARY_TYPE_MISMATCH",
      Canonical.replace("inst: Monoid[A]", "inst: Monoid") ->
        "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_UNSUPPORTED",
      Canonical.replace("Monoid[A]", "pkg.Monoid[A]") ->
        "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_UNSUPPORTED",
      Canonical.replace("Monoid[A]", "Monoid[A, A]") ->
        "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_UNSUPPORTED",
      Canonical.replace("Monoid[A]", "Monoid[String]") ->
        "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_ARGUMENT_MISMATCH",
      Canonical.replace("): A =\n", "): String =\n") ->
        "NEUTRAL_EXTENSION_MODULE_RESULT_TYPE_MISMATCH",
      Canonical.replace("Monoid[A]", "A[A]") ->
        "NEUTRAL_EXTENSION_MODULE_LEXICAL_ROLE_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects delegated-body topology and exact role mismatches"):
    val rows = List(
      Canonical.replace("inst.combine(a, a1)", "inst") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_APPLICATION_UNSUPPORTED",
      Canonical.replace("inst.combine(a, a1)", "combine(a, a1)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_SELECTION_UNSUPPORTED",
      Canonical.replace("inst.combine(a, a1)", "other.combine(a, a1)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_RECEIVER_MISMATCH",
      Canonical.replace("inst.combine(a, a1)", "inst.other(a, a1)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_SELECTED_METHOD_MISMATCH",
      Canonical.replace("inst.combine(a, a1)", "inst.combine(a)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENTS_UNSUPPORTED",
      Canonical.replace("inst.combine(a, a1)", "inst.combine(a, a1, a)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENTS_UNSUPPORTED",
      Canonical.replace("inst.combine(a, a1)", "inst.combine(a1, a)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("inst.combine(a, a1)", "inst.combine(a, a)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENT_ROLE_MISMATCH",
      Canonical.replace("inst.combine(a, a1)", "inst.combine(this.a, a1)") ->
        "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENTS_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects duplicate Term roles and parsed backticked source spellings"):
    assertRejected(
      Canonical.replace("a1: A", "a: A").replace("inst.combine(a, a1)", "inst.combine(a, a)"),
      "NEUTRAL_EXTENSION_MODULE_TERM_ROLE_COLLISION"
    )
    assertRejected(
      Canonical.replace("object syntax", "object `type`"),
      "NEUTRAL_EXTENSION_MODULE_NAME_UNSUPPORTED"
    )
    assertRejected(
      Canonical.replace("def combine", "def `def`").replace("inst.combine", "inst.`def`"),
      "NEUTRAL_EXTENSION_MODULE_NAME_UNSUPPORTED"
    )

  private def assertPlan(
      plan: Plan,
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      ordinary: String,
      contextual: String,
      evidenceConstructor: String
  ): Unit =
    assertEquals(plan.moduleDisplayName, module)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.receiverParameter.displayName, receiver)
    assertEquals(plan.receiverParameter.parameterType.displayName, typeParameter)
    assertEquals(plan.methodIdentity.sourceName, method)
    assertEquals(plan.ordinaryArgument.displayName, ordinary)
    assertEquals(plan.ordinaryArgument.parameterType.displayName, typeParameter)
    assertEquals(plan.contextualParameter.displayName, contextual)
    plan.contextualParameter.parameterType match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, evidenceConstructor)
        assertEquals(reference.binderId, plan.typeParameter.binderId)
      case other => fail(s"unexpected contextual Type $other")
    assertEquals(plan.resultType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.body.receiver.binderId, plan.contextualParameter.binderId)
    assert(plan.body.selectedMethodIdentity eq plan.methodIdentity)
    assertEquals(
      plan.body.arguments.map(_.binderId),
      Vector(plan.receiverParameter.binderId, plan.ordinaryArgument.binderId)
    )

  private def project(definition: Defn.Object): ProjectedExtensionModule =
    ScalametaExtensionModuleProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertRejected(source: String, code: String): Unit =
    assertProjectedRejected(parse(source), code)

  private def assertProjectedRejected(definition: Defn.Object, code: String): Unit =
    assertCode(ScalametaExtensionModuleProjection.project(definition), code)

  private def assertCode[A](result: Either[NeutralProjectionError, A], code: String): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, code)
      case Right(_) => fail(s"expected $code")

  private def withExtension(
      definition: Defn.Object,
      extension: Defn.ExtensionGroup
  ): Defn.Object =
    definition.copy(templ = definition.templ.copy(stats = List(extension)))

  private def replaceFirstLiteral(source: String, from: String, to: String): String =
    val index = source.indexOf(from)
    require(index >= 0, s"missing literal $from")
    source.substring(0, index) + to + source.substring(index + from.length)

  private def parse(source: String): Defn.Object =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Object => definition
      case other => fail(s"expected Defn.Object, found ${other.productPrefix}")

  private def direct(
      module: String,
      typeParameterName: String,
      receiverName: String,
      methodName: String,
      ordinaryName: String,
      contextualName: String,
      evidenceConstructorName: String
  ): Defn.Object =
    val typeParameter = Type.Param(
      Nil,
      Type.Name(typeParameterName),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val receiver = Term.Param(
      Nil,
      Term.Name(receiverName),
      Some(Type.Name(typeParameterName)),
      None
    )
    val ordinary = Term.Param(
      Nil,
      Term.Name(ordinaryName),
      Some(Type.Name(typeParameterName)),
      None
    )
    val contextual = Term.Param(
      List(Mod.Using()),
      Term.Name(contextualName),
      Some(
        Type.Apply(
          Type.Name(evidenceConstructorName),
          Type.ArgClause(List(Type.Name(typeParameterName)))
        )
      ),
      None
    )
    val method = Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(List(ordinary)),
            Term.ParamClause(List(contextual), Some(Mod.Using()))
          )
        )
      ),
      Some(Type.Name(typeParameterName)),
      Term.Apply(
        Term.Select(Term.Name(contextualName), Term.Name(methodName)),
        Term.ArgClause(List(Term.Name(receiverName), Term.Name(ordinaryName)))
      )
    )
    val extension = Defn.ExtensionGroup(
      Some(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(receiver)))
        )
      ),
      method
    )
    Defn.Object(
      Nil,
      Term.Name(module),
      Template(Nil, Nil, Self(Name.Anonymous(), None), List(extension), Nil)
    )
