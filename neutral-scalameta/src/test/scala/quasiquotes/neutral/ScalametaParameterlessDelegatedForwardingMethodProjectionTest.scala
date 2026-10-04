package quasiquotes.neutral

import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan
import quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaParameterlessDelegatedForwardingMethodProjectionTest
    extends munit.FunSuite:
  private val Canonical = "def empty[A](using inst: Empty[A]): A = inst.empty"

  test("projects canonical and fully renamed parsed definitions with truthful spans"):
    val canonicalDefinition = parse(Canonical)
    val renamedDefinition =
      parse("def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain")
    val canonical = project(canonicalDefinition)
    val renamed = project(renamedDefinition)

    assertPlan(canonical.plan, "empty", "A", "inst", "Empty")
    assertPlan(renamed.plan, "obtain", "Element", "evidence", "Provider")
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
    val fresh = direct("empty", "A", "inst", "Empty")
    val projected = project(fresh)

    assert(allTrees(fresh).forall(_.pos == Position.None))
    assertEquals(projected.sourceSpan, None)
    assertPlan(projected.plan, "empty", "A", "inst", "Empty")

  test("admits all compiler-proven safe same-spelling cross-namespace controls"):
    val sources = List(
      "def same[A](using same: Evidence[A]): A = same.same",
      "def get[A](using A: Evidence[A]): A = A.get",
      "def A[A](using inst: Evidence[A]): A = inst.A",
      "def Same[A](using inst: Same[A]): A = inst.Same",
      "def get[A](using Evidence: Evidence[A]): A = Evidence.get"
    )

    sources.foreach(source => project(parse(source)))

  test("rejects outer, group, Type-parameter, and value-clause near misses"):
    assertCode(
      ScalametaParameterlessDelegatedForwardingMethodProjection.project(null),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_MISSING"
    )
    val rows = List(
      ("private " + Canonical) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_OUTER_TOPOLOGY_UNSUPPORTED",
      "def empty(using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "def empty[A, B](using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "def empty[+A](using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "def empty[A <: Any](using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "def empty[A[_]](using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "def empty[A]: A = ???" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED",
      "def empty[A]()(using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED",
      "def empty[A](value: A)(using inst: Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED",
      "def empty[A](using one: Empty[A])(using two: Empty[A]): A = one.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val fresh = direct("empty", "A", "inst", "Empty")
    assertProjectedRejected(
      regroup(fresh, fresh.paramClauseGroups ++ fresh.paramClauseGroups),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_PARAMETER_CLAUSE_GROUPS_UNSUPPORTED"
    )
    val group = fresh.paramClauseGroups.head
    val parameter = group.tparamClause.values.head
    val annotation = Mod.Annot(
      Init(Type.Name("ann"), Name.Anonymous(), List.empty[Term.ArgClause])
    )
    assertProjectedRejected(
      regroup(
        fresh,
        List(
          group.copy(
            tparamClause = Type.ParamClause(List(parameter.copy(mods = List(annotation))))
          )
        )
      ),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED"
    )

  test("rejects every contextual-clause and contextual-parameter boundary"):
    val noUsing = parse("def empty[A](inst: Empty[A]): A = inst.empty")
    assertProjectedRejected(
      noUsing,
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_CLAUSE_UNSUPPORTED"
    )

    val base = direct("empty", "A", "inst", "Empty")
    val group = base.paramClauseGroups.head
    val clause = group.paramClauses.head
    val parameter = clause.values.head
    List(
      clause.copy(mod = None) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      clause.copy(values = List(parameter.copy(mods = Nil))) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      clause.copy(values = Nil) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      clause.copy(values = List(parameter, parameter)) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      clause.copy(values = List(parameter.copy(default = Some(Term.Name("value"))))) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      clause.copy(values = List(parameter.copy(mods = List(Mod.Using(), Mod.Inline())))) ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED"
    ).foreach { case (changed, code) =>
      assertProjectedRejected(regroup(base, List(group.copy(paramClauses = List(changed)))), code)
    }

    val parameterOnly = parameter.copy(mods = List(Mod.Using()))
    assertProjectedRejected(
      regroup(
        base,
        List(group.copy(paramClauses = List(Term.ParamClause(List(parameterOnly), None))))
      ),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_CLAUSE_UNSUPPORTED"
    )

  test("rejects evidence-Type and result-Type near misses"):
    val rows = List(
      "def empty[A](using inst: pkg.Empty[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty[A, A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty[String]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_ARGUMENT_MISMATCH",
      "def empty[A](using inst: A[A]): A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_LEXICAL_ROLE_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]) = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]): pkg.A = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]): Box[A] = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]): String = inst.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_BINDER_MISMATCH"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

    val base = direct("empty", "A", "inst", "Empty")
    val group = base.paramClauseGroups.head
    val clause = group.paramClauses.head
    val parameter = clause.values.head
    assertProjectedRejected(
      regroup(
        base,
        List(
          group.copy(
            paramClauses = List(clause.copy(values = List(parameter.copy(decltpe = None))))
          )
        )
      ),
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED"
    )

  test("rejects body topology, receiver, and selected-member near misses"):
    val rows = List(
      "def empty[A](using inst: Empty[A]): A = inst.empty()" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTION_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]): A = empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTION_UNSUPPORTED",
      "def empty[A](using inst: Empty[A]): A = other.empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_RECEIVER_MISMATCH",
      "def empty[A](using inst: Empty[A]): A = inst.other" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTED_METHOD_MISMATCH",
      "def empty[A](using inst: Empty[A]): A = (inst: Empty[A]).empty" ->
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTION_UNSUPPORTED"
    )
    rows.foreach { case (source, code) => assertRejected(source, code) }

  test("rejects backticked token loss at every neutral source-name boundary"):
    val rows = List(
      "def `def`[A](using inst: Empty[A]): A = inst.`def`",
      "def empty[`type`](using inst: Empty[`type`]): `type` = inst.empty",
      "def empty[A](using `given`: Empty[A]): A = `given`.empty",
      "def empty[A](using inst: `class`[A]): A = inst.empty"
    )
    rows.foreach { source =>
      assertRejected(source, "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_NAME_UNSUPPORTED")
    }

  private def assertPlan(
      plan: Plan,
      method: String,
      typeParameter: String,
      contextualParameter: String,
      contextualConstructor: String
  ): Unit =
    assertEquals(plan.methodIdentity.sourceName, method)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.contextualParameter.displayName, contextualParameter)
    plan.contextualParameter.parameterType match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, contextualConstructor)
        assertEquals(reference.binderId, plan.typeParameter.binderId)
      case other => fail("unexpected evidence Type " + other)
    assertEquals(plan.resultType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.body.receiver.binderId, plan.contextualParameter.binderId)
    assert(plan.body.selectedMethodIdentity eq plan.methodIdentity)

  private def project(
      definition: Defn.Def
  ): ProjectedParameterlessDelegatedForwardingMethod =
    ScalametaParameterlessDelegatedForwardingMethodProjection
      .project(definition)
      .fold(problem => fail(problem.code + ": " + problem.detail), identity)

  private def assertRejected(source: String, code: String): Unit =
    assertProjectedRejected(parse(source), code)

  private def assertProjectedRejected(definition: Defn.Def, code: String): Unit =
    assertCode(ScalametaParameterlessDelegatedForwardingMethodProjection.project(definition), code)

  private def assertCode[A](result: Either[NeutralProjectionError, A], code: String): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, code, clues(problem.detail))
      case Right(_) => fail("expected " + code)

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail("expected Defn.Def, found " + other.productPrefix)

  private def regroup(
      definition: Defn.Def,
      groups: List[Member.ParamClauseGroup]
  ): Defn.Def =
    Defn.Def(definition.mods, definition.name, groups, definition.decltpe, definition.body)

  private def direct(
      method: String,
      typeParameterName: String,
      contextualParameterName: String,
      contextualConstructorName: String
  ): Defn.Def =
    val typeParameter =
      Type.Param(Nil, Type.Name(typeParameterName), Type.ParamClause(Nil), Type.Bounds.empty)
    val contextualParameter = Term.Param(
      List(Mod.Using()),
      Term.Name(contextualParameterName),
      Some(
        Type.Apply(
          Type.Name(contextualConstructorName),
          Type.ArgClause(List(Type.Name(typeParameterName)))
        )
      ),
      None
    )
    Defn.Def(
      Nil,
      Term.Name(method),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(contextualParameter), Some(Mod.Using())))
        )
      ),
      Some(Type.Name(typeParameterName)),
      Term.Select(Term.Name(contextualParameterName), Term.Name(method))
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
