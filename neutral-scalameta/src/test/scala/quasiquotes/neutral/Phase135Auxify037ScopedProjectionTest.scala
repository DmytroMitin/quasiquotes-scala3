package quasiquotes.neutral

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class Phase135Auxify037ScopedProjectionTest extends munit.FunSuite:
  test("projects the exact bounded refinement into distinct ordered scoped binders") {
    val definition = targetDefinition(
      methodName = "apply",
      constructorName = "Add",
      firstTypeParameterName = "N",
      secondTypeParameterName = "M",
      upperBoundName = "Nat",
      memberName = "Out",
      contextualParameterName = "inst"
    )

    val projected = ScalametaScopedContextualMethodProjection
      .project(definition)
      .fold(error => fail(error.message), identity)
    val plan = projected.plan

    assertEquals(plan.methodDisplayName, "apply")
    assertEquals(plan.typeParameters.map(_.binderId), Vector(BinderId(0), BinderId(1)))
    assertEquals(plan.typeParameters.map(_.displayName), Vector("N", "M"))
    assertEquals(
      plan.typeParameters.map(_.upperBound),
      Vector(SourceName("Nat"), SourceName("Nat"))
    )
    assertEquals(plan.contextualTermBinderId, BinderId(2))
    assertEquals(plan.contextualDisplayName, "inst")
    assertEquals(plan.typeArgumentBinderPositions, Vector(0, 1))
    assertEquals(plan.contextualType.constructor, SourceName("Add"))
    assertEquals(plan.contextualType, plan.resultType.base)
    assertEquals(plan.refinementMember.memberName, "Out")
    assertEquals(plan.selectedResult, DirectStableSelected(BinderId(2), "Out"))
    assertEquals(plan.bodyTermBinderId, BinderId(2))
    assertEquals(
      projected.sourceSpan,
      Some(NeutralSourceSpan(definition.pos.start, definition.pos.end))
    )
  }

  test("rejects unsupported type-parameter and contextual-type shapes deterministically") {
    assertRejected(
      "def apply[N <: Nat](using inst: Add[N]): Add[N] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_CLAUSE_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat, K <: Nat](using inst: Add[N, M]): Add[N, M] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_CLAUSE_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N >: Nothing <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N, M <: Nat](using inst: Add[N, M]): Add[N, M] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M](using inst: Add[N, M]): Add[N, M] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Other](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_MISMATCH"
    )
    assertRejected(
      "def apply[@ann N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, N <: Nat](using inst: Add[N, N]): Add[N, N] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_PLAN_REJECTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[M, N]): Add[M, N] = inst",
      "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, X]): Add[N, X] = inst",
      "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH"
    )
    assertRejected(
      "def apply[F[_] <: Nat, M <: Nat](using inst: Add[F, M]): Add[F, M] = inst",
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_UNSUPPORTED"
    )
  }

  test("rejects unsupported refinement and body shapes deterministically") {
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Other[N, M] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_BASE_MISMATCH"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] {} = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_COUNT_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out; type Other = inst.Other } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_COUNT_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { val out: inst.Out } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = String } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_RHS_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = other.Out } = inst",
      "NEUTRAL_SCOPED037_SELECTED_PREFIX_UNBOUND"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Result } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_NAME_MISMATCH"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.inner.Out } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_RHS_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = other",
      "NEUTRAL_SCOPED037_BODY_BINDER_MISMATCH"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out; val extra: Int } = inst",
      "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_COUNT_UNSUPPORTED"
    )
  }

  test("rejects extra definition and parameter structure without legacy fallback") {
    assertRejected(
      "inline def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_DEFINITION_MODIFIERS_UNSUPPORTED"
    )
    assertRejected(
      "def apply[N <: Nat, M <: Nat](using inst: Add[N, M])(value: Int): Add[N, M] { type Out = inst.Out } = inst",
      "NEUTRAL_SCOPED037_CONTEXTUAL_CLAUSE_UNSUPPORTED"
    )

    val malformed = parseDefinition(
      "def apply[N <: Nat, M <: Nat](using inst: Add[M, N]): Add[M, N] { type Out = inst.Out } = inst"
    )
    val dispatchError = ScalametaContextualMethodDispatch
      .project(malformed)
      .left
      .toOption
      .getOrElse(fail("malformed 037 unexpectedly passed dispatch"))
    assertEquals(
      dispatchError.code,
      "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH"
    )
  }

  private def targetDefinition(
      methodName: String,
      constructorName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      upperBoundName: String,
      memberName: String,
      contextualParameterName: String
  ): Defn.Def =
    val method = Term.Name(methodName)
    val constructor = Type.Name(constructorName)
    val firstName = Type.Name(firstTypeParameterName)
    val secondName = Type.Name(secondTypeParameterName)
    val upperBound = Type.Name(upperBoundName)
    val selectedMember = Type.Name(memberName)
    val contextualName = Term.Name(contextualParameterName)
    val first: Type.Param = tparam"$firstName <: $upperBound"
    val second: Type.Param = tparam"$secondName <: $upperBound"
    val applied: Type = t"$constructor[..${List(firstName, secondName)}]"
    val selected: Type = t"$contextualName.$selectedMember"
    val refined: Type = t"$applied { type $selectedMember = $selected }"
    q"def $method[..${List(first, second)}](using $contextualName: $applied): $refined = $contextualName"
      .asInstanceOf[Defn.Def]

  private def parseDefinition(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def assertRejected(source: String, expectedCode: String): Unit =
    val definition = parseDefinition(source)
    val result = ScalametaScopedContextualMethodProjection.project(definition)
    assertEquals(
      result.left.toOption.map(_.code),
      Some(expectedCode),
      clues(result)
    )
    val typeParameterCount = definition.paramClauseGroups match
      case group :: Nil => group.tparamClause.values.size
      case _ => 0
    if typeParameterCount == 2 then
      val dispatched = ScalametaContextualMethodDispatch.project(definition)
      assertEquals(
        dispatched.left.toOption.map(_.code),
        Some(expectedCode),
        clues(dispatched)
      )
