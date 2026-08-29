package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class Phase133Auxify037ScalametaProbeTest extends munit.FunSuite:
  private val CanonicalSource =
    """def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] {
      |  type Out = inst.Out
      |} = inst""".stripMargin

  test("Scalameta preserves the exact two-bound applied and one-refinement source shape") {
    val target = targetDefinition(
      constructorName = "Add",
      firstTypeParameterName = "N",
      secondTypeParameterName = "M",
      upperBoundName = "Nat",
      memberName = "Out",
      contextualParameterName = "inst"
    )

    assertEquals(target.syntax, CanonicalSource)
    val group = target.paramClauseGroups.head
    assertEquals(
      group.tparamClause.values.map(_.syntax),
      List("N <: Nat", "M <: Nat")
    )
    assertEquals(
      group.paramClauses.head.values.head.decltpe.map(_.syntax),
      Some("Add[N, M]")
    )
    assertEquals(
      target.decltpe.map(_.syntax),
      Some(
        """Add[N, M] {
          |  type Out = inst.Out
          |}""".stripMargin
      )
    )
    assert(target.decltpe.exists(_.isInstanceOf[Type.Refine]))
    assertEquals(target.body.syntax, "inst")
  }

  test("dynamic legal spellings preserve the same exact Scalameta categories") {
    val target = targetDefinition(
      constructorName = "Combine",
      firstTypeParameterName = "Left",
      secondTypeParameterName = "Right",
      upperBoundName = "Domain",
      memberName = "Result",
      contextualParameterName = "evidence"
    )

    assertEquals(
      target.syntax,
      """def apply[Left <: Domain, Right <: Domain](using evidence: Combine[Left, Right]): Combine[Left, Right] {
        |  type Result = evidence.Result
        |} = evidence""".stripMargin
    )
  }

  test("the current neutral projector rejects each not-yet-admitted dimension deterministically") {
    val twoUnbounded =
      q"def apply[N, M](using inst: Add[N, M]): Add[N, M] = inst"
        .asInstanceOf[Defn.Def]
    val oneBounded =
      q"def apply[N <: Nat](using inst: Add[N]): Add[N] = inst"
        .asInstanceOf[Defn.Def]
    val selectedResult =
      q"def apply[N](using inst: Add[N]): inst.Out = inst"
        .asInstanceOf[Defn.Def]
    val exactTarget = targetDefinition("Add", "N", "M", "Nat", "Out", "inst")

    assertEquals(projectedCode(twoUnbounded), "NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED")
    assertEquals(projectedCode(oneBounded), "NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED")
    assertEquals(projectedCode(selectedResult), "NEUTRAL_TYPE_UNSUPPORTED")
    assertEquals(projectedCode(exactTarget), "NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED")
  }

  private def targetDefinition(
      constructorName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      upperBoundName: String,
      memberName: String,
      contextualParameterName: String
  ): Defn.Def =
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
    q"def apply[..${List(first, second)}](using $contextualName: $applied): $refined = $contextualName"
      .asInstanceOf[Defn.Def]

  private def projectedCode(definition: Defn.Def): String =
    ScalametaContextualMethodProjection
      .project(definition)
      .left
      .toOption
      .map(_.code)
      .getOrElse(fail("the current projector unexpectedly admitted the bounded refinement probe"))
