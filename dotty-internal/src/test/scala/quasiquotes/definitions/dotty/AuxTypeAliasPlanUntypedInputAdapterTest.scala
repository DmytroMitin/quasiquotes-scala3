package quasiquotes.definitions.dotty

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.TypeParameterReference
import quasiquotes.neutral.ScalametaAuxTypeAliasProjection
import AuxTypeAliasUntypedLoweringInput.TypeInput.{
  BinderReference,
  SourceName as USourceName
}

import scala.meta.*
import scala.meta.dialects.Scala3

class AuxTypeAliasPlanUntypedInputAdapterTest extends munit.FunSuite:
  test("adapter copies all three binder identities and reference roles verbatim") {
    val expected = AuxTypeAliasExpectation(
      "Aux",
      AuxTypeParameterExpectation("N", "Nat"),
      AuxTypeParameterExpectation("M", "Nat"),
      AuxTypeParameterExpectation("Out0", "Nat"),
      "Add",
      "Out"
    )
    val plan = ScalametaAuxTypeAliasProjection
      .project(parseAlias("type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"), expected)
      .fold(problem => fail(problem.message), _.plan)
    val validated = AuxTypeAliasPlanUntypedInputAdapter
      .adapt(plan)
      .fold(problem => fail(problem.message), identity)

    assertEquals(
      validated.parameters.map(_.binderId),
      plan.typeParameters.map(_.binderId)
    )
    assertEquals(
      validated.parameters.map(_.displayName),
      plan.typeParameters.map(_.displayName)
    )
    assertEquals(validated.parameters.map(_.binderId).distinct.size, 3)
    assertEquals(
      validated.target.arguments,
      plan.appliedBase.arguments.map {
        case TypeParameterReference(binderId, displayName) =>
          BinderReference(binderId, displayName)
        case other => fail(s"expected binder reference, found $other")
      }
    )
    assertEquals(
      validated.refinement.rhs,
      BinderReference(
        plan.outputReference.binderId,
        plan.outputReference.displayName
      )
    )
  }

  test("adapter preserves renamed spellings without allocating or inferring identities") {
    val expected = AuxTypeAliasExpectation(
      "Evidence",
      AuxTypeParameterExpectation("Left", "Domain"),
      AuxTypeParameterExpectation("Right", "Domain"),
      AuxTypeParameterExpectation("Result0", "Domain"),
      "Combine",
      "Result"
    )
    val plan = ScalametaAuxTypeAliasProjection
      .project(
        parseAlias(
          "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
        ),
        expected
      )
      .fold(problem => fail(problem.message), _.plan)
    val validated = AuxTypeAliasPlanUntypedInputAdapter
      .adapt(plan)
      .fold(problem => fail(problem.message), identity)

    assertEquals(
      validated.parameters.map(parameter => parameter.binderId -> parameter.displayName),
      plan.typeParameters.map(parameter => parameter.binderId -> parameter.displayName)
    )
    assertEquals(
      validated.parameters.map(_.upperBound),
      Vector.fill(3)(Some(USourceName("Domain")))
    )
    assertEquals(
      validated.target.arguments,
      Vector(
        BinderReference(plan.typeParameters(0).binderId, "Left"),
        BinderReference(plan.typeParameters(1).binderId, "Right")
      )
    )
    assertEquals(
      validated.refinement.rhs,
      BinderReference(plan.typeParameters(2).binderId, "Result0")
    )
  }

  test("adapter fails closed for a missing plan") {
    val result = AuxTypeAliasPlanUntypedInputAdapter.adapt(null)
    assertEquals(result.left.toOption.map(_.code), Some("PLAN_INPUT_REQUIRED"))
  }

  private def parseAlias(source: String): Defn.Type =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Type]
