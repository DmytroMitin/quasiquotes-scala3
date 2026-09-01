package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

class AuxTypeAliasPlanTest extends munit.FunSuite:
  test("canonical alias retains distinct declarations and exact binder-reference roles") {
    val plan = validPlan(canonicalExpectation)

    assertEquals(plan.aliasDisplayName, "Aux")
    assertEquals(
      plan.typeParameters.map(_.binderId),
      Vector(BinderId(0), BinderId(1), BinderId(2))
    )
    assertEquals(
      plan.appliedBase.arguments,
      Vector(
        TypeParameterReference(BinderId(0), "N"),
        TypeParameterReference(BinderId(1), "M")
      )
    )
    assertEquals(
      plan.outputReference,
      TypeParameterReference(BinderId(2), "Out0")
    )
    assertEquals(plan.argumentBinderPositions, Vector(0, 1))
    assertEquals(plan.refinementMember.memberName, "Out")
  }

  test("fully renamed legal names preserve binder roles independently of display spelling") {
    val expected = AuxTypeAliasExpectation(
      aliasName = "Evidence",
      firstParameter = AuxTypeParameterExpectation("Left", "Domain"),
      secondParameter = AuxTypeParameterExpectation("Right", "Domain"),
      outputParameter = AuxTypeParameterExpectation("Result0", "Domain"),
      targetName = "Combine",
      refinementMemberName = "Result"
    )
    val plan = validPlan(expected)

    assertEquals(plan.aliasDisplayName, "Evidence")
    assertEquals(
      plan.typeParameters.map(_.displayName),
      Vector("Left", "Right", "Result0")
    )
    assertEquals(
      plan.typeParameters.map(_.upperBound),
      Vector.fill(3)(SourceName("Domain"))
    )
    assertEquals(plan.appliedBase.constructor, SourceName("Combine"))
    assertEquals(plan.refinementMember.memberName, "Result")
    assertEquals(
      plan.appliedBase.arguments.map {
        case TypeParameterReference(binderId, _) => binderId
        case other => fail(s"expected binder reference, found $other")
      },
      Vector(BinderId(0), BinderId(1))
    )
  }

  test("binder identity and display spelling mismatches fail as separate semantic edges") {
    val fixture = semanticFixture(canonicalExpectation)
    val swapped = fixture.applied.copy(arguments = fixture.applied.arguments.reverse)
    assertRejected(
      fixture.copy(rhs = Refinement(swapped, Vector(fixture.member))),
      canonicalExpectation,
      "TARGET_BINDER_REFERENCE_MISMATCH"
    )

    val detachedOutput = TypeParameterReference(BinderId(99), "Out0")
    assertRejected(
      fixture.copy(
        rhs = Refinement(
          fixture.applied,
          Vector(ScopedTypeAlias("Out", detachedOutput))
        )
      ),
      canonicalExpectation,
      "OUTPUT_BINDER_REFERENCE_MISMATCH"
    )

    val wrongDisplay = TypeParameterReference(BinderId(0), "Other")
    assertRejected(
      fixture.copy(
        rhs = Refinement(
          fixture.applied.copy(
            arguments = Vector(wrongDisplay, fixture.applied.arguments(1))
          ),
          Vector(fixture.member)
        )
      ),
      canonicalExpectation,
      "TYPE_PARAMETER_REFERENCE_DISPLAY_NAME_MISMATCH"
    )
  }

  test("expectations and observed source names are validated before plan construction") {
    val fixture = semanticFixture(canonicalExpectation)
    assertEquals(
      AuxTypeAliasPlan
        .create(
          fixture.aliasName,
          fixture.parameters,
          fixture.rhs,
          canonicalExpectation.copy(aliasName = "bad-name")
        )
        .left
        .toOption
        .map(_.code),
      Some("EXPECTED_ALIAS_NAME_INVALID")
    )
    assertRejected(
      fixture.copy(aliasName = "Other"),
      canonicalExpectation,
      "ALIAS_NAME_MISMATCH"
    )
    assertRejected(
      fixture.copy(
        parameters = fixture.parameters.updated(
          1,
          fixture.parameters(1).copy(upperBound = SourceName("Other"))
        )
      ),
      canonicalExpectation,
      "TYPE_PARAMETER_UPPER_BOUND_MISMATCH"
    )
    assertRejected(
      fixture.copy(
        rhs = Refinement(
          fixture.applied.copy(constructor = SourceName("Other")),
          Vector(fixture.member)
        )
      ),
      canonicalExpectation,
      "TARGET_NAME_MISMATCH"
    )
    assertRejected(
      fixture.copy(
        rhs = Refinement(
          fixture.applied,
          Vector(fixture.member.copy(memberName = "Other"))
        )
      ),
      canonicalExpectation,
      "REFINEMENT_MEMBER_NAME_MISMATCH"
    )
  }

  test("invalid declaration cardinality identity and semantic topology fail closed") {
    val fixture = semanticFixture(canonicalExpectation)
    val cases = List(
      fixture.copy(parameters = fixture.parameters.take(2)) ->
        "TYPE_PARAMETER_ARITY_UNSUPPORTED",
      fixture.copy(
        parameters = fixture.parameters.updated(
          2,
          fixture.parameters(2).copy(binderId = BinderId(1))
        )
      ) -> "TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT",
      fixture.copy(
        parameters = fixture.parameters.updated(
          2,
          fixture.parameters(2).copy(displayName = "N")
        )
      ) -> "TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT",
      fixture.copy(rhs = fixture.applied) -> "RHS_REFINEMENT_REQUIRED",
      fixture.copy(
        rhs = fixture.refinement.copy(
          members = fixture.refinement.members :+ ScopedTypeAlias(
            "Other",
            fixture.output
          )
        )
      ) -> "REFINEMENT_MEMBER_COUNT_UNSUPPORTED"
    )

    cases.foreach { case (candidate, expectedCode) =>
      assertRejected(candidate, canonicalExpectation, expectedCode)
    }
  }

  private val canonicalExpectation = AuxTypeAliasExpectation(
    aliasName = "Aux",
    firstParameter = AuxTypeParameterExpectation("N", "Nat"),
    secondParameter = AuxTypeParameterExpectation("M", "Nat"),
    outputParameter = AuxTypeParameterExpectation("Out0", "Nat"),
    targetName = "Add",
    refinementMemberName = "Out"
  )

  private final case class SemanticFixture(
      aliasName: String,
      parameters: Vector[ScopedTypeParameter],
      applied: Applied,
      member: ScopedTypeAlias,
      output: TypeParameterReference,
      refinement: Refinement,
      rhs: ScopedType
  )

  private def semanticFixture(
      expected: AuxTypeAliasExpectation
  ): SemanticFixture =
    val parameterExpectations = Vector(
      expected.firstParameter,
      expected.secondParameter,
      expected.outputParameter
    )
    val parameters = parameterExpectations.zipWithIndex.map {
      case (parameter, index) =>
        ScopedTypeParameter(
          BinderId(index),
          parameter.displayName,
          SourceName(parameter.upperBoundName)
        )
    }
    val applied = Applied(
      SourceName(expected.targetName),
      Vector(
        TypeParameterReference(parameters(0).binderId, parameters(0).displayName),
        TypeParameterReference(parameters(1).binderId, parameters(1).displayName)
      )
    )
    val output = TypeParameterReference(
      parameters(2).binderId,
      parameters(2).displayName
    )
    val member = ScopedTypeAlias(expected.refinementMemberName, output)
    SemanticFixture(
      expected.aliasName,
      parameters,
      applied,
      member,
      output,
      Refinement(applied, Vector(member)),
      Refinement(applied, Vector(member))
    )

  private def validPlan(expected: AuxTypeAliasExpectation): AuxTypeAliasPlan =
    val fixture = semanticFixture(expected)
    AuxTypeAliasPlan
      .create(fixture.aliasName, fixture.parameters, fixture.rhs, expected)
      .fold(problem => fail(problem.message), identity)

  private def assertRejected(
      fixture: SemanticFixture,
      expected: AuxTypeAliasExpectation,
      expectedCode: String
  ): Unit =
    assertEquals(
      AuxTypeAliasPlan
        .create(fixture.aliasName, fixture.parameters, fixture.rhs, expected)
        .left
        .toOption
        .map(_.code),
      Some(expectedCode)
    )
