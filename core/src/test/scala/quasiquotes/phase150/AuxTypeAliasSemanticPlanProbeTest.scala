package quasiquotes.phase150

import quasiquotes.parser.BinderId
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.*
import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.TypeNode.*

class AuxTypeAliasSemanticPlanProbeTest extends munit.FunSuite:
  test("three declarations and all five references retain exact binder identity") {
    val plan = validPlan()

    assertEquals(plan.parameters.map(_.binderId), Vector(BinderId(1), BinderId(2), BinderId(3)))
    assertEquals(plan.appliedBase.arguments.map(_.binderId), Vector(BinderId(1), BinderId(2)))
    assertEquals(plan.outputReference.binderId, BinderId(3))
    assertEquals(plan.argumentBinderPositions, Vector(0, 1))
    assertEquals(plan.refinementMember.memberName, "Out")
  }

  test("fully dynamic legal names preserve the same exact binder roles") {
    val plan = validPlan(
      aliasName = "Evidence",
      firstName = "Left",
      secondName = "Right",
      outputName = "Result0",
      upperBoundName = "Domain",
      constructorName = "Combine",
      memberName = "Result"
    )

    assertEquals(plan.aliasName, "Evidence")
    assertEquals(plan.parameters.map(_.displayName), Vector("Left", "Right", "Result0"))
    assertEquals(plan.parameters.map(_.upperBound.value), Vector.fill(3)("Domain"))
    assertEquals(plan.appliedBase.constructor.value, "Combine")
    assertEquals(plan.refinementMember.memberName, "Result")
  }

  test("swapped target arguments and a detached output reference fail separately") {
    val fixture = nodes()
    val swapped = fixture.applied.copy(arguments = fixture.applied.arguments.reverse)
    val detachedOutput = BinderReference(BinderId(99), fixture.output.displayName)

    assertEquals(
      create("Aux", fixture.parameters, Refinement(swapped, Vector(TypeAlias("Out", fixture.output))))
        .left.toOption.map(_.code),
      Some("APPLIED_ARGUMENT_1_BINDER_MISMATCH")
    )
    assertEquals(
      create("Aux", fixture.parameters, Refinement(fixture.applied, Vector(TypeAlias("Out", detachedOutput))))
        .left.toOption.map(_.code),
      Some("OUTPUT_REFERENCE_BINDER_MISMATCH")
    )
  }

  test("arity duplicate names malformed RHS and extra refinements fail closed") {
    val fixture = nodes()
    val cases = List(
      create("Aux", fixture.parameters.take(2), fixture.rhs).left.toOption.map(_.code) -> Some("TYPE_PARAMETER_ARITY_UNSUPPORTED"),
      create("Aux", fixture.parameters.updated(2, fixture.parameters(2).copy(displayName = "N")), fixture.rhs).left.toOption.map(_.code) -> Some("TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT"),
      create("Aux", fixture.parameters, fixture.applied).left.toOption.map(_.code) -> Some("RHS_REFINEMENT_REQUIRED"),
      create("Aux", fixture.parameters, fixture.rhs.copy(members = fixture.rhs.members :+ TypeAlias("Other", fixture.output))).left.toOption.map(_.code) -> Some("REFINEMENT_MEMBER_COUNT_UNSUPPORTED")
    )
    cases.foreach { case (actual, expected) => assertEquals(actual, expected) }
  }

  private final case class Nodes(
      parameters: Vector[TypeParameter],
      applied: Applied,
      output: BinderReference,
      rhs: Refinement
  )

  private def nodes(
      firstName: String = "N",
      secondName: String = "M",
      outputName: String = "Out0",
      upperBoundName: String = "Nat",
      constructorName: String = "Add",
      memberName: String = "Out"
  ): Nodes =
    val parameters = Vector(
      TypeParameter(BinderId(1), firstName, SourceName(upperBoundName)),
      TypeParameter(BinderId(2), secondName, SourceName(upperBoundName)),
      TypeParameter(BinderId(3), outputName, SourceName(upperBoundName))
    )
    val applied = Applied(
      SourceName(constructorName),
      Vector(
        BinderReference(parameters(0).binderId, firstName),
        BinderReference(parameters(1).binderId, secondName)
      )
    )
    val output = BinderReference(parameters(2).binderId, outputName)
    val rhs = Refinement(applied, Vector(TypeAlias(memberName, output)))
    Nodes(parameters, applied, output, rhs)

  private def validPlan(
      aliasName: String = "Aux",
      firstName: String = "N",
      secondName: String = "M",
      outputName: String = "Out0",
      upperBoundName: String = "Nat",
      constructorName: String = "Add",
      memberName: String = "Out"
  ): Plan =
    val fixture = nodes(firstName, secondName, outputName, upperBoundName, constructorName, memberName)
    create(aliasName, fixture.parameters, fixture.rhs).fold(error => fail(error.message), identity)
