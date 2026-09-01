package quasiquotes.neutral

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.parsers.Parsed

@nowarn("cat=deprecation")
class ScalametaAuxTypeAliasProjectionTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("canonical positioned alias projects to the production binder-aware plan") {
    val definition = parseAlias(Canonical)
    val projected = project(definition, canonicalExpectation)
      .fold(problem => fail(problem.message), identity)

    assertEquals(projected.plan.aliasDisplayName, "Aux")
    assertEquals(
      projected.plan.typeParameters.map(_.binderId),
      Vector(BinderId(0), BinderId(1), BinderId(2))
    )
    assertEquals(
      projected.plan.appliedBase.arguments,
      Vector(
        TypeParameterReference(BinderId(0), "N"),
        TypeParameterReference(BinderId(1), "M")
      )
    )
    assertEquals(
      projected.plan.outputReference,
      TypeParameterReference(BinderId(2), "Out0")
    )
    assertEquals(projected.plan.argumentBinderPositions, Vector(0, 1))
    assertEquals(projected.sourceSpan, Some(NeutralSourceSpan(0, 73)))
  }

  test("fully renamed legal source uses caller expectations without changing binder roles") {
    val source =
      "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
    val expected = AuxTypeAliasExpectation(
      aliasName = "Evidence",
      firstParameter = AuxTypeParameterExpectation("Left", "Domain"),
      secondParameter = AuxTypeParameterExpectation("Right", "Domain"),
      outputParameter = AuxTypeParameterExpectation("Result0", "Domain"),
      targetName = "Combine",
      refinementMemberName = "Result"
    )
    val projected = project(parseAlias(source), expected)
      .fold(problem => fail(problem.message), identity)

    assertEquals(
      projected.plan.typeParameters.map(_.displayName),
      Vector("Left", "Right", "Result0")
    )
    assertEquals(
      projected.plan.typeParameters.map(_.upperBound),
      Vector.fill(3)(SourceName("Domain"))
    )
    assertEquals(projected.plan.appliedBase.constructor, SourceName("Combine"))
    assertEquals(projected.plan.refinementMember.memberName, "Result")
    assertEquals(
      projected.plan.appliedBase.arguments.map {
        case TypeParameterReference(binderId, _) => binderId
        case other => fail(s"expected binder reference, found $other")
      },
      Vector(BinderId(0), BinderId(1))
    )
    assertEquals(projected.plan.outputReference.binderId, BinderId(2))
  }

  test("programmatically constructed input remains truthfully unpositioned") {
    val definition = parseAlias(Canonical).copy()
    assertEquals(definition.pos, Position.None)

    val projected = project(definition, canonicalExpectation)
      .fold(problem => fail(problem.message), identity)
    assertEquals(projected.sourceSpan, None)
  }

  test("invalid expectations and source-name mismatches fail deterministically") {
    val definition = parseAlias(Canonical)
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(aliasName = "bad-name"),
      "NEUTRAL_AUX_EXPECTATION_INVALID"
    )
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(aliasName = "Other"),
      "NEUTRAL_AUX_ALIAS_NAME_MISMATCH"
    )
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(
        firstParameter = AuxTypeParameterExpectation("Left", "Nat")
      ),
      "NEUTRAL_AUX_TYPE_PARAMETER_NAME_MISMATCH"
    )
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(
        firstParameter = AuxTypeParameterExpectation("N", "Domain")
      ),
      "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_MISMATCH"
    )
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(targetName = "Combine"),
      "NEUTRAL_AUX_TARGET_NAME_MISMATCH"
    )
    assertProjectedRejected(
      definition,
      canonicalExpectation.copy(refinementMemberName = "Result"),
      "NEUTRAL_AUX_REFINEMENT_MEMBER_NAME_MISMATCH"
    )
  }

  test("outer alias and type-parameter near misses fail closed") {
    val cases = List(
      "type Aux[N <: Nat, M <: Nat] = Add[N, M] { type Out = N }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat, Extra <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_ARITY_UNSUPPORTED",
      "type Aux[N >: Nothing <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_LOWER_BOUND_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_UPPER_BOUND_MISSING",
      "type Aux[+N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_MODIFIERS_UNSUPPORTED",
      "type Aux[N <: Nat : Ordering, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED",
      "type Aux[N[X] <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat[String], M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED",
      "type Aux[N <: pkg.Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED",
      "private type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_ALIAS_MODIFIERS_UNSUPPORTED"
    )

    cases.foreach { case (source, expectedCode) =>
      assertRejected(source, canonicalExpectation, expectedCode)
    }
  }

  test("target application near misses fail closed without string rendering") {
    val cases = List(
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M, Out0] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = pkg.Add[N, M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_CONSTRUCTOR_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[List[N], M] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_ARGUMENT_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[M, N] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_BINDER_REFERENCE_MISMATCH",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, N] { type Out = Out0 }" ->
        "NEUTRAL_AUX_TARGET_BINDER_REFERENCE_MISMATCH"
    )

    cases.foreach { case (source, expectedCode) =>
      assertRejected(source, canonicalExpectation, expectedCode)
    }
  }

  test("refinement near misses fail closed") {
    val cases = List(
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M]" ->
        "NEUTRAL_AUX_RHS_REFINEMENT_REQUIRED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] {}" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0; type Other = Out0 }" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out }" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out[A] = Out0 }" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out >: Nothing = Out0 }" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_BOUNDS_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Result = Out0 }" ->
        "NEUTRAL_AUX_REFINEMENT_MEMBER_NAME_MISMATCH",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = String }" ->
        "NEUTRAL_AUX_OUTPUT_BINDER_REFERENCE_MISMATCH"
    )

    cases.foreach { case (source, expectedCode) =>
      assertRejected(source, canonicalExpectation, expectedCode)
    }
  }

  test("programmatic unsupported outer and refinement bounds are rejected") {
    val canonical = parseAlias(Canonical)
    val outerBounded = canonical.copy(
      bounds = canonical.bounds.copy(hi = Some(Type.Name("Top")))
    )
    assertProjectedRejected(
      outerBounded,
      canonicalExpectation,
      "NEUTRAL_AUX_ALIAS_BOUNDS_UNSUPPORTED"
    )

    val Type.Refine(Some(base), List(member: Defn.Type)) = canonical.body: @unchecked
    val contextualMember = member.copy(
      bounds = member.bounds.copy(context = List(Type.Name("Evidence")))
    )
    val boundedRefinement = canonical.copy(
      body = Type.Refine(Some(base), List(contextualMember))
    )
    assertProjectedRejected(
      boundedRefinement,
      canonicalExpectation,
      "NEUTRAL_AUX_REFINEMENT_MEMBER_BOUNDS_UNSUPPORTED"
    )
  }

  test("parser-owned illegal refinement modifiers remain parser failures") {
    val parsed = Scala3(
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { private type Out = Out0 }"
    ).parse[Stat]
    assert(parsed.isInstanceOf[Parsed.Error], clues(parsed))
  }

  private val canonicalExpectation = AuxTypeAliasExpectation(
    aliasName = "Aux",
    firstParameter = AuxTypeParameterExpectation("N", "Nat"),
    secondParameter = AuxTypeParameterExpectation("M", "Nat"),
    outputParameter = AuxTypeParameterExpectation("Out0", "Nat"),
    targetName = "Add",
    refinementMemberName = "Out"
  )

  private def parseAlias(source: String): Defn.Type =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Type => definition
      case other => fail(s"expected Defn.Type, found ${other.getClass.getSimpleName}")

  private def assertRejected(
      source: String,
      expected: AuxTypeAliasExpectation,
      expectedCode: String
  ): Unit =
    assertProjectedRejected(parseAlias(source), expected, expectedCode)

  private def assertProjectedRejected(
      definition: Defn.Type,
      expected: AuxTypeAliasExpectation,
      expectedCode: String
  ): Unit =
    val result = project(definition, expected)
    assertEquals(
      result.left.toOption.map(_.code),
      Some(expectedCode),
      clues(result)
    )

  private def project(
      definition: Defn.Type,
      expected: AuxTypeAliasExpectation
  ): Either[NeutralProjectionError, ProjectedAuxTypeAlias] =
    ScalametaAuxTypeAliasProjection.project(definition, expected)
