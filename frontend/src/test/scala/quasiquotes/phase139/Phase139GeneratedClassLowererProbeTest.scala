package quasiquotes.phase139

import scala.annotation.experimental
import scala.compiletime.testing.typeCheckErrors

@experimental
class Phase139GeneratedClassLowererProbeTest extends munit.FunSuite:
  private inline def messages(inline call: String): List[String] =
    typeCheckErrors(call).map(_.message)

  test("the internal plan lowers one dynamic class and override through public reflection") {
    val evidence = Phase139GeneratedClassLowererProbe.generated[Phase139Mapper](
      1,
      41,
      "RuntimeSelectedMapper",
      "combine",
      "operand"
    )
    assertEquals(evidence.result, 42)
    assert(evidence.classOwnedBySplice)
    assert(evidence.methodOwnedByClass)
    assert(evidence.constructorOwnedByClass)
    assert(evidence.methodHasOverrideFlag)
    assert(evidence.overridesRequestedParentMember)
    assert(evidence.callerTermObjectRetained)
    assert(evidence.invocationArgumentObjectRetained)
    assert(evidence.bodyUsesGeneratedParameterRefExactlyOnce)
    assert(evidence.classTreeUsesRequestedParent)
    assert(evidence.invocationUsesPrimaryConstructor)
    assert(evidence.generatedClassNameUsesDisplayPrefix)
    assertEquals(evidence.generatedMethodName, "combine")
    assertEquals(evidence.generatedParameterName, "operand")
  }

  test("a caller local reference is captured unchanged") {
    val callerLocal = 2
    val evidence = Phase139GeneratedClassLowererProbe.generated[Phase139Mapper](
      callerLocal,
      40,
      "LocalCaptureMapper",
      "combine",
      "delta"
    )
    assertEquals(evidence.result, 42)
    assert(evidence.callerTermObjectRetained)
    assert(evidence.invocationArgumentObjectRetained)
    assert(evidence.bodyUsesGeneratedParameterRefExactlyOnce)
  }

  test("a detached generated method-owner role is rejected before ClassDef assembly") {
    assertPlanRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectDetachedMethodOwner()"),
      "DETACHED_METHOD_OWNER"
    )
  }

  test("an overloaded inherited name is rejected without overload selection") {
    assertPlanRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectOverloadedParent()"),
      "OVERLOADED_PARENT_MEMBER"
    )
  }

  test("invalid class, method, and parameter display names are rejected") {
    assertPlanRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectInvalidDisplayNames()"),
      "INVALID_DISPLAY_NAMES"
    )
  }

  test("a body that references a foreign generated binder is rejected") {
    assertPlanRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectMalformedBodyBinder()"),
      "MALFORMED_BODY_BINDER"
    )
  }

  test("owned ValDef, DefDef, and ClassDef captures are distinguished from lexical Term capture") {
    List(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectOwnedValCapture()"),
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectOwnedDefCapture()"),
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.rejectOwnedClassCapture()")
    ).foreach(assertPlanRejection(_, "CAPTURE_CONTAINS_OWNED_DEFINITION"))
  }

  test("the compiler rejects a method name with no inherited target") {
    assertCompilerRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.compilerRejectsNoMatchingMember()")
    )
  }

  test("the compiler rejects an incompatible override parameter") {
    assertCompilerRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.compilerRejectsIncompatibleParameter()")
    )
  }

  test("the compiler rejects an incompatible override result") {
    assertCompilerRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.compilerRejectsIncompatibleResult()")
    )
  }

  test("the compiler rejects an override of a final inherited member") {
    assertCompilerRejection(
      messages("quasiquotes.phase139.Phase139GeneratedClassLowererProbe.compilerRejectsFinalMember()")
    )
  }

  private def assertPlanRejection(diagnostics: List[String], code: String): Unit =
    assertEquals(diagnostics.size, 1, diagnostics.mkString("\n"))
    assert(diagnostics.head.contains(s"PHASE139_PLAN_REJECTED_$code"), diagnostics.mkString("\n"))

  private def assertCompilerRejection(diagnostics: List[String]): Unit =
    assert(diagnostics.nonEmpty, "expected compiler rejection")
    assert(
      diagnostics.forall(!_.contains("PHASE139_PLAN_REJECTED")),
      diagnostics.mkString("\n")
    )
