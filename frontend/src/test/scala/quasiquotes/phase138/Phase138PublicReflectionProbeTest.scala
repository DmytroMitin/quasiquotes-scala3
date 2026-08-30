package quasiquotes.phase138

import scala.annotation.experimental
import scala.compiletime.testing.typeCheckErrors

@experimental
class Phase138PublicReflectionProbeTest extends munit.FunSuite:
  private inline def messages(inline call: String): List[String] =
    typeCheckErrors(call).map(_.message)

  test("public reflection constructs an owned class, override, constructor call, and runtime invocation") {
    val evidence = Phase138PublicReflectionProbe.generated[Phase138Mapper](1, 41)
    assertEquals(evidence.result, 42)
    assert(evidence.classOwnedBySplice)
    assert(evidence.methodOwnedByClass)
    assert(evidence.constructorOwnedByClass)
    assert(evidence.methodHasOverrideFlag)
    assert(evidence.overridesParentMethod)
    assert(evidence.callerTermObjectRetained)
    assert(evidence.classTreeUsesRequestedParent)
    assert(evidence.invocationUsesPrimaryConstructor)
  }

  test("a caller local reference remains a lexical capture rather than an owned definition to move") {
    val callerLocal = 2
    val evidence = Phase138PublicReflectionProbe.generated[Phase138Mapper](callerLocal, 40)
    assertEquals(evidence.result, 42)
    assert(evidence.callerTermObjectRetained)
  }

  test("ordinary anonymous syntax has the same class, override, and constructor obligations") {
    val evidence = Phase138PublicReflectionProbe.quotedAnonymous
    assertEquals(evidence.result, 42)
    assertEquals(evidence.classDefinitionCount, 1)
    assertEquals(evidence.overrideDefinitionCount, 1)
    assertEquals(evidence.constructorApplicationCount, 1)
  }

  test("a method synthesized under the splice owner is rejected as detached from the generated class") {
    val diagnostics = messages(
      """quasiquotes.phase138.Phase138PublicReflectionProbe.rejectDetachedMethodOwner()"""
    )
    assertEquals(diagnostics.size, 1)
    assert(
      diagnostics.head.contains("PHASE138_DETACHED_METHOD_OWNER_REJECTED"),
      diagnostics.mkString("\n")
    )
  }
