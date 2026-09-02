package external.consumer

final class Q008StandardUmbrellaFacadeTest extends munit.FunSuite:
  test("the standard umbrella preserves all six syntax families and exact behavior"):
    val evidence = Q008StandardUmbrellaFacadeMacros.evidence(42)

    assertEquals(evidence.scalarConstruction, 42)
    assertEquals(evidence.sequenceConstruction, List(1, 2, 3))
    assertEquals(evidence.scalarCapture, (20, 22))
    assertEquals(evidence.rankedApplyCapture, List(1, 2, 3))
    assertEquals(evidence.rankedNewCapture, List(1, 2, 3))
    assert(evidence.typeConstruction)
    assert(evidence.typeCaptureIdentity)
    assertEquals(evidence.definitionResult, 42)
    assert(evidence.definitionOwnerBinderAndTypeIdentity)
    assert(evidence.definitionCaptureIdentity)
    assert(evidence.scalarRankedFallthrough)
    assert(evidence.definitionFallthrough)

  test("the four original standard hosts remain usable"):
    assert(Q008StandardUmbrellaFacadeMacros.legacyImportsRemainUsable)
