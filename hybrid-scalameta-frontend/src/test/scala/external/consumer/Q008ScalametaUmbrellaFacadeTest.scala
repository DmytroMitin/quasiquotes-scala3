package external.consumer

final class Q008ScalametaUmbrellaFacadeTest extends munit.FunSuite:
  test("the typed Scalameta umbrella preserves all six opt-in syntax families"):
    val evidence = Q008ScalametaUmbrellaFacadeMacros.evidence(42)

    assert(evidence.termConstructionIdentity)
    assertEquals(evidence.termConstructionResult, 42)
    assertEquals(evidence.orderedTermCaptures, (20, 22))
    assert(evidence.typeConstruction)
    assert(evidence.typeCaptureIdentity)
    assertEquals(evidence.definitionResult, 42)
    assert(evidence.definitionOwnerBinderAndTypeIdentity)
    assert(evidence.definitionCaptureIdentity)
    assert(evidence.mismatchFallthrough)

  test("the two original typed Scalameta hosts remain usable"):
    assert(Q008ScalametaUmbrellaFacadeMacros.legacyImportsRemainUsable)
