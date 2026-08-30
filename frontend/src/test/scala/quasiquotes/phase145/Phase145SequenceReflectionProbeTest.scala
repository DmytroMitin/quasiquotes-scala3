package quasiquotes.phase145

class Phase145SequenceReflectionProbeTest extends munit.FunSuite:
  test("ordered sequence expansion supports empty, one, many, and fixed surrounding arguments") {
    val callerLocal = 2
    val evidence = Phase145SequenceReflectionProbe.evidence(callerLocal)

    assertEquals(evidence.applyEmpty, Nil)
    assertEquals(evidence.applyOne, List(1))
    assertEquals(evidence.applyMany, List(1, 2, 42, 2))
    assertEquals(evidence.fixedAround, List(-1, 1, 2, 42, 2, 99))
  }

  test("one-list constructor expansion uses the same empty, one, and ordered-many primitive") {
    val evidence = Phase145SequenceReflectionProbe.evidence(2)

    assertEquals(evidence.newEmpty, Nil)
    assertEquals(evidence.newOne, List(1))
    assertEquals(evidence.newMany, List(1, 2, 42, 2))
  }

  test("reflection factories retain exact caller-owned children without reparsing or owner repair") {
    val evidence = Phase145SequenceReflectionProbe.evidence(2)

    assert(evidence.applyChildrenRetainObjects)
    assert(evidence.newChildrenRetainObjects)
    assert(evidence.callerLocalRetained)
    assert(evidence.reusedQrTermRetained)
    assert(evidence.ownedDefinitionBlockRetained)
  }
