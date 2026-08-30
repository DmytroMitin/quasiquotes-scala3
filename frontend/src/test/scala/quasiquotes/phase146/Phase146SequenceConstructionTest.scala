package quasiquotes.phase146

final class Phase146SequenceConstructionTest extends munit.FunSuite:
  test("public sequence carrier expands empty, one, many, and fixed-around Apply arguments"):
    val evidence = Phase146SequenceConstructionMacros.evidence(2)

    assertEquals(evidence.applyEmpty, Nil)
    assertEquals(evidence.applyOne, List(1))
    assertEquals(evidence.applyMany, List(1, 2, 42, 2))
    assertEquals(evidence.applyFixedAround, List(-1, 1, 2, 42, 2, 99))

  test("one-list reflected and static constructors share ordered sequence expansion"):
    val evidence = Phase146SequenceConstructionMacros.evidence(2)

    assertEquals(evidence.newEmpty, Nil)
    assertEquals(evidence.newOne, List(1))
    assertEquals(evidence.newMany, List(1, 2, 42, 2))
    assertEquals(evidence.newFixedAround, List(-1, 1, 2, 42, 2, 99))
    assertEquals(evidence.staticNewOne, List(1))

  test("expanded arguments preserve exact caller Term objects and ownership-sensitive subtrees"):
    val evidence = Phase146SequenceConstructionMacros.evidence(2)

    assert(evidence.applyChildrenRetainObjects)
    assert(evidence.newChildrenRetainObjects)
    assert(evidence.callerLocalRetained)
    assert(evidence.reusedQrTermRetained)
    assert(evidence.ownedDefinitionBlockRetained)
    assertEquals(evidence.ordinarySingleTermUnchanged, 1)
