package quasiquotes.phase132

class SourceOwnedLocalDefConstructionTest extends munit.FunSuite:
  test("qr constructs one source-owned identity method and invokes it") {
    assertEquals(SourceOwnedLocalDefMacros.identity(41), 41)
  }

  test("method, parameter, body, following reference, and caller argument retain hygienic ownership") {
    assertEquals(
      SourceOwnedLocalDefMacros.ownerAndHygieneEvidence(7),
      (true, true, true, true, true, true)
    )
  }

  test("each qr expansion creates a fresh local method symbol") {
    assert(SourceOwnedLocalDefMacros.freshPerExpansion)
  }

  test("unsupported local-def topology, clauses, body, Type positions, and dynamic names fail closed") {
    val messages = SourceOwnedLocalDefMacros.rejectionEvidence
    assertEquals(messages.size, 14)
    assert(messages(0).contains("exactly one local method"), messages.mkString("\n---\n"))
    assert(messages(1).contains("modifiers or annotations"), messages(1))
    assert(messages(2).contains("type parameters"), messages(2))
    assert(messages(3).contains("exactly one ordinary value parameter"), messages(3))
    assert(messages(4).contains("body must be exactly"), messages(4))
    assert(messages(5).contains("body must be exactly"), messages(5))
    assert(messages(6).contains("Term splice"), messages(6))
    assert(messages(6).contains("parameter Type"), messages(6))
    assert(messages(7).contains("non-constructor type syntax"), messages(7))
    assert(messages(8).contains("not compatible"), messages(8))
    assert(messages(9).contains("Selected-member name"), messages(9))
    assert(messages(10).contains("complete explicit parameter and result Types"), messages(10))
    assert(messages(11).contains("fixed Int/String/Boolean Types"), messages(11))
    assert(messages(12).contains("Constructed-type splice"), messages(12))
    assert(messages(12).contains("parameter Type"), messages(12))
    assert(messages(13).contains("fixed Int/String/Boolean Types"), messages(13))
  }
