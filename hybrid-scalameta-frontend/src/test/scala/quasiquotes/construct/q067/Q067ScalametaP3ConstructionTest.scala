package quasiquotes.construct.q067

final class Q067ScalametaP3ConstructionTest extends munit.FunSuite:
  test("public typed-Scalameta qr constructs and runs one local Int identity method"):
    assertEquals(Q067ScalametaP3ConstructionMacros.intIdentity(41), 41)

  test("public typed-Scalameta qr supports fixed String Boolean and alpha-renamed source binders"):
    assertEquals(Q067ScalametaP3ConstructionMacros.stringIdentity("value"), "value")
    assertEquals(Q067ScalametaP3ConstructionMacros.booleanIdentity(true), true)
    assertEquals(Q067ScalametaP3ConstructionMacros.alphaRenamedIdentity(43), 43)

  test("public qr preserves exact owner symbol Type and original caller Term identity with standard parity"):
    val evidence = Q067ScalametaP3ConstructionMacros.ownerEvidence(42)
    assertEquals(evidence.value, 42)
    assertEquals(evidence.engine, "Scalameta")
    assert(evidence.methodOwnedBySplice, evidence)
    assert(evidence.distinctSymbols, evidence)
    assert(evidence.parameterOwnedByMethod, evidence)
    assert(evidence.bodyUsesParameter, evidence)
    assert(evidence.resultUsesMethod, evidence)
    assert(evidence.parameterAndResultTypesEqual, evidence)
    assert(evidence.exactArgumentIdentity, evidence)
    assert(evidence.standardExactArgumentIdentity, evidence)
    assert(evidence.sameStructuralRolesAsStandard, evidence)

    val literal = Q067ScalametaP3ConstructionMacros.literalOwnerEvidence
    assertEquals(literal.value, 47)
    assert(literal.exactArgumentIdentity, literal)

  test("synthetic P3 construction keeps the accepted immediate matching provenance mismatch on both routes"):
    val evidence = Q067ScalametaP3ConstructionMacros.roundTripEvidence(44)
    assert(!evidence.hybridMatched, evidence)
    assert(!evidence.standardMatched, evidence)
    assert(evidence.hybridOutcome.contains("P3 requires explicit parameter and result Types"), evidence)
    assertEquals(evidence.hybridOutcome, evidence.standardOutcome)
    assert(evidence.exactHybridArgumentIdentity, evidence)
    assert(evidence.exactStandardArgumentIdentity, evidence)

  test("unsupported P3 shapes fail deterministically without current-Dotty fallback"):
    val evidence = Q067ScalametaP3ConstructionMacros.negativeEvidence
    assertEquals(evidence.size, 38)
    evidence.foreach { row =>
      assert(!row.contains("ADMIT"), row)
      assert(row.endsWith("deterministic=true"), row)
      assert(!row.contains("CurrentDottyFallback"), row)
    }
    assert(evidence.exists(_.startsWith("wrong-body|SCALAMETA_P3_SEMANTIC_REJECTION")), evidence)
    assert(evidence.exists(_.startsWith("unsupported-type|SCALAMETA_P3_TYPE_REJECTION")), evidence)
    assert(evidence.exists(_.startsWith("null-argument|SCALAMETA_P3_SOURCE_REJECTION")), evidence)
    assert(evidence.exists(_.startsWith("zero-hole|SCALAMETA_P3_SOURCE_REJECTION")), evidence)
    assertEquals(
      Q067ScalametaP3ConstructionMacros.incorrectLexicalSourceCategory,
      "SCALAMETA_P3_SOURCE_REJECTION"
    )
