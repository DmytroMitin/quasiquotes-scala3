package quasiquotes.construct.q066

import Q066ScalametaP3ConstructionMacros.*

private object Q066ScalametaP3ConstructionFixtures:
  val callerOwned = 42
  val intResult = intValue(callerOwned)
  val stringResult = stringValue("value")
  val booleanResult = booleanValue(true)
  val alphaRenamedResult = alphaRenamedValue(43)
  val owner = ownerEvidence(callerOwned)
  val literal = literalEvidence
  val hybridRoundTrip = hybridRoundTripEvidence(callerOwned)
  val literalRoundTrip = literalRoundTripEvidence

final class Q066ScalametaP3ConstructionFeasibilityTest extends munit.FunSuite:
  import Q066ScalametaP3ConstructionFixtures.*

  test("test-only Scalameta P3 adapter constructs fixed Int String and Boolean siblings"):
    assertEquals(intResult, 42)
    assertEquals(stringResult, "value")
    assertEquals(booleanResult, true)

  test("accepted simple source binders may be alpha-renamed without changing the construction model"):
    assertEquals(alphaRenamedResult, 43)

  test("caller-owned and literal arguments retain exact identity ownership and standard-P3 role parity"):
    List(owner, literal).foreach { evidence =>
      assert(evidence.value > 0, evidence)
      assert(evidence.methodOwnedBySplice, evidence)
      assert(evidence.distinctSymbols, evidence)
      assert(evidence.parameterOwnedByMethod, evidence)
      assert(evidence.bodyUsesParameter, evidence)
      assert(evidence.resultUsesMethod, evidence)
      assert(evidence.exactArgumentIdentity, evidence)
      assert(evidence.standardExactArgumentIdentity, evidence)
      assert(evidence.sameStructuralRolesAsStandard, evidence)
    }
    assertEquals(owner.value, 42)
    assertEquals(literal.value, 41)

  test("constructed P3 tree matches standard P3 at the immediate synthetic-tree round-trip boundary"):
    assert(!hybridRoundTrip.hybridMatched, hybridRoundTrip)
    assert(!hybridRoundTrip.standardMatched, hybridRoundTrip)
    assert(hybridRoundTrip.hybridOutcome.contains("P3 requires explicit parameter and result Types"), hybridRoundTrip)
    assert(hybridRoundTrip.standardOutcome.contains("P3 requires explicit parameter and result Types"), hybridRoundTrip)
    assert(hybridRoundTrip.adapterExactArgumentIdentity, hybridRoundTrip)
    assert(hybridRoundTrip.standardExactArgumentIdentity, hybridRoundTrip)
    assert(hybridRoundTrip.sameImmediateRoundTripOutcome, hybridRoundTrip)
    assert(!literalRoundTrip.hybridMatched, literalRoundTrip)
    assert(!literalRoundTrip.standardMatched, literalRoundTrip)
    assert(literalRoundTrip.adapterExactArgumentIdentity, literalRoundTrip)
    assert(literalRoundTrip.standardExactArgumentIdentity, literalRoundTrip)
    assert(literalRoundTrip.sameImmediateRoundTripOutcome, literalRoundTrip)

  test("source-hole sidecar parse exact-compiler P2 and Q067 production stages remain distinct"):
    val evidence = stageEvidence
    assert(evidence.exists(_ == "root=Term.Block"), evidence)
    assert(evidence.exists(_ == "stats=Defn.Def,Term.Apply"), evidence)
    assert(evidence.exists(_ == "names=id,value"), evidence)
    assert(evidence.exists(_ == "types=STypeIdent(Int),STypeIdent(Int)"), evidence)
    assert(evidence.exists(_.startsWith("span=")), evidence)
    assert(evidence.exists(_ == "origin=0:TermSplice"), evidence)
    assert(evidence.exists(_ == "exact=true"), evidence)
    assert(evidence.exists(_ == "p2=true"), evidence)
    assert(evidence.exists(_ == "production=ADMIT"), evidence)
    assert(evidence.exists(_ == "hybrid=ADMIT:Scalameta"), evidence)
    assert(evidence.exists(_ == "identity=true"), evidence)

  test("bounded family negatives reject before any Q066 construction is exposed"):
    val evidence = negativeEvidence
    assertEquals(evidence.size, 37)
    evidence.foreach { row =>
      assert(!row.endsWith(":ADMIT"), evidence.mkString("\n"))
    }
    assert(evidence.exists(_ == "wrong-argument-type:Q066_P3_SEMANTIC_REJECTION"), evidence)
    assert(evidence.exists(_ == "owned-definition-splice:Q066_P3_SEMANTIC_REJECTION"), evidence)
    assert(evidence.exists(_ == "null-argument:Q066_P3_SOURCE_ADAPTER_REJECTION"), evidence)
    assert(evidence.exists(_ == "two-holes:Q066_P3_SEMANTIC_REJECTION"), evidence)
