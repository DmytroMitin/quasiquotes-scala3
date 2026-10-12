package quasiquotes.construct.q067r

final class Q067RRootOnlyP3Test extends munit.FunSuite:
  test("public typed-Scalameta qr retains the admitted root P3 runtime control"):
    assertEquals(Q067RRootOnlyP3Macros.rootRuntime(41), 41)

  test("typed-Scalameta P3 construction is admitted only at the complete source root"):
    val rows = Q067RRootOnlyP3Macros.admissionMatrix(41)
    assertEquals(rows.map(_.label), List(
      "root",
      "if-branch",
      "apply-argument",
      "tuple-element",
      "p2-initializer",
      "lambda-body"
    ))
    rows.foreach { row =>
      assert(row.parsed, row)
      assert(row.exactCompilerAccepted, row)
      assertEquals(row.bindingCount, 1, row)
      assert(row.exactTermOrigin, row)
      assert(row.exactArgumentIdentity, row)
    }
    assertEquals(rows.head.standardOutcome, "ADMIT")
    assertEquals(rows.head.hybridOutcome, "ADMIT:Scalameta")
    rows.tail.foreach(row => assert(row.standardOutcome.startsWith("REJECT:"), row))
    val invalidHybridRows = rows.tail.filterNot { row =>
      val fields = row.hybridOutcome.split(":", 4)
      row.hybridOutcome.startsWith("SCALAMETA_P3_SEMANTIC_REJECTION:") &&
        fields.length == 4 &&
        fields(1).toIntOption.exists(_ >= 0) &&
        fields(2).toIntOption.exists(end => end > fields(1).toInt) &&
        row.hybridOutcome.contains("root") &&
        !row.hybridOutcome.contains("CurrentDottyFallback")
    }
    assertEquals(invalidHybridRows, Nil, clues(rows))
