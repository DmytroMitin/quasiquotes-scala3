package quasiquotes.source

class SourceMetadataTest extends munit.FunSuite:
  private val originalId = SourceId("original")
  private val generatedId = SourceId("generated")

  test("valid spans use zero-based half-open UTF-16 code-unit offsets") {
    val text = "a\uD83D\uDE00b"
    val empty = SourceSpan(2, 2)
    val emoji = SourceSpan(1, 3)

    assertEquals(text.length, 4)
    assert(empty.isEmpty)
    assertEquals(emoji.length, 2)
    assert(emoji.contains(1))
    assert(emoji.contains(2))
    assert(!emoji.contains(3))
  }

  test("invalid spans and source identities are rejected") {
    intercept[IllegalArgumentException](SourceSpan(-1, 0))
    intercept[IllegalArgumentException](SourceSpan(2, 1))
    intercept[IllegalArgumentException](SourceId(""))
  }

  test("overlap and intersection preserve half-open boundaries") {
    val left = SourceSpan(1, 4)
    val touching = SourceSpan(4, 7)
    val crossing = SourceSpan(3, 6)

    assert(!left.overlaps(touching))
    assertEquals(left.intersection(touching), None)
    assert(left.overlaps(crossing))
    assertEquals(left.intersection(crossing), Some(SourceSpan(3, 4)))
  }

  test("generated segments enforce ordering range and non-overlap") {
    val first = GeneratedSegment(SourceSpan(0, 2), SourceOrigin.OriginalText(originalId, SourceSpan(0, 2)))
    val second = GeneratedSegment(SourceSpan(2, 4), SourceOrigin.OriginalText(originalId, SourceSpan(2, 4)))
    val accepted = GeneratedSourceMap("abcd", generatedId, Vector(first, second))

    assertEquals(accepted, GeneratedSourceMap("abcd", generatedId, Vector(first, second)))
    intercept[IllegalArgumentException] {
      GeneratedSourceMap("abcd", generatedId, Vector(first, second.copy(generatedSpan = SourceSpan(1, 3))))
    }
    intercept[IllegalArgumentException] {
      GeneratedSourceMap("abcd", generatedId, Vector(first, second.copy(generatedSpan = SourceSpan(4, 5))))
    }
  }

  test("point and range lookups retain repeated origins in generated order") {
    val literalOrigin = SourceOrigin.OriginalText(originalId, SourceSpan(0, 1))
    val holeOrigin = SourceOrigin.RewrittenHole(originalId, SourceSpan(1, 3), "x", HoleRole.TermPattern)
    val map = GeneratedSourceMap(
      "aXXbXX",
      generatedId,
      Vector(
        GeneratedSegment(SourceSpan(0, 1), literalOrigin),
        GeneratedSegment(SourceSpan(1, 3), holeOrigin),
        GeneratedSegment(SourceSpan(3, 4), literalOrigin),
        GeneratedSegment(SourceSpan(4, 6), holeOrigin)
      )
    )

    assertEquals(map.originAt(0), Some(literalOrigin))
    assertEquals(map.originAt(1), Some(holeOrigin))
    assertEquals(map.originAt(map.generatedSource.length), None)
    assertEquals(
      map.originsFor(SourceSpan(0, 5)).map(_.origin),
      Vector(literalOrigin, holeOrigin, literalOrigin, holeOrigin)
    )
    assertEquals(map.generatedSpansFor(_ == holeOrigin), Vector(SourceSpan(1, 3), SourceSpan(4, 6)))
  }

  test("located diagnostics combine a neutral generated span with mapped origins") {
    val origin = SourceOrigin.OriginalText(originalId, SourceSpan(2, 4))
    val map = GeneratedSourceMap("xy", generatedId, Vector(GeneratedSegment(SourceSpan(0, 2), origin)))
    val location = DiagnosticLocation.from(map, SourceSpan(0, 1))

    assertEquals(location.map(_.generatedSourceId), Some(generatedId))
    assertEquals(location.map(_.origins), Some(Vector(origin)))
    assertEquals(LocatedDiagnostic("problem", location).diagnostic, "problem")
    assertEquals(DiagnosticLocation.from(map, SourceSpan(1, 3)), None)
  }
