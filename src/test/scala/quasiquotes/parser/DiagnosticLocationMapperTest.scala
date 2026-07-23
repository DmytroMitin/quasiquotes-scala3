package quasiquotes.parser

import quasiquotes.source.*

class DiagnosticLocationMapperTest extends munit.FunSuite:
  private val originalId = SourceId("mapper-original")
  private val generatedId = SourceId("mapper-generated")
  private val origin = SourceOrigin.OriginalText(originalId, SourceSpan(0, 4))
  private val sourceMap = GeneratedSourceMap(
    "abcd",
    generatedId,
    Vector(GeneratedSegment(SourceSpan(0, 4), origin))
  )

  test("parse mapping chooses the first valid structured span and marks it exact") {
    val error = ParseError(
      "abcd",
      ParseErrorKind.SyntaxError,
      List("empty", "out", "valid", "later"),
      List(
        ParseDiagnostic("empty", Some(SourceSpan(1, 1))),
        ParseDiagnostic("out", Some(SourceSpan(3, 6))),
        ParseDiagnostic("valid", Some(SourceSpan(1, 3))),
        ParseDiagnostic("later", Some(SourceSpan(0, 1)))
      )
    )
    val location = DiagnosticLocationMapper.fromParseError(error, sourceMap)

    assertEquals(location.map(_.sourceId), Some(generatedId))
    assertEquals(location.map(_.span), Some(SourceSpan(1, 3)))
    assertEquals(location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
  }

  test("whole-source mapping prefers a parsed-tree span but never claims exact precision") {
    val preferred = DiagnosticLocationMapper.wholeSource(sourceMap, Some(SourceSpan(1, 3)))
    val fallback = DiagnosticLocationMapper.wholeSource(sourceMap)

    assertEquals(preferred.map(_.span), Some(SourceSpan(1, 3)))
    assertEquals(preferred.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    assertEquals(fallback.map(_.span), Some(SourceSpan(0, 4)))
    assertEquals(fallback.map(_.precision), Some(DiagnosticPrecision.WholeSource))
  }

  test("invalid or originless candidates yield fallback or unavailable locations") {
    val gapped = GeneratedSourceMap(
      "abcd",
      generatedId,
      Vector(GeneratedSegment(SourceSpan(0, 1), origin))
    )
    val noStructured = ParseError.syntax("abcd", List("syntax at offset 2"))

    assertEquals(DiagnosticLocationMapper.fromParseError(noStructured, sourceMap), None)
    assertEquals(
      DiagnosticLocationMapper.wholeSource(gapped, Some(SourceSpan(2, 3))).map(_.span),
      Some(SourceSpan(0, 4))
    )
    assertEquals(
      DiagnosticLocationMapper.wholeSource(
        GeneratedSourceMap("", generatedId, Vector.empty)
      ),
      None
    )
  }

  test("parse mapping skips an originless span before the first mappable span") {
    val gapped = GeneratedSourceMap(
      "abcd",
      generatedId,
      Vector(GeneratedSegment(SourceSpan(2, 4), origin))
    )
    val error = ParseError(
      "abcd",
      ParseErrorKind.SyntaxError,
      List("originless", "mappable"),
      List(
        ParseDiagnostic("originless", Some(SourceSpan(0, 1))),
        ParseDiagnostic("mappable", Some(SourceSpan(2, 3)))
      )
    )

    assertEquals(
      DiagnosticLocationMapper.fromParseError(error, gapped).map(_.span),
      Some(SourceSpan(2, 3))
    )
  }
