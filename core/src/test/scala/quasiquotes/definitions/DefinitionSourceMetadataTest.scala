package quasiquotes.definitions

import quasiquotes.parser.{TermShape, TypeShape}
import quasiquotes.source.*

class DefinitionSourceMetadataTest extends munit.FunSuite:
  private val sourceId = SourceId("definition-source")
  private val shape =
    DefinitionShape
      .parameterlessDef(
        DefinitionName.plain("answer").toOption.get,
        TypeShape.Identifier("Int"),
        TermShape.Literal("42")
      )
      .toOption
      .get

  private def validSpans: DefinitionComponentSpans =
    DefinitionComponentSpans
      .create(
        SourceSpan(0, 24),
        SourceSpan(4, 10),
        SourceSpan(13, 16),
        SourceSpan(21, 23)
      )
      .toOption
      .get

  test("component spans are nonempty ordered contained and need not be adjacent") {
    val spans = validSpans

    assertEquals(spans.definition, SourceSpan(0, 24))
    assertEquals(spans.name, SourceSpan(4, 10))
    assertEquals(spans.declaredType, SourceSpan(13, 16))
    assertEquals(spans.body, SourceSpan(21, 23))
    assertEquals(
      spans.render,
      "DefinitionComponentSpans(definition=[0, 24), name=[4, 10), type=[13, 16), body=[21, 23))"
    )
    assertEquals(spans, validSpans)
  }

  test("every required source span must be nonempty") {
    val candidates = Vector(
      (SourceSpan(0, 0), SourceSpan(0, 1), SourceSpan(1, 2), SourceSpan(2, 3)),
      (SourceSpan(0, 4), SourceSpan(1, 1), SourceSpan(2, 3), SourceSpan(3, 4)),
      (SourceSpan(0, 4), SourceSpan(0, 1), SourceSpan(2, 2), SourceSpan(3, 4)),
      (SourceSpan(0, 4), SourceSpan(0, 1), SourceSpan(2, 3), SourceSpan(4, 4))
    )

    candidates.foreach { case (definition, name, declaredType, body) =>
      assert(DefinitionComponentSpans.create(definition, name, declaredType, body).isLeft)
    }
  }

  test("component spans must be contained by the complete definition span") {
    val error =
      DefinitionComponentSpans
        .create(
          SourceSpan(2, 20),
          SourceSpan(1, 4),
          SourceSpan(8, 11),
          SourceSpan(16, 18)
        )
        .left
        .toOption
        .get

    assertEquals(
      error.message,
      "Invalid definition source metadata: the name span must be contained by the definition span."
    )
  }

  test("component spans must be ordered and non-overlapping") {
    val nameOverlap =
      DefinitionComponentSpans.create(
        SourceSpan(0, 20),
        SourceSpan(4, 10),
        SourceSpan(9, 12),
        SourceSpan(15, 18)
      )
    val typeOverlap =
      DefinitionComponentSpans.create(
        SourceSpan(0, 20),
        SourceSpan(4, 8),
        SourceSpan(9, 15),
        SourceSpan(14, 18)
      )

    assertEquals(
      nameOverlap.left.toOption.get.message,
      "Invalid definition source metadata: component spans must be ordered and non-overlapping: name before type."
    )
    assertEquals(
      typeOverlap.left.toOption.get.message,
      "Invalid definition source metadata: component spans must be ordered and non-overlapping: type before body."
    )
  }

  test("semantic shape is constructible and locatable without an origin map") {
    val located = LocatedDefinitionShape.create(shape, sourceId, validSpans).toOption.get

    assertEquals(located.shape, shape)
    assertEquals(located.sourceId, sourceId)
    assertEquals(located.components, validSpans)
    assertEquals(located.originMap, None)
    assert(located.render.endsWith("originMap=none)"))
  }

  test("coherent complete generated coverage is accepted") {
    val origin = SourceOrigin.OriginalText(SourceId("original"), SourceSpan(0, 24))
    val sourceMap = GeneratedSourceMap(
      "abcdefghijklmnopqrstuvwx",
      sourceId,
      Vector(
        GeneratedSegment(SourceSpan(0, 12), origin),
        GeneratedSegment(SourceSpan(12, 24), origin)
      )
    )
    val located =
      LocatedDefinitionShape.create(shape, sourceId, validSpans, Some(sourceMap)).toOption.get

    assertEquals(located.originMap, Some(sourceMap))
    assert(located.render.endsWith("originMap=present)"))
  }

  test("origin-map identity range and complete-coverage invariants are enforced") {
    val origin = SourceOrigin.OriginalText(SourceId("original"), SourceSpan(0, 24))
    val wrongIdentity = GeneratedSourceMap(
      "abcdefghijklmnopqrstuvwx",
      SourceId("other"),
      Vector(GeneratedSegment(SourceSpan(0, 24), origin))
    )
    val tooShort = GeneratedSourceMap(
      "short",
      sourceId,
      Vector(GeneratedSegment(SourceSpan(0, 5), origin))
    )
    val gap = GeneratedSourceMap(
      "abcdefghijklmnopqrstuvwx",
      sourceId,
      Vector(
        GeneratedSegment(SourceSpan(0, 10), origin),
        GeneratedSegment(SourceSpan(12, 24), origin)
      )
    )

    assert(LocatedDefinitionShape.create(shape, sourceId, validSpans, Some(wrongIdentity)).isLeft)
    assert(LocatedDefinitionShape.create(shape, sourceId, validSpans, Some(tooShort)).isLeft)
    assertEquals(
      LocatedDefinitionShape.create(shape, sourceId, validSpans, Some(gap)).left.toOption.get.message,
      "Invalid definition source metadata: the generated source map must cover the complete definition span without holes."
    )
  }
