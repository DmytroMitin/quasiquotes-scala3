package quasiquotes.matching

import quasiquotes.source.{
  GeneratedSegment,
  HoleOccurrence,
  HoleRole,
  SourceId,
  SourceOrigin,
  SourceSpan
}

final class N068SelectedNameSourceNormalizerTest extends munit.FunSuite:
  import SelectedNameSourceNormalizationError.*

  test("normalizes repeated selected occurrences while retaining public source truth and exact provenance"):
    val normalized = normalize(
      "$receiver.$member.$member($arg)",
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )

    assertEquals(
      normalized.publicMapped.patternSource.source,
      "__qqhole_receiver.__qqhole_member.__qqhole_member(__qqhole_arg)"
    )
    assertEquals(
      normalized.generatedSource,
      "__qqhole_receiver.__qqhole_member__selected_1.__qqhole_member__selected_0(__qqhole_arg)"
    )
    assertEquals(
      normalized.occurrences.map(_.name),
      Vector("receiver", "member", "member", "arg")
    )
    assertEquals(
      normalized.occurrences.map(_.generatedName),
      Vector(
        "__qqhole_receiver",
        "__qqhole_member__selected_1",
        "__qqhole_member__selected_0",
        "__qqhole_arg"
      )
    )
    assertEquals(
      normalized.occurrences.map(_.originalSpan),
      Vector(
        SourceSpan(0, 9),
        SourceSpan(10, 17),
        SourceSpan(18, 25),
        SourceSpan(26, 30)
      )
    )
    assertEquals(
      normalized.occurrences.map(_.generatedSpan),
      Vector(
        SourceSpan(0, 17),
        SourceSpan(18, 45),
        SourceSpan(46, 73),
        SourceSpan(74, 86)
      )
    )
    assertEquals(
      normalized.occurrences.map(_.role).distinct,
      Vector(HoleRole.TermPattern)
    )
    assertEquals(
      normalized.ordinaryHoleIndex.entries,
      Vector("__qqhole_arg" -> "arg", "__qqhole_receiver" -> "receiver")
    )
    assertEquals(
      normalized.selectedOccurrences,
      Vector(
        SelectedNamePatternOccurrence(
          "member",
          1,
          "__qqhole_member__selected_1"
        ),
        SelectedNamePatternOccurrence(
          "member",
          0,
          "__qqhole_member__selected_0"
        )
      )
    )
    assertEquals(
      rewrittenOccurrences(normalized),
      normalized.occurrences
    )
    assertEquals(
      normalized.sourceMap.generatedSource,
      normalized.generatedSource
    )
    assertEquals(
      normalized.sourceMap.generatedSourceId,
      SourceId.VirtualTermPatternParserInput
    )

  test("selected transports avoid literal identifiers and every public transport deterministically"):
    val normalized = normalize(
      "__qqhole_member__selected_1.$member.$member",
      Vector(
        SelectedNameSourcePosition(0, 1),
        SelectedNameSourcePosition(1, 0)
      )
    )

    assertEquals(
      normalized.selectedOccurrences.map(_.transport),
      Vector(
        "__qqhole_member__selected_1_1",
        "__qqhole_member__selected_0"
      )
    )
    assertEquals(
      normalized.occurrences.map(_.generatedName).distinct.size,
      normalized.occurrences.size
    )
    assert(
      normalized.selectedOccurrences
        .map(_.transport)
        .forall(!normalized.publicMapped.occurrences.map(_.generatedName).contains(_))
    )

  test("replays authoritative transformed literal segments and their exact origins"):
    val normalized = normalize(
      "$receiver.$first.$second(s\"$arg\")",
      Vector(
        SelectedNameSourcePosition(1, 1),
        SelectedNameSourcePosition(2, 0)
      )
    )

    assertEquals(
      normalized.generatedSource,
      "__qqhole_receiver.__qqhole_first__selected_1.__qqhole_second__selected_0(s\"$__qqhole_arg\")"
    )
    assertEquals(
      normalized.sourceMap.segments.map(_.origin),
      normalized.publicMapped.originMap.segments.map(_.origin)
    )
    assertEquals(
      normalized.sourceMap.segments.head.generatedSpan.start,
      0
    )
    assertEquals(
      normalized.sourceMap.segments.last.generatedSpan.end,
      normalized.generatedSource.length
    )
    normalized.sourceMap.segments
      .zip(normalized.sourceMap.segments.drop(1))
      .foreach { case (left, right) =>
        assertEquals(left.generatedSpan.end, right.generatedSpan.start)
      }
    assertEquals(rewrittenOccurrences(normalized), normalized.occurrences)

  test("ordinary repeated holes retain one transport and source order while selected holes leave their index"):
    val normalized = normalize(
      "$ordinary + $ordinary + $first.$second",
      Vector(
        SelectedNameSourcePosition(2, 1),
        SelectedNameSourcePosition(3, 0)
      )
    )
    val ordinary = normalized.occurrences.filter(_.name == "ordinary")

    assertEquals(
      ordinary.map(_.generatedName),
      Vector("__qqhole_ordinary", "__qqhole_ordinary")
    )
    assertEquals(
      normalized.ordinaryHoleIndex.entries,
      Vector("__qqhole_ordinary" -> "ordinary")
    )
    normalized.selectedOccurrences.foreach { selected =>
      assertEquals(
        normalized.ordinaryHoleIndex.semanticNameFor(selected.transport),
        None
      )
    }
    assertEquals(
      normalized.occurrences.map(_.name),
      Vector("ordinary", "ordinary", "first", "second")
    )

  test("rejects selected and ordinary semantic-name category collisions"):
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        "$member.$member.$second",
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      ),
      Left(CaptureCategoryConflict("member"))
    )

  test("rejects malformed missing extra duplicate and stale selected position metadata"):
    val source = "$receiver.$first.$second"
    val validFirst = SelectedNameSourcePosition(1, 1)
    val validSecond = SelectedNameSourcePosition(2, 0)

    assertEquals(
      SelectedNameSourceNormalizer.normalize(null, Vector(validFirst, validSecond)),
      Left(MissingOriginalSource)
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(source, null),
      Left(MissingSelectedPositions)
    )
    Vector(Vector.empty, Vector(validFirst), Vector(validFirst, validSecond, validSecond))
      .zip(Vector(0, 1, 3))
      .foreach { case (positions, size) =>
        assertEquals(
          SelectedNameSourceNormalizer.normalize(source, positions),
          Left(SelectedPositionCount(size))
        )
      }
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(validFirst, null.asInstanceOf[SelectedNameSourcePosition])
      ),
      Left(MissingSelectedPosition(1))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(validFirst, validFirst.copy(selectOrdinal = 0))
      ),
      Left(DuplicateSourceOccurrenceIndex(1))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(validFirst, validSecond.copy(selectOrdinal = 1))
      ),
      Left(DuplicateSelectOrdinal(1))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(validFirst.copy(selectOrdinal = -1), validSecond)
      ),
      Left(InvalidSelectOrdinal(-1))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        source,
        Vector(validFirst, validSecond.copy(sourceOccurrenceIndex = 3))
      ),
      Left(InvalidSourceOccurrenceIndex(3, 3))
    )
    assert(
      SelectedNameSourceNormalizer
        .normalize("$receiver.$", Vector(validFirst, validSecond))
        .left
        .toOption
        .exists(_.isInstanceOf[PublicPatternSourceError])
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        "$receiver.$type.$second",
        Vector(validFirst, validSecond)
      ),
      Left(UnsupportedSelectedSemanticName("type"))
    )

  private def normalize(
      source: String,
      positions: Vector[SelectedNameSourcePosition]
  ): NormalizedSelectedNameSource =
    SelectedNameSourceNormalizer
      .normalize(source, positions)
      .fold(error => fail(error.message), identity)

  private def rewrittenOccurrences(
      normalized: NormalizedSelectedNameSource
  ): Vector[HoleOccurrence] =
    normalized.sourceMap.segments.collect {
      case GeneratedSegment(
            generatedSpan,
            SourceOrigin.RewrittenHole(_, originalSpan, name, role)
          ) =>
        HoleOccurrence(
          name,
          normalized.generatedSource.substring(
            generatedSpan.start,
            generatedSpan.end
          ),
          originalSpan,
          generatedSpan,
          role
        )
    }
