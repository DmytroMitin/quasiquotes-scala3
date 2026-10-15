package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.TermShape
import quasiquotes.source.{HoleRole, SourceSpan}

final class N072RecoveredTwoSelectedNameSourcePatternTest
    extends munit.FunSuite:

  private val standardPositions = Vector(
    SelectedNameSourcePosition(1, 1),
    SelectedNameSourcePosition(2, 0)
  )

  test("recovers two distinct selected source roles for direct nullary and unary matching"):
    val cases = Vector(
      (
        "$receiver.$first.$second",
        (pattern: TermPattern) => pattern,
        TermShape.Select(
          TermShape.Select(identifier("service"), "alpha"),
          "beta"
        ),
        Map("receiver" -> identifier("service")),
        Vector("receiver")
      ),
      (
        "$receiver.$first.$second()",
        (pattern: TermPattern) => TermPattern.Apply(pattern, Nil),
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          Nil
        ),
        Map("receiver" -> identifier("service")),
        Vector("receiver")
      ),
      (
        "$receiver.$first.$second($arg)",
        (pattern: TermPattern) =>
          TermPattern.Apply(pattern, List(TermPattern.Hole("arg"))),
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "beta"
          ),
          List(TermShape.Literal("1"))
        ),
        Map(
          "receiver" -> identifier("service"),
          "arg" -> TermShape.Literal("1")
        ),
        Vector("receiver", "arg")
      )
    )

    cases.foreach { case (source, wrap, target, ordinaryBindings, ordinaryNames) =>
      val normalized = normalize(source, standardPositions)
      val compiled = wrap(twoSelectPattern(normalized))
      val recovered = recover(source, standardPositions, compiled)

      assertEquals(recovered.originalSource, source)
      assertEquals(recovered.publicMapped, normalized.publicMapped)
      assertEquals(recovered.generatedSource, normalized.generatedSource)
      assertEquals(recovered.sourceMap, normalized.sourceMap)
      assertEquals(recovered.occurrences, normalized.occurrences)
      assertEquals(recovered.ordinaryHoleIndex, normalized.ordinaryHoleIndex)
      assertEquals(
        recovered.selectedOccurrences,
        normalized.selectedOccurrences.sortBy(_.selectOrdinal)
      )
      assertEquals(
        recovered.selectedSourceOccurrences.map(_.name),
        Vector("second", "first")
      )
      assertEquals(
        recovered.selectedSourceOccurrences.map(_.generatedName),
        recovered.selectedOccurrences.map(_.transport)
      )
      assertEquals(recovered.ordinaryTermHoleNames, ordinaryNames)
      assertEquals(
        recovered.matchTarget(target),
        Right(
          NeutralSelectedNameMatchResult(
            ordinaryBindings,
            Map("first" -> selected("alpha"), "second" -> selected("beta"))
          )
        )
      )
    }

  test("keeps wrapper identity distinct while validated patterns retain semantic equality"):
    val source = "$receiver.$first.$second"
    val normalized = normalize(source, standardPositions)
    val compiled = twoSelectPattern(normalized)
    val first = recover(source, standardPositions, compiled)
    val second = recover(source, standardPositions, compiled)

    assertNotEquals(first, second)
    assertEquals(first.validatedPattern, second.validatedPattern)

  test("preserves two repeated selected source occurrences and N065 equality matching"):
    val source = "$receiver.$member.$member($receiver)"
    val positions = Vector(
      SelectedNameSourcePosition(1, 1),
      SelectedNameSourcePosition(2, 0)
    )
    val normalized = normalize(source, positions)
    val compiled = TermPattern.Apply(
      twoSelectPattern(normalized),
      List(TermPattern.Hole("receiver"))
    )
    val recovered = recover(source, positions, compiled)

    assertEquals(recovered.ordinaryTermHoleNames, Vector("receiver", "receiver"))
    assertEquals(
      recovered.selectedSourceOccurrences.map(_.originalSpan),
      Vector(SourceSpan(18, 25), SourceSpan(10, 17))
    )
    assertEquals(
      recovered.matchTarget(
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "same"),
            "same"
          ),
          List(identifier("service"))
        )
      ).map(_.selectedNameBindings),
      Right(Map("member" -> selected("same")))
    )
    assertEquals(
      recovered.matchTarget(
        TermShape.Apply(
          TermShape.Select(
            TermShape.Select(identifier("service"), "inner"),
            "outer"
          ),
          List(identifier("service"))
        )
      ),
      Left(
        NeutralSelectedNameMatchError.RepeatedSelectedNameCaptureMismatch(
          "member"
        )
      )
    )

  test("retains fixed middle names nonadjacent ordinals and exact private spans"):
    val source = "$receiver.$first.fixed.$second"
    val positions = Vector(
      SelectedNameSourcePosition(1, 2),
      SelectedNameSourcePosition(2, 0)
    )
    val normalized = normalize(source, positions)
    val byOrdinal = normalized.selectedOccurrences.map(value => value.selectOrdinal -> value).toMap
    val compiled = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(
          TermPattern.Hole("receiver"),
          byOrdinal(2).transport
        ),
        "fixed"
      ),
      byOrdinal(0).transport
    )
    val recovered = recover(source, positions, compiled)

    assertEquals(recovered.selectedOccurrences.map(_.selectOrdinal), Vector(0, 2))
    assertEquals(
      recovered.selectedSourceOccurrences.map(_.generatedSpan),
      recovered.selectedOccurrences.map(occurrence =>
        normalized.occurrences
          .find(_.generatedName == occurrence.transport)
          .getOrElse(fail("missing selected source occurrence"))
          .generatedSpan
      )
    )
    assertEquals(
      recovered.matchTarget(
        TermShape.Select(
          TermShape.Select(
            TermShape.Select(identifier("service"), "alpha"),
            "fixed"
          ),
          "beta"
        )
      ).map(_.selectedNameBindings),
      Right(Map("first" -> selected("alpha"), "second" -> selected("beta")))
    )

  test("keeps public mapped provenance separate from collision-safe private normalization"):
    val source = "__qqhole_member__selected_1.$member.$member"
    val positions = Vector(
      SelectedNameSourcePosition(0, 1),
      SelectedNameSourcePosition(1, 0)
    )
    val normalized = normalize(source, positions)
    val recovered = recover(
      source,
      positions,
      TermPattern.Select(
        TermPattern.Select(
          TermPattern.Identifier("__qqhole_member__selected_1"),
          selectedAt(normalized, 1).transport
        ),
        selectedAt(normalized, 0).transport
      )
    )

    assertEquals(
      recovered.publicMapped.patternSource.source,
      "__qqhole_member__selected_1.__qqhole_member.__qqhole_member"
    )
    assertEquals(
      recovered.selectedOccurrences.map(_.transport),
      Vector(
        "__qqhole_member__selected_0",
        "__qqhole_member__selected_1_1"
      )
    )
    assertNotEquals(
      recovered.publicMapped.originMap.generatedSource,
      recovered.sourceMap.generatedSource
    )
    assertEquals(rewrittenOccurrences(recovered), recovered.occurrences)
    assertEquals(
      recovered.selectedSourceOccurrences.map(_.role).distinct,
      Vector(HoleRole.TermPattern)
    )

  test("rejects missing null and invalid source position inputs without throwing"):
    val compiled = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), "first"),
      "second"
    )

    assert(RecoveredTwoSelectedNameSourcePattern.recover(null, standardPositions, compiled).isLeft)
    assert(RecoveredTwoSelectedNameSourcePattern.recover("$r.$a.$b", null, compiled).isLeft)
    assert(RecoveredTwoSelectedNameSourcePattern.recover("$r.$a.$b", Vector.empty, compiled).isLeft)
    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover("$r.$a.$b", standardPositions, null)
        .isLeft
    )
    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(
          "$r.$a.$b",
          standardPositions,
          TermPattern.Select(null, "selected")
        )
        .isLeft
    )

  test("rejects ordinal swaps duplicate transports and undeclared selected fields"):
    val source = "$receiver.$first.$second"
    val normalized = normalize(source, standardPositions)
    val first = normalized.selectedOccurrences.find(_.name == "first").get
    val second = normalized.selectedOccurrences.find(_.name == "second").get

    val swapped = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), second.transport),
      first.transport
    )
    assert(RecoveredTwoSelectedNameSourcePattern.recover(source, standardPositions, swapped).isLeft)

    val duplicated = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), first.transport),
      first.transport
    )
    assert(RecoveredTwoSelectedNameSourcePattern.recover(source, standardPositions, duplicated).isLeft)

    val sourceWithThird = "$receiver.$first.$second.$third"
    val normalizedWithThird = normalize(
      sourceWithThird,
      Vector(
        SelectedNameSourcePosition(1, 2),
        SelectedNameSourcePosition(2, 1)
      )
    )
    val thirdTransport = normalizedWithThird.occurrences.find(_.name == "third").get.generatedName
    val undeclared = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(
          TermPattern.Hole("receiver"),
          selectedAt(normalizedWithThird, 2).transport
        ),
        selectedAt(normalizedWithThird, 1).transport
      ),
      thirdTransport
    )
    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(
          sourceWithThird,
          Vector(
            SelectedNameSourcePosition(1, 2),
            SelectedNameSourcePosition(2, 1)
          ),
          undeclared
        )
        .isLeft
    )

  test("rejects selected transports reused outside declared Select fields"):
    val source = "$receiver.$first.$second"
    val normalized = normalize(source, standardPositions)
    val first = normalized.selectedOccurrences.find(_.name == "first").get
    val compiled = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole(first.transport), first.transport),
      selectedAt(normalized, 0).transport
    )

    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(source, standardPositions, compiled)
        .isLeft
    )

    val reusedAsIdentifier = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Identifier(first.transport),
        first.transport
      ),
      selectedAt(normalized, 0).transport
    )
    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(source, standardPositions, reusedAsIdentifier)
        .isLeft
    )

  test("rejects dropped extra renamed and reordered ordinary Term holes"):
    val source = "$receiver.$first.$second($arg)"
    val normalized = normalize(source, standardPositions)
    val selected = twoSelectPattern(normalized)
    val invalid = Vector(
      selected,
      TermPattern.Apply(selected, List(TermPattern.Hole("renamed"))),
      TermPattern.Apply(
        selected,
        List(TermPattern.Hole("receiver"), TermPattern.Hole("arg"))
      ),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("arg"), selectedAt(normalized, 1).transport),
        List(TermPattern.Hole("receiver"))
      )
    )

    invalid.foreach(pattern =>
      assert(
        RecoveredTwoSelectedNameSourcePattern
          .recover(source, standardPositions, pattern)
          .isLeft
      )
    )

  test("delegates unsupported Apply envelopes and target lexical failures to N065"):
    val source = "$receiver.$first.$second($arg)"
    val normalized = normalize(source, standardPositions)
    val unsupported = TermPattern.Apply(
      twoSelectPattern(normalized),
      List(TermPattern.Hole("arg"), TermPattern.Hole("extra"))
    )
    assert(
      RecoveredTwoSelectedNameSourcePattern
        .recover(source, standardPositions, unsupported)
        .isLeft
    )

    val recovered = recover(
      "$receiver.$first.$second",
      standardPositions,
      twoSelectPattern(normalize("$receiver.$first.$second", standardPositions))
    )
    assertEquals(
      recovered.matchTarget(
        TermShape.Select(
          TermShape.Select(identifier("service"), "+"),
          "ordinary"
        )
      ),
      Left(NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported("+"))
    )

  private def recover(
      source: String,
      positions: Vector[SelectedNameSourcePosition],
      compiled: TermPattern
  ): RecoveredTwoSelectedNameSourcePattern =
    RecoveredTwoSelectedNameSourcePattern
      .recover(source, positions, compiled)
      .fold(error => fail(error.message), identity)

  private def normalize(
      source: String,
      positions: Vector[SelectedNameSourcePosition]
  ): NormalizedSelectedNameSource =
    SelectedNameSourceNormalizer
      .normalize(source, positions)
      .fold(error => fail(error.message), identity)

  private def twoSelectPattern(
      normalized: NormalizedSelectedNameSource
  ): TermPattern =
    TermPattern.Select(
      TermPattern.Select(
        TermPattern.Hole("receiver"),
        selectedAt(normalized, 1).transport
      ),
      selectedAt(normalized, 0).transport
    )

  private def selectedAt(
      normalized: NormalizedSelectedNameSource,
      ordinal: Int
  ): SelectedNamePatternOccurrence =
    normalized.selectedOccurrences
      .find(_.selectOrdinal == ordinal)
      .getOrElse(fail(s"missing selected occurrence at ordinal $ordinal"))

  private def rewrittenOccurrences(
      recovered: RecoveredTwoSelectedNameSourcePattern
  ) =
    recovered.sourceMap.segments.flatMap { segment =>
      segment.origin match
        case quasiquotes.source.SourceOrigin.RewrittenHole(_, originalSpan, name, role) =>
          Some(
            quasiquotes.source.HoleOccurrence(
              name,
              recovered.generatedSource.substring(
                segment.generatedSpan.start,
                segment.generatedSpan.end
              ),
              originalSpan,
              segment.generatedSpan,
              role
            )
          )
        case _ => None
    }

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
