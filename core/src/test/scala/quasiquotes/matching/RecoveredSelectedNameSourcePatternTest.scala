package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.TermShape
import quasiquotes.source.{HoleOccurrence, HoleRole, SourceSpan}

final class RecoveredSelectedNameSourcePatternTest extends munit.FunSuite:
  test("recovers direct nullary and unary source roles and delegates matching to N061"):
    val cases = Vector(
      (
        "$receiver.$member",
        (member: HoleOccurrence) =>
          TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName),
        TermShape.Select(identifier("service"), "ordinary"),
        Vector("receiver"),
        Map("receiver" -> identifier("service")),
        Map("member" -> selected("ordinary"))
      ),
      (
        "$receiver.$member()",
        (member: HoleOccurrence) =>
          TermPattern.Apply(
            TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName),
            Nil
          ),
        TermShape.Apply(TermShape.Select(identifier("service"), "nullary"), Nil),
        Vector("receiver"),
        Map("receiver" -> identifier("service")),
        Map("member" -> selected("nullary"))
      ),
      (
        "$receiver.$member($arg)",
        (member: HoleOccurrence) =>
          TermPattern.Apply(
            TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName),
            List(TermPattern.Hole("arg"))
          ),
        TermShape.Apply(
          TermShape.Select(identifier("service"), "unary"),
          List(TermShape.Literal("1"))
        ),
        Vector("receiver", "arg"),
        Map("receiver" -> identifier("service"), "arg" -> TermShape.Literal("1")),
        Map("member" -> selected("unary"))
      )
    )

    cases.foreach {
      case (source, compiledFor, target, ordinaryNames, termBindings, selectedBindings) =>
        val mapped = synthesize(source)
        val member = occurrence(mapped, "member")
        val recovered = recover(mapped, compiledFor(member))

        assert(recovered.selectedOccurrence eq member)
        assert(recovered.sourceMap eq mapped.originMap)
        assertEquals(recovered.ordinaryTermHoleNames, ordinaryNames)
        assertEquals(recovered.validatedPattern.occurrence.name, "member")
        assertEquals(recovered.validatedPattern.occurrence.selectOrdinal, 0)
        assertEquals(
          recovered.matchTarget(target),
          Right(NeutralSelectedNameMatchResult(termBindings, selectedBindings))
        )
    }

  test("recovers the actual root-first Select ordinal instead of assuming zero"):
    val outerMapped = synthesize("$receiver.fixed.$member")
    val outerMember = occurrence(outerMapped, "member")
    val outer = recover(
      outerMapped,
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), "fixed"),
        outerMember.generatedName
      )
    )

    val innerMapped = synthesize("$receiver.$member.fixed")
    val innerMember = occurrence(innerMapped, "member")
    val inner = recover(
      innerMapped,
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), innerMember.generatedName),
        "fixed"
      )
    )

    assertEquals(outer.validatedPattern.occurrence.selectOrdinal, 0)
    assertEquals(inner.validatedPattern.occurrence.selectOrdinal, 1)
    assertEquals(
      inner.matchTarget(
        TermShape.Select(
          TermShape.Select(identifier("service"), "ordinary"),
          "fixed"
        )
      ).toOption.get.selectedNameBinding("member"),
      Some(selected("ordinary"))
    )

  test("preserves repeated ordinary-hole occurrences in source order"):
    val mapped = synthesize("$same.$member($same)")
    val member = occurrence(mapped, "member")
    val recovered = recover(
      mapped,
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("same"), member.generatedName),
        List(TermPattern.Hole("same"))
      )
    )

    assertEquals(recovered.ordinaryTermHoleNames, Vector("same", "same"))
    assertEquals(
      recovered.matchTarget(
        TermShape.Apply(
          TermShape.Select(identifier("service"), "ordinary"),
          List(identifier("service"))
        )
      ).toOption.get.termBindings,
      Map("same" -> identifier("service"))
    )

  test("keeps transport spelling out of semantic selected-name identity"):
    val firstMapped = synthesize("$receiver.$member")
    val firstMember = occurrence(firstMapped, "member")
    val secondMapped = withRenamedTransport(firstMapped, firstMember, "__collision_safe_member_2")
    val secondMember = occurrence(secondMapped, "member")

    val first = recover(
      firstMapped,
      TermPattern.Select(TermPattern.Hole("receiver"), firstMember.generatedName)
    )
    val second = recover(
      secondMapped,
      TermPattern.Select(TermPattern.Hole("receiver"), secondMember.generatedName)
    )

    assertEquals(first.validatedPattern, second.validatedPattern)
    assertNotEquals(first, second)
    assertEquals(
      first.matchTarget(TermShape.Select(identifier("service"), "ordinary")),
      second.matchTarget(TermShape.Select(identifier("service"), "ordinary"))
    )

  test("rejects null and malformed mapped-source inputs fail-closed"):
    val mapped = synthesize("$receiver.$member")
    val member = occurrence(mapped, "member")
    val compiled = TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName)

    assertInvalidOccurrence(RecoveredSelectedNameSourcePattern.recover(null, compiled))
    assertInvalidOccurrence(RecoveredSelectedNameSourcePattern.recover(mapped, null))
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(mapped.copy(patternSource = null), compiled)
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(mapped.copy(originMap = null), compiled)
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(mapped.copy(occurrences = null), compiled)
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(mapped.copy(occurrences = Vector(null)), compiled)
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(
        mapped.copy(occurrences = mapped.occurrences.updated(1, member.copy(name = null))),
        compiled
      )
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(
        mapped.copy(
          occurrences = mapped.occurrences.updated(1, member.copy(generatedName = null))
        ),
        compiled
      )
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(
        mapped.copy(occurrences = mapped.occurrences.updated(1, member.copy(originalSpan = null))),
        compiled
      )
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(
        mapped.copy(occurrences = mapped.occurrences.updated(1, member.copy(generatedSpan = null))),
        compiled
      )
    )
    assertInvalidOccurrence(
      RecoveredSelectedNameSourcePattern.recover(
        mapped.copy(
          occurrences = mapped.occurrences.updated(
            1,
            member.copy(role = HoleRole.TermTemplate)
          )
        ),
        compiled
      )
    )

  test("rejects missing multiple repeated and duplicated selected-name source roles"):
    val missing = synthesize("$receiver.fixed")
    val missingError = recoverLeft(
      missing,
      TermPattern.Select(TermPattern.Hole("receiver"), "fixed")
    )
    assert(missingError.isInstanceOf[SelectedNameSourceRecoveryError.MissingSelectedNameSourceRole])

    val forged = synthesize("$receiver.$member")
    val forgedError = recoverLeft(
      forged,
      TermPattern.Select(TermPattern.Hole("receiver"), "__transport_absent_from_source")
    )
    assert(forgedError.isInstanceOf[SelectedNameSourceRecoveryError.MissingSelectedNameSourceRole])

    val multiple = synthesize("$receiver.$first.$second")
    val first = occurrence(multiple, "first")
    val second = occurrence(multiple, "second")
    val multipleError = recoverLeft(
      multiple,
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), first.generatedName),
        second.generatedName
      )
    )
    val multipleRoles = multipleError.asInstanceOf[
      SelectedNameSourceRecoveryError.MultipleSelectedNameSourceRoles
    ]
    assertEquals(multipleRoles.selectedOccurrences.map(_.name), Vector("first", "second"))

    val single = synthesize("$receiver.$member")
    val singleMember = occurrence(single, "member")
    assertSidecarMismatch(
      RecoveredSelectedNameSourcePattern.recover(
        single,
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("receiver"), singleMember.generatedName),
          singleMember.generatedName
        )
      )
    )

    val duplicated = synthesize("$member.$member")
    val duplicatedMember = occurrence(duplicated, "member")
    val duplicateError = recoverLeft(
      duplicated,
      TermPattern.Select(TermPattern.Hole("member"), duplicatedMember.generatedName)
    ).asInstanceOf[SelectedNameSourceRecoveryError.MultipleSelectedNameSourceRoles]
    assertEquals(
      duplicateError.selectedOccurrences.map(_.originalSpan),
      Vector(SourceSpan(0, 7), SourceSpan(8, 15))
    )

  test("rejects category conflicts argument-only occurrences and stale sidecars"):
    val mapped = synthesize("$receiver.$member")
    val member = occurrence(mapped, "member")
    val categoryConflict = recoverLeft(
      mapped,
      TermPattern.Select(TermPattern.Hole("member"), member.generatedName)
    )
    assertEquals(
      categoryConflict,
      SelectedNameSourceRecoveryError.SelectedNameSourceCategoryConflict(
        "member",
        member,
        Vector.empty
      )
    )

    val argumentOnly = synthesize("$receiver.fixed($member)")
    val argumentMember = occurrence(argumentOnly, "member")
    val argumentError = recoverLeft(
      argumentOnly,
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), "fixed"),
        List(TermPattern.Hole("member"))
      )
    ).asInstanceOf[SelectedNameSourceRecoveryError.MissingSelectedNameSourceRole]
    assertEquals(
      argumentError.sourceOccurrences.find(_.name == "member").map(_.originalSpan),
      Some(argumentMember.originalSpan)
    )

    assertSidecarMismatch(
      RecoveredSelectedNameSourcePattern.recover(
        mapped,
        TermPattern.Select(TermPattern.Hole("stale"), member.generatedName)
      )
    )

    val duplicatedTransport = synthesize("$member.$member")
    val originalOccurrences = duplicatedTransport.occurrences
    val conflictingOccurrence = originalOccurrences(1).copy(name = "other")
    val conflictingSegments = duplicatedTransport.originMap.segments.map { segment =>
      if segment.generatedSpan == conflictingOccurrence.generatedSpan then
        segment.copy(
          origin = quasiquotes.source.SourceOrigin.RewrittenHole(
            quasiquotes.source.SourceId.TermPattern,
            conflictingOccurrence.originalSpan,
            conflictingOccurrence.name,
            conflictingOccurrence.role
          )
        )
      else segment
    }
    val conflictingIndex = duplicatedTransport.copy(
      patternSource = duplicatedTransport.patternSource.copy(holes = Vector("member", "other")),
      originMap = duplicatedTransport.originMap.copy(segments = conflictingSegments),
      occurrences = Vector(originalOccurrences.head, conflictingOccurrence)
    )
    assertSidecarMismatch(
      RecoveredSelectedNameSourcePattern.recover(
        conflictingIndex,
        TermPattern.Select(
          TermPattern.Hole("member"),
          originalOccurrences.head.generatedName
        )
      )
    )

  test("wraps N061 validation and matching errors without losing selected source evidence"):
    val mapped = synthesize("$receiver.$member")
    val member = occurrence(mapped, "member")
    val unsupported = RecoveredSelectedNameSourcePattern.recover(
      mapped,
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName),
        List(TermPattern.Hole("first"), TermPattern.Hole("second"))
      )
    )
    val unsupportedError = unsupported.left.toOption.get.asInstanceOf[
      SelectedNameSourceRecoveryError.UnderlyingSelectedNamePatternError
    ]
    assert(unsupportedError.error.isInstanceOf[NeutralSelectedNameMatchError.UnsupportedPatternShape])
    assert(unsupportedError.selectedOccurrence eq member)

    val recovered = recover(
      mapped,
      TermPattern.Select(TermPattern.Hole("receiver"), member.generatedName)
    )
    val lexicalError = recovered
      .matchTarget(TermShape.Select(identifier("service"), "+"))
      .left
      .toOption
      .get
    assert(lexicalError.isInstanceOf[NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported])
    assertEquals(recovered.selectedOccurrence.originalSpan, SourceSpan(10, 17))

  private def synthesize(source: String): MappedPatternSource =
    PatternSource.synthesizeMapped(source).fold(error => fail(error.message), identity)

  private def occurrence(mapped: MappedPatternSource, name: String): HoleOccurrence =
    mapped.occurrences.find(_.name == name).getOrElse(fail(s"missing occurrence $name"))

  private def recover(
      mapped: MappedPatternSource,
      compiled: TermPattern
  ): RecoveredSelectedNameSourcePattern =
    RecoveredSelectedNameSourcePattern
      .recover(mapped, compiled)
      .fold(error => fail(error.message), identity)

  private def recoverLeft(
      mapped: MappedPatternSource,
      compiled: TermPattern
  ): SelectedNameSourceRecoveryError =
    RecoveredSelectedNameSourcePattern
      .recover(mapped, compiled)
      .left
      .getOrElse(fail("expected source-role recovery failure"))

  private def assertInvalidOccurrence(
      result: Either[SelectedNameSourceRecoveryError, RecoveredSelectedNameSourcePattern]
  ): Unit =
    assert(
      result.left.toOption.exists(
        _.isInstanceOf[SelectedNameSourceRecoveryError.InvalidSelectedNameSourceOccurrence]
      )
    )

  private def assertSidecarMismatch(
      result: Either[SelectedNameSourceRecoveryError, RecoveredSelectedNameSourcePattern]
  ): Unit =
    assert(
      result.left.toOption.exists(
        _.isInstanceOf[SelectedNameSourceRecoveryError.SourcePatternSidecarMismatch]
      )
    )

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)

  private def withRenamedTransport(
      mapped: MappedPatternSource,
      selectedOccurrence: HoleOccurrence,
      replacement: String
  ): MappedPatternSource =
    val source = mapped.patternSource.source.replace(selectedOccurrence.generatedName, replacement)
    val delta = replacement.length - selectedOccurrence.generatedName.length
    val renamed = selectedOccurrence.copy(
      generatedName = replacement,
      generatedSpan = SourceSpan(
        selectedOccurrence.generatedSpan.start,
        selectedOccurrence.generatedSpan.end + delta
      )
    )
    val receiver = mapped.occurrences.head
    val shiftedSegments = mapped.originMap.segments.map { segment =>
      if segment.generatedSpan == selectedOccurrence.generatedSpan then
        segment.copy(generatedSpan = renamed.generatedSpan)
      else segment
    }
    mapped.copy(
      patternSource = mapped.patternSource.copy(source = source),
      originMap = mapped.originMap.copy(generatedSource = source, segments = shiftedSegments),
      occurrences = Vector(receiver, renamed)
    )
