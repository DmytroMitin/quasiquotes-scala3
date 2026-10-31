package quasiquotes.matching

import quasiquotes.source.{SourceOrigin, SourceSpan}

final class Q083RankedSourceTest extends munit.FunSuite:
  private val source = "$receiver.$first.$second(..$arguments)"
  private def plan = TwoSelectedRankedPatternSource.planSource(source).fold(fail(_), identity)

  test("original source carries rank marker, argument spans and compiler-validated source roles together"):
    val p = plan
    assertEquals(p.mapped.occurrences.map(o => source.slice(o.originalSpan.start, o.originalSpan.end)),
      Vector("$receiver", "$first", "$second", "$arguments"))
    assertEquals(p.mapped.occurrences.map(_.originalSpan.start), Vector(0, 10, 17, 27))
    assertEquals(p.rankMarkerSpan, SourceSpan(25, 27))
    assertEquals(source.slice(p.rankMarkerSpan.start, p.rankMarkerSpan.end), "..")
    assertEquals(p.selectedOccurrences.map(o => o.name -> o.selectOrdinal), Vector("first" -> 1, "second" -> 0))
    assertEquals(QuasiPattern.term(p.effectiveSource).map(_.pattern), Right(p.pattern))
    assertEquals(p.pattern, TermPattern.Apply(TermPattern.Select(TermPattern.Select(
      TermPattern.Hole("receiver"), p.selectedOccurrences(0).transport), p.selectedOccurrences(1).transport),
      List(TermPattern.Hole("arguments"))))
    p.selectedOccurrences.foreach(o => assert(!p.mapped.generatedHoleIndex.generatedNames(o.transport)))

  test("candidate-first classification preserves unrelated generated routes without reading source"):
    List(List("", " + ", ""), List("", ".", ""), List("", ".", "()"),
      List("", ".", ".", ""), List("", ".", ".", "(", ")"), List("", ".", ".", ".", ""),
      List("", "(..", ")"), List("", ".", "(..", ")"),
      List("{ def id(x: Int): Int = x; id(", ") }")).foreach(parts =>
        assertEquals(TwoSelectedRankedPatternSource.classify(parts, fail("original source requested")), Right(None)))

  test("actual mixed generated NoSpan source fails with a categorized missing-source diagnostic"):
    val result = Q080NoSpanProbe.route(TwoSelectedRankedPatternSource.Parts)
    assert(result.startsWith("source=none;static=true;"), result)
    assert(result.contains("two-selected-name rank-2 source-role planning requires recoverable original interpolation source"), result)
    assert(!result.contains("end of NoSpan") && !result.contains("start of NoSpan"), result)

  test("unrelated actual generated NoSpan sources retain scalar, ranked, one-name and P3 routing"):
    List(List("", " + ", ""), List("", "(..", ")"), List("", ".", ""),
      List("", ".", "(..", ")"), List("{ def id(x: Int): Int = x; id(", ") }")).foreach(parts =>
        assertEquals(Q080NoSpanProbe.route(parts), "source=none;static=true;legacy-routed"))
    assert(Q080NoSpanProbe.route(List("", ".", ".", "")).contains("two-selected-name source-role planning"))
    assert(Q080NoSpanProbe.route(List("", ".", ".", ".", "")).contains("selected-chain source-role planning"))

  test("malformed original ranked literals and source category conflicts fail closed"):
    List(null, "$receiver.$first.$second($arguments)", "$receiver.$first.$second(...$arguments)",
      "$receiver.$first.$second(.. $arguments)", "$receiver..$first.$second(..$arguments)",
      "$receiver.$first.$second(..$arguments)()", "$receiver.$first.$second()(..$arguments)",
      "prefix.$receiver.$first.$second(..$arguments)", "$receiver.$first.$second(..$arguments, 1)",
      "$receiver.$first.$first(..$arguments)", "$first.$first.$second(..$arguments)",
      "$receiver.$first.$second(..$receiver)", "$receiver.$naïve.$second(..$arguments)").foreach(s =>
        assert(TwoSelectedRankedPatternSource.planSource(s).isLeft, String.valueOf(s)))

  test("ordinary receiver and sequence source binders retain Unicode hole spelling"):
    assert(TwoSelectedRankedPatternSource.planSource("$naïve.$first.$second(..$arguments)").isRight)
    assert(TwoSelectedRankedPatternSource.planSource("$receiver.$first.$second(..$naïve)").isRight)

  test("ordinary transport roles retain original Unicode semantics and exact spans"):
    val original = "$réceiver.$first.$second(..$ärguments)"
    val p = TwoSelectedRankedPatternSource.planSource(original).fold(fail(_), identity)
    assertEquals(p.mapped.occurrences.map(_.name), Vector("réceiver", "first", "second", "ärguments"))
    assertEquals(p.mapped.occurrences.map(o => original.slice(o.originalSpan.start, o.originalSpan.end)),
      Vector("$réceiver", "$first", "$second", "$ärguments"))
    assert(quasiquotes.definitions.DefinitionName.plain(p.receiverName).isRight)
    assert(quasiquotes.definitions.DefinitionName.plain(p.sequenceName).isRight)
    assert(!p.mapped.occurrences.exists(o => o.name == p.receiverName || o.name == p.sequenceName))
    assert(!p.selectedOccurrences.exists(o => o.transport == p.receiverName || o.transport == p.sequenceName))
    assertEquals(p.pattern, TermPattern.Apply(TermPattern.Select(TermPattern.Select(
      TermPattern.Hole(p.receiverName), p.selectedOccurrences(0).transport), p.selectedOccurrences(1).transport),
      List(TermPattern.Hole(p.sequenceName))))

  test("forged original and generated span maps cannot validate a rank-erased or shifted source"):
    val p = plan
    val arguments = p.mapped.occurrences(3)
    val shifted = p.mapped.copy(occurrences = p.mapped.occurrences.updated(3,
      arguments.copy(originalSpan = SourceSpan(arguments.originalSpan.start - 2, arguments.originalSpan.end - 2))))
    assert(TwoSelectedRankedPatternSource.validateOriginal(source, shifted).isLeft)
    val generated = p.mapped.copy(occurrences = p.mapped.occurrences.updated(3,
      arguments.copy(generatedSpan = SourceSpan(0, 1))))
    assert(TwoSelectedRankedPatternSource.validateOriginal(source, generated).isLeft)
    val stripped = p.mapped.copy(originMap = p.mapped.originMap.copy(segments =
      p.mapped.originMap.segments.filterNot(_.origin.isInstanceOf[SourceOrigin.OriginalText])))
    assert(TwoSelectedRankedPatternSource.validateOriginal(source, stripped).isLeft)
    val noClosingMap = p.mapped.copy(originMap = p.mapped.originMap.copy(segments = p.mapped.originMap.segments.dropRight(1)))
    assert(TwoSelectedRankedPatternSource.validateOriginal(source, noClosingMap).isLeft)

  test("transport allocation is disjoint from all original binders and placeholders"):
    val p = TwoSelectedRankedPatternSource.planSource(
      "$receiver.$first.$__qqhole_first__selected_rank2_1(..$__qqhole_first__selected_rank2_1_1)").fold(fail(_), identity)
    val allBinders = p.mapped.occurrences.map(_.name)
    assert(p.selectedOccurrences.forall(o => !allBinders.contains(o.transport)))
    assertEquals(p.selectedOccurrences.map(_.transport).distinct.size, 2)

  test("compiled category, selected ordinal and sequence cardinality validation fail atomically"):
    val p = plan
    def invalid(pattern: TermPattern = p.pattern, selected: Vector[SelectedNamePatternOccurrence] = p.selectedOccurrences) =
      assert(TwoSelectedRankedPatternSource.validateCompiled(pattern, p.receiverName, p.sequenceName, selected).isLeft)
    invalid(selected = p.selectedOccurrences.reverse)
    invalid(selected = p.selectedOccurrences.updated(1, p.selectedOccurrences(1).copy(transport = p.selectedOccurrences(0).transport)))
    invalid(selected = p.selectedOccurrences.updated(1, p.selectedOccurrences(1).copy(transport = p.sequenceName)))
    invalid(selected = p.selectedOccurrences.take(1))
    invalid(pattern = TermPattern.Hole(p.receiverName))
    invalid(pattern = TermPattern.Apply(p.pattern, List(TermPattern.Hole(p.sequenceName))))
