package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.construct.SelectedMemberName
import quasiquotes.matching.*
import quasiquotes.parser.{TermShape, TinyTermParser}

final class Q069TwoSelectedNameFeasibilityTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  private val source = "$receiver.$first.$second($argument)"
  private val parts = List("", ".", ".", "(", ")")

  test("public standard qq stops two selected-name source roles at the classifier"):
    val classified = SelectedMemberPatternSource.classifyIncludingRanked(parts)
    assert(classified.left.toOption.exists(_.contains("selected-name capture is supported only")))

    val direct = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    val repeated = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    Vector(direct, unary, repeated).foreach { errors =>
      assert(errors.exists(_.contains("Invalid qq term-pattern template")), clues(errors))
      assert(errors.exists(_.contains("selected-name capture is supported only")), clues(errors))
    }

  test("public source mapping and Dotty parser retain two Select slots without assigning their roles"):
    val mapped = PatternSource.synthesizeMapped(source).toOption.get
    assertEquals(mapped.occurrences.map(_.name), Vector("receiver", "first", "second", "argument"))
    assertEquals(mapped.occurrences.map(_.generatedName),
      Vector("__qqhole_receiver", "__qqhole_first", "__qqhole_second", "__qqhole_argument"))
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__qqhole_first"), Some("first"))

    val raw = TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
    val compiled = PatternCompiler.compileLocated(raw, mapped.generatedHoleIndex).toOption.get
    assertEquals(compiled,
      TermPattern.Apply(
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_first"),
          "__qqhole_second"
        ),
        List(TermPattern.Hole("argument"))
      ))
    assertEquals(QuasiPattern.term(source).toOption.get.pattern, compiled)

  test("private N normalization and matcher distinguish source order from root-first Select ordinals"):
    val normalized = SelectedNameSourceNormalizer.normalize(source,
      Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0)))
      .toOption.get
    assertEquals(normalized.occurrences.map(_.name),
      Vector("receiver", "first", "second", "argument"))
    assertEquals(normalized.selectedOccurrences.map(value => value.name -> value.selectOrdinal),
      Vector("first" -> 1, "second" -> 0))
    assertEquals(normalized.selectedOccurrences.map(_.transport).distinct.size, 2)
    normalized.selectedOccurrences.foreach { occurrence =>
      assertEquals(normalized.ordinaryHoleIndex.semanticNameFor(occurrence.transport), None)
    }

    val raw = TinyTermParser.parseOrThrow(normalized.generatedSource).rawTree
    val pattern = PatternCompiler.compileLocated(raw, normalized.ordinaryHoleIndex).toOption.get
    val validated = ValidatedSelectedNamePattern.createTwo(
      pattern, normalized.selectedOccurrences(0), normalized.selectedOccurrences(1)
    ).toOption.get
    val receiver = TermShape.Identifier("service", isPlaceholder = false)
    val argument = TermShape.Literal("1")
    val target = TermShape.Apply(
      TermShape.Select(TermShape.Select(receiver, "alpha"), "beta"), List(argument)
    )
    val result = SelectedNamePatternMatcher.matchTarget(validated, target).toOption.get
    assertEquals(result.termBindings, Map("receiver" -> receiver, "argument" -> argument))
    assertEquals(result.selectedNameBindings,
      Map("first" -> selected("alpha"), "second" -> selected("beta")))
    assertEquals(normalized.occurrences.map(_.originalSpan.start),
      normalized.occurrences.map(_.originalSpan.start).sorted)

  test("private repeated logical name enforces equality with two occurrence transports"):
    val normalized = SelectedNameSourceNormalizer.normalize(
      "$receiver.$same.$same",
      Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
    ).toOption.get
    assertEquals(normalized.selectedOccurrences.map(_.name), Vector("same", "same"))
    assertEquals(normalized.selectedOccurrences.map(_.transport).distinct.size, 2)
    val raw = TinyTermParser.parseOrThrow(normalized.generatedSource).rawTree
    val pattern = PatternCompiler.compileLocated(raw, normalized.ordinaryHoleIndex).toOption.get
    val validated = ValidatedSelectedNamePattern.createTwo(
      pattern, normalized.selectedOccurrences(0), normalized.selectedOccurrences(1)
    ).toOption.get
    val receiver = TermShape.Identifier("service", isPlaceholder = false)
    def target(inner: String, outer: String): TermShape =
      TermShape.Select(TermShape.Select(receiver, inner), outer)

    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, target("same", "same"))
        .toOption.get.selectedNameBindings,
      Map("same" -> selected("same"))
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, target("inner", "outer")),
      Left(NeutralSelectedNameMatchError.RepeatedSelectedNameCaptureMismatch("same"))
    )

  test("private two-role failures are atomic and keep Term and selected-name categories separate"):
    val base = "$receiver.$first.$second($argument)"
    val positions = Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
    assertEquals(SelectedNameSourceNormalizer.normalize(base,
      Vector(positions(0), positions(0))).left.toOption.get,
      SelectedNameSourceNormalizationError.DuplicateSourceOccurrenceIndex(1))
    assertEquals(SelectedNameSourceNormalizer.normalize("$first.$first.$second",
      positions).left.toOption.get,
      SelectedNameSourceNormalizationError.CaptureCategoryConflict("first"))
    val normalized = SelectedNameSourceNormalizer.normalize(base, positions).toOption.get
    val raw = TinyTermParser.parseOrThrow(normalized.generatedSource).rawTree
    val pattern = PatternCompiler.compileLocated(raw, normalized.ordinaryHoleIndex).toOption.get
    val validated = ValidatedSelectedNamePattern.createTwo(
      pattern, normalized.selectedOccurrences(0), normalized.selectedOccurrences(1)
    ).toOption.get
    assert(ValidatedSelectedNamePattern.createTwo(pattern,
      normalized.selectedOccurrences(0).copy(selectOrdinal = 0),
      normalized.selectedOccurrences(1).copy(selectOrdinal = 1)).isLeft)
    assert(ValidatedSelectedNamePattern.createTwo(pattern,
      normalized.selectedOccurrences(0),
      normalized.selectedOccurrences(1).copy(transport = normalized.selectedOccurrences(0).transport)).isLeft)
    val receiver = TermShape.Identifier("service", isPlaceholder = false)
    val good = TermShape.Select(TermShape.Select(receiver, "alpha"), "beta")
    val wrongReceiver = TermShape.Select(receiver, "beta")
    val wrongName = TermShape.Apply(
      TermShape.Select(TermShape.Select(receiver, "+"), "beta"),
      List(TermShape.Literal("1"))
    )
    val wrongShape = TermShape.Apply(good, Nil)
    Vector(wrongReceiver, wrongName, wrongShape, null).foreach { target =>
      assert(SelectedNamePatternMatcher.matchTarget(validated, target).isLeft)
    }
    assertEquals(SelectedNamePatternMatcher.matchTarget(validated,
      TermShape.Apply(good, List(TermShape.Literal("1")))).toOption.get
        .selectedNameBindings.size, 2)

  test("generated-looking source identifiers do not become selected captures and lexical policies remain distinct"):
    val normalized = SelectedNameSourceNormalizer.normalize(
      "__qqhole_second__selected_0.$first.$second",
      Vector(SelectedNameSourcePosition(0, 1), SelectedNameSourcePosition(1, 0))
    ).toOption.get
    assertEquals(normalized.selectedOccurrences.map(_.name), Vector("first", "second"))
    assertEquals(normalized.selectedOccurrences.map(_.transport).distinct.size, 2)
    assert(normalized.selectedOccurrences.forall(_.transport != "__qqhole_second__selected_0"))
    Vector("+", "type", "safe spaced name").foreach { name =>
      assert(SelectedMemberName.from(name).isRight, clues(name))
    }
    Vector("naïve", "$internal").foreach { name =>
      assert(SelectedMemberName.from(name).isLeft, clues(name))
    }
    Vector("$receiver.+.$second", "$receiver.`type`.$second", "$receiver.naïve.$second")
      .foreach { candidate =>
        assert(SelectedNameSourceNormalizer.normalize(candidate,
          Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))).isLeft)
      }

  test("the existing product carrier can type a four-position heterogeneous tuple without public expansion"):
    assertEquals(Q069TypedCaptureCarrierProbe.observation, "inner:outer:true:true")

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)
