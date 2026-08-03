package quasiquotes.matching

import quasiquotes.parser.TinyTermParser
import quasiquotes.source.*

private object CollisionSafeMatchScope:
  private val __qqhole_x = 1
  private val captured = 2
  private val different = 3

  val mixedSuccess: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(__qqhole_x, $x)", (__qqhole_x, captured))
  val literalMismatch: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(__qqhole_x, $x)", (different, captured))
  val repeatedSuccess: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(__qqhole_x, $x, $x)", (__qqhole_x, captured, captured))
  val repeatedFailure: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(__qqhole_x, $x, $x)", (__qqhole_x, captured, different))

class CollisionSafePatternTest extends munit.FunSuite:
  test("literal prefix-looking identifier stays literal and a source hole stays semantic") {
    assertEquals(QuasiPattern.termOrThrow("__qqhole_x").pattern, TermPattern.Identifier("__qqhole_x"))
    assertEquals(QuasiPattern.termOrThrow("$x").pattern, TermPattern.Hole("x"))
  }

  test("mixed literal and hole receive distinct parser identifiers and patterns") {
    val pattern = QuasiPattern.termOrThrow("(__qqhole_x, $x)")

    assertEquals(pattern.placeholderSource, "(__qqhole_x, __qqhole_x_1)")
    assertEquals(pattern.pattern.render, "Tuple([Ident(__qqhole_x), Hole($x)])")
  }

  test("an existing suffix forces the next deterministic suffix") {
    val mapped = PatternSource.synthesizeMapped("(__qqhole_x, __qqhole_x_1, $x)").toOption.get

    assertEquals(mapped.occurrences.map(_.generatedName), Vector("__qqhole_x_2"))
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__qqhole_x_2"), Some("x"))
  }

  test("repeated term holes reuse one generated identifier and one semantic name") {
    val mapped = PatternSource.synthesizeMapped("(__qqhole_x, $x, $x)").toOption.get
    val pattern = QuasiPattern.termOrThrow("(__qqhole_x, $x, $x)")

    assertEquals(mapped.occurrences.map(_.generatedName), Vector("__qqhole_x_1", "__qqhole_x_1"))
    assertEquals(mapped.patternSource.holes, Vector("x", "x"))
    assertEquals(pattern.pattern.render, "Tuple([Ident(__qqhole_x), Hole($x), Hole($x)])")
  }

  test("mixed literal and hole matching preserves literal requirements and capture") {
    assert(CollisionSafeMatchScope.mixedSuccess.success)
    assert(CollisionSafeMatchScope.mixedSuccess.bindings.exists(_.startsWith("$x = ")))
    assert(!CollisionSafeMatchScope.literalMismatch.success)
  }

  test("collision-safe repeated holes retain normalized equality behavior") {
    assert(CollisionSafeMatchScope.repeatedSuccess.success)
    assert(!CollisionSafeMatchScope.repeatedFailure.success)
    assert(CollisionSafeMatchScope.repeatedFailure.detail.contains("Repeated hole"))
  }

  test("located compiler failures preserve literal and rewritten origins after renaming") {
    val source = "(__qqhole_x, $x) match { case _ => 1 }"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val origins = located.location.toVector.flatMap(_.origins)

    assert(origins.exists {
      case SourceOrigin.OriginalText(SourceId.TermPattern, span) => span.contains(1)
      case _ => false
    })
    assert(origins.exists {
      case SourceOrigin.RewrittenHole(SourceId.TermPattern, SourceSpan(13, 15), "x", HoleRole.TermPattern) => true
      case _ => false
    })
    assertEquals(QuasiPattern.term(source).swap.toOption, Some(located.diagnostic))
  }

  test("invalid-hole diagnostics and legacy projections remain unchanged") {
    val invalid = PatternSource.synthesizeMappedLocated("foo($1)").swap.toOption.get
    assertEquals(invalid.diagnostic.message, "Invalid pattern hole name: $1")
    assertEquals(invalid.location.map(_.span), Some(SourceSpan(4, 6)))

    val source = "(__qqhole_x, $x)"
    assertEquals(QuasiPattern.termLocated(source).left.map(_.diagnostic), QuasiPattern.term(source))
  }

  test("direct low-level compiler retains documented prefix compatibility") {
    val tree = TinyTermParser.parseOrThrow("__qqhole_x").rawTree

    assertEquals(PatternCompiler.compile(tree), Right(TermPattern.Hole("x")))
    assertEquals(QuasiPattern.term("__qqhole_x").map(_.pattern), Right(TermPattern.Identifier("__qqhole_x")))
  }
