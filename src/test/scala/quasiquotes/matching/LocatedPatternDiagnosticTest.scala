package quasiquotes.matching

import quasiquotes.source.*

class LocatedPatternDiagnosticTest extends munit.FunSuite:
  test("located term-pattern synthesis reports bounded direct original spans") {
    List("$" -> SourceSpan(0, 1), "$1" -> SourceSpan(0, 2), "$-" -> SourceSpan(0, 2)).foreach {
      case (source, expectedSpan) =>
        val located = PatternSource.synthesizeMappedLocated(source).swap.toOption.get
        assertEquals(located.diagnostic, PatternError.InvalidHoleName(source))
        assertEquals(located.location.map(_.sourceId), Some(SourceId.TermPattern))
        assertEquals(located.location.map(_.span), Some(expectedSpan))
        assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
        assertEquals(
          located.location.map(_.origins),
          Some(Vector(SourceOrigin.OriginalText(SourceId.TermPattern, expectedSpan)))
        )
        assert(located.location.forall(_.span.end <= source.length))
    }
  }

  test("located term-pattern success is identical to the legacy projection") {
    val located = QuasiPattern.termLocated("foo($x)")
    val legacy = QuasiPattern.term("foo($x)")

    assertEquals(located.left.map(_.diagnostic), legacy)
    assertEquals(located.toOption.map(_.pattern.render), Some("Apply(Ident(foo), [Hole($x)])"))
  }

  test("located term-pattern trailing-input errors use the first structured parser span") {
    val source = "foo; bar"
    val located = QuasiPattern.termLocated(source).swap.toOption.get

    assert(located.diagnostic.isInstanceOf[PatternError.ParseFailure])
    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTermPatternParserInput))
    assertEquals(located.location.map(_.span.end), Some(source.length))
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(located.location.exists(location => location.span.start > 0 && !location.span.isEmpty))
    assertEquals(
      located.location.map(_.origins),
      Some(Vector(SourceOrigin.OriginalText(SourceId.TermPattern, SourceSpan(0, source.length))))
    )

    assertEquals(QuasiPattern.termLocated("foo bar").swap.toOption.flatMap(_.location), None)
  }

  test("located compiler failures retain the deepest offending tree span") {
    val source = "foo((x => x))"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val span = located.location.map(_.span).get

    assert(located.diagnostic.isInstanceOf[PatternError.UnsupportedPatternShape])
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(source.slice(span.start, span.end).contains("x => x"))
    assert(span.length < source.length)
    assertEquals(QuasiPattern.term(source).swap.toOption, Some(located.diagnostic))
  }

  test("malformed unary syntax preserves located and legacy parse diagnostics") {
    val source = "-("
    val located = QuasiPattern.termLocated(source).swap.toOption.get

    assert(located.diagnostic.isInstanceOf[PatternError.ParseFailure])
    assertEquals(QuasiPattern.term(source).swap.toOption, Some(located.diagnostic))
    assert(located.location.forall(_.precision == DiagnosticPrecision.ExactOccurrence))
  }

  test("unsupported unary operands point at the deepest offending subtree") {
    val source = "-({ val x = 1; x })"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val span = located.location.map(_.span).get

    assert(located.diagnostic.isInstanceOf[PatternError.UnsupportedPatternShape])
    assertEquals(located.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(source.slice(span.start, span.end).contains("val x"))
    assert(span.length < source.length)
    assertEquals(QuasiPattern.term(source).swap.toOption, Some(located.diagnostic))
  }

  test("unary holes retain one and repeated original origins") {
    val one = PatternSource.synthesizeMapped("-$x").toOption.get
    val repeated = PatternSource.synthesizeMapped("(-$x, +$x)").toOption.get

    assertEquals(one.occurrences.map(_.originalSpan), Vector(SourceSpan(1, 3)))
    assertEquals(
      repeated.occurrences.map(_.originalSpan),
      Vector(SourceSpan(2, 4), SourceSpan(7, 9))
    )
  }

  test("unary pattern compilation remains collision-safe beside literal generated-prefix identifiers") {
    val pattern = QuasiPattern.termOrThrow("(__qqhole_x, -$x)")

    assertEquals(pattern.placeholderSource, "(__qqhole_x, -__qqhole_x_1)")
    assertEquals(
      pattern.pattern.render,
      "Tuple([Ident(__qqhole_x), Unary(-, Hole($x))])"
    )
  }

  test("whole unsupported patterns retain all repeated-hole origins") {
    val source = "($x, $x) match { case _ => 1 }"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val repeated = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole if origin.holeName == "x" => origin.originalSpan
    }

    assertEquals(repeated, Vector(SourceSpan(1, 3), SourceSpan(5, 7)))
    assertEquals(located.location.map(_.sourceId), Some(SourceId.VirtualTermPatternParserInput))
  }

  test("located integration preserves unsupported binder, control-flow, and prefix-collision boundaries") {
    List("case ($x, $y) => $x", "while cond do a", "for x <- xs yield x").foreach { source =>
      assert(QuasiPattern.termLocated(source).isLeft)
      assertEquals(
        QuasiPattern.termLocated(source).left.map(_.diagnostic),
        QuasiPattern.term(source)
      )
    }
    assert(QuasiPattern.termLocated("__qqhole_x").isRight)
  }

  test("quoted and commented dollar text is literal under the shared lexical policy") {
    assertEquals(
      QuasiPattern.termOrThrow("\"$1\"").pattern,
      TermPattern.Literal("\"$1\"")
    )
    assertEquals(
      QuasiPattern.termOrThrow("\"$x\"").pattern,
      TermPattern.Literal("\"$x\"")
    )
    assertEquals(
      QuasiPattern.termOrThrow("'$'").pattern,
      TermPattern.Literal("$")
    )
    assertEquals(
      QuasiPattern.termOrThrow("foo /* $1 */").pattern,
      TermPattern.Identifier("foo")
    )
    assertEquals(
      QuasiPattern.termOrThrow("foo /* outer /* $1 */ inner */").pattern,
      TermPattern.Identifier("foo")
    )
    assertEquals(
      QuasiPattern.termOrThrow("foo // $-\n").pattern,
      TermPattern.Identifier("foo")
    )
  }

  test("quoted valid hole spelling is not semantic while code holes remain semantic") {
    val quoted = PatternSource.synthesizeMapped("\"$x\"").toOption.get
    val code = PatternSource.synthesizeMapped("$x").toOption.get

    assertEquals(quoted.occurrences, Vector.empty)
    assertEquals(quoted.patternSource.holes, Vector.empty)
    assertEquals(code.occurrences.map(_.name), Vector("x"))
    assertEquals(QuasiPattern.termOrThrow("$x").pattern, TermPattern.Hole("x"))
  }
