package quasiquotes.matching

import quasiquotes.source.*

class LocatedPatternDiagnosticTest extends munit.FunSuite:
  test("located term-pattern synthesis reports bounded direct original spans") {
    List("$" -> SourceSpan(0, 1), "$1" -> SourceSpan(0, 2), "$-" -> SourceSpan(0, 2)).foreach {
      case (source, expectedSpan) =>
        val located = PatternSource.synthesizeMappedLocated(source).swap.toOption.get
        assertEquals(located.diagnostic, PatternError.InvalidHoleName(source))
        assertEquals(located.location.map(_.generatedSourceId), Some(SourceId.TermPattern))
        assertEquals(located.location.map(_.generatedSpan), Some(expectedSpan))
        assertEquals(
          located.location.map(_.origins),
          Some(Vector(SourceOrigin.OriginalText(SourceId.TermPattern, expectedSpan)))
        )
        assert(located.location.forall(_.generatedSpan.end <= source.length))
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
    assertEquals(located.location.map(_.generatedSourceId), Some(SourceId.VirtualTermPatternParserInput))
    assertEquals(located.location.map(_.generatedSpan.end), Some(source.length))
    assert(located.location.exists(location => location.generatedSpan.start > 0 && !location.generatedSpan.isEmpty))
    assertEquals(
      located.location.map(_.origins),
      Some(Vector(SourceOrigin.OriginalText(SourceId.TermPattern, SourceSpan(0, source.length))))
    )

    assertEquals(QuasiPattern.termLocated("foo bar").swap.toOption.flatMap(_.location), None)
  }

  test("located compiler failures retain the deepest offending tree span") {
    val source = "foo((x => x))"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val span = located.location.map(_.generatedSpan).get

    assert(located.diagnostic.isInstanceOf[PatternError.UnsupportedPatternShape])
    assert(source.slice(span.start, span.end).contains("x => x"))
    assert(span.length < source.length)
    assertEquals(QuasiPattern.term(source).swap.toOption, Some(located.diagnostic))
  }

  test("whole unsupported patterns retain all repeated-hole origins") {
    val source = "($x, $x) match { case _ => 1 }"
    val located = QuasiPattern.termLocated(source).swap.toOption.get
    val repeated = located.location.toVector.flatMap(_.origins).collect {
      case origin: SourceOrigin.RewrittenHole if origin.holeName == "x" => origin.originalSpan
    }

    assertEquals(repeated, Vector(SourceSpan(1, 3), SourceSpan(5, 7)))
    assertEquals(located.location.map(_.generatedSourceId), Some(SourceId.VirtualTermPatternParserInput))
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
