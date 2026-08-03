package quasiquotes.matching

import quasiquotes.source.*

class PatternSourceMappingTest extends munit.FunSuite:
  test("one term-pattern hole retains its original and generated spans") {
    val mapped = PatternSource.synthesizeMapped("foo($x)").toOption.get
    val occurrence = mapped.occurrences.head

    assertEquals(mapped.patternSource.source, "foo(__qqhole_x)")
    assertEquals(occurrence.originalSpan, SourceSpan(4, 6))
    assertEquals(
      mapped.originMap.originsFor(occurrence.generatedSpan).map(_.origin),
      Vector(SourceOrigin.RewrittenHole(SourceId.TermPattern, SourceSpan(4, 6), "x", HoleRole.TermPattern))
    )
  }

  test("repeated term-pattern holes map to distinct original spans in order") {
    val mapped = PatternSource.synthesizeMapped("$x + $x").toOption.get

    assertEquals(mapped.patternSource.holes, Vector("x", "x"))
    assertEquals(mapped.occurrences.map(_.originalSpan), Vector(SourceSpan(0, 2), SourceSpan(5, 7)))
    assertEquals(mapped.occurrences.map(_.generatedSpan).distinct.size, 2)
  }

  test("invalid term-pattern hole syntax retains the current error") {
    assertEquals(
      PatternSource.synthesizeMapped("foo($1)").swap.toOption.map(_.message),
      PatternSource.synthesize("foo($1)").swap.toOption.map(_.message)
    )
  }
