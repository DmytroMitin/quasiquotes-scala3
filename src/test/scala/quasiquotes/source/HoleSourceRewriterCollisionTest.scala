package quasiquotes.source

class HoleSourceRewriterCollisionTest extends munit.FunSuite:
  private def rewrite(
      source: String,
      prefix: String = "__testhole_",
      role: HoleRole = HoleRole.TermPattern,
      originalId: SourceId = SourceId.TermPattern,
      generatedId: SourceId = SourceId.VirtualTermPatternParserInput,
      unicode: Boolean = true
  ): MappedHoleSource =
    HoleSourceRewriter.rewrite(source, prefix, role, originalId, generatedId, unicode)

  test("no collision uses the base generated name") {
    val mapped = rewrite("foo($x)")

    assertEquals(mapped.generatedSource, "foo(__testhole_x)")
    assertEquals(mapped.occurrences.map(_.generatedName), Vector("__testhole_x"))
  }

  test("exact base and suffix collisions advance deterministic numeric suffixes") {
    val baseCollision = rewrite("(__testhole_x, $x)")
    val suffixCollision = rewrite("(__testhole_x, __testhole_x_1, $x)")

    assertEquals(baseCollision.occurrences.map(_.generatedName), Vector("__testhole_x_1"))
    assertEquals(suffixCollision.occurrences.map(_.generatedName), Vector("__testhole_x_2"))
  }

  test("substring-only identifiers and quoted or commented text do not collide") {
    assertEquals(
      rewrite("prefix__testhole_xsuffix + $x").occurrences.map(_.generatedName),
      Vector("__testhole_x")
    )
    assertEquals(
      rewrite("\"__testhole_x\" + $x /* __testhole_x */").occurrences.map(_.generatedName),
      Vector("__testhole_x")
    )
  }

  test("generated names are deterministic and different semantic holes stay distinct") {
    val source = "(__testhole_x, $x, $x_1, $y)"
    val first = rewrite(source)
    val second = rewrite(source)

    assertEquals(first, second)
    assertEquals(first.occurrences.map(_.generatedName), Vector("__testhole_x_1", "__testhole_x_1_1", "__testhole_y"))
    assertEquals(first.occurrences.map(_.generatedName).distinct.size, 3)
  }

  test("term-pattern scanning retains Unicode hole syntax without broadening ASCII type holes") {
    val term = rewrite("(__testhole_\u00e9, $\u00e9)")
    val typePattern = rewrite(
      "(__testhole_\u00e9, $\u00e9)",
      role = HoleRole.TypePattern,
      originalId = SourceId.TypePattern,
      generatedId = SourceId.VirtualTypePatternParserInput,
      unicode = false
    )

    assertEquals(term.occurrences.map(_.generatedName), Vector("__testhole_\u00e9_1"))
    assertEquals(typePattern.occurrences, Vector.empty)
  }

  test("repeated semantic holes reuse one generated name and keep distinct spans") {
    val mapped = rewrite("(__testhole_x, $x + $x)")

    assertEquals(mapped.occurrences.map(_.name), Vector("x", "x"))
    assertEquals(mapped.occurrences.map(_.generatedName), Vector("__testhole_x_1", "__testhole_x_1"))
    assertEquals(mapped.occurrences.map(_.originalSpan), Vector(SourceSpan(15, 17), SourceSpan(20, 22)))
    assertEquals(mapped.occurrences.map(_.generatedSpan).distinct.size, 2)
  }

  test("collision suffixes preserve semantic origins and literal text origins") {
    val mapped = rewrite("(__testhole_x, $x)")
    val occurrence = mapped.occurrences.head
    val holeOrigins = mapped.originMap.originsFor(occurrence.generatedSpan).map(_.origin)

    assertEquals(occurrence.name, "x")
    assertEquals(occurrence.role, HoleRole.TermPattern)
    assertEquals(
      holeOrigins,
      Vector(SourceOrigin.RewrittenHole(SourceId.TermPattern, SourceSpan(15, 17), "x", HoleRole.TermPattern))
    )
    assertEquals(
      mapped.originMap.originAt(1),
      Some(SourceOrigin.OriginalText(SourceId.TermPattern, SourceSpan(0, 15)))
    )
  }

  test("generated-hole index is authoritative and prefix-neutral") {
    val mapped = rewrite("(__testhole_x, $x)")

    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__testhole_x"), None)
    assertEquals(mapped.generatedHoleIndex.semanticNameFor("__testhole_x_1"), Some("x"))
    assertEquals(mapped.generatedHoleIndex.generatedNameFor("x"), Some("__testhole_x_1"))
  }

  test("semantic restoration replaces exact generated identifiers only") {
    val mapped = rewrite("(__testhole_x, __testhole_x_1, $x)")
    val generated = mapped.occurrences.head.generatedName
    val text =
      s"($generated, prefix${generated}suffix, ${generated}_tail, __testhole_x, __testhole_x_1)"

    assertEquals(
      HoleSourceRewriter.restoreSemanticHoleIdentifiers(text, mapped, allowUnicodeIdentifiers = true),
      s"($$x, prefix${generated}suffix, ${generated}_tail, __testhole_x, __testhole_x_1)"
    )
  }

  test("term, type-pattern, and type-template rewrites retain isolated roles and identities") {
    val source = "(__qqhole_x, __tqhole_x, __tqconstructhole_x, $x)"
    val term = rewrite(source, "__qqhole_", HoleRole.TermPattern, SourceId.TermPattern, SourceId.VirtualTermPatternParserInput)
    val pattern = rewrite(source, "__tqhole_", HoleRole.TypePattern, SourceId.TypePattern, SourceId.VirtualTypePatternParserInput, unicode = false)
    val template = rewrite(source, "__tqconstructhole_", HoleRole.TypeTemplate, SourceId.TypeTemplate, SourceId.VirtualTypeTemplateParserInput, unicode = false)

    assertEquals(term.occurrences.head.generatedName, "__qqhole_x_1")
    assertEquals(pattern.occurrences.head.generatedName, "__tqhole_x_1")
    assertEquals(template.occurrences.head.generatedName, "__tqconstructhole_x_1")
    assertEquals(Vector(term, pattern, template).map(_.occurrences.head.role), Vector(HoleRole.TermPattern, HoleRole.TypePattern, HoleRole.TypeTemplate))
    assertEquals(
      Vector(term, pattern, template).map(_.originMap.generatedSourceId),
      Vector(SourceId.VirtualTermPatternParserInput, SourceId.VirtualTypePatternParserInput, SourceId.VirtualTypeTemplateParserInput)
    )
    assertEquals(term.generatedHoleIndex.semanticNameFor(pattern.occurrences.head.generatedName), None)
  }
