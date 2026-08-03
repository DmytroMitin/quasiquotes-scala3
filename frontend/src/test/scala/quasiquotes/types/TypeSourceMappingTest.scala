package quasiquotes.types

import quasiquotes.source.*

class TypeSourceMappingTest extends munit.FunSuite:
  test("simple and applied type-pattern holes retain source origins") {
    val simple = TypePattern.rewriteSourceMapped("$t")
    val applied = TypePattern.rewriteSourceMapped("List[$t]")

    assertEquals(simple.generatedSource, "__tqhole_t")
    assertEquals(simple.occurrences.head.originalSpan, SourceSpan(0, 2))
    assertEquals(applied.generatedSource, "List[__tqhole_t]")
    assertEquals(applied.occurrences.head.originalSpan, SourceSpan(5, 7))
  }

  test("repeated type-pattern holes retain distinct occurrences") {
    val mapped = TypePattern.rewriteSourceMapped("($t, $t)")

    assertEquals(mapped.occurrences.map(_.originalSpan), Vector(SourceSpan(1, 3), SourceSpan(5, 7)))
    assertEquals(mapped.occurrences.map(_.role), Vector(HoleRole.TypePattern, HoleRole.TypePattern))
  }

  test("Tuple3 and Function2 holes retain ordered source occurrences") {
    val tuple = TypeTemplate.rewriteSourceMapped("($a, $b, $c)")
    val function = TypePattern.rewriteSourceMapped("($a, $b) => $r")

    assertEquals(tuple.occurrences.map(_.name), Vector("a", "b", "c"))
    assertEquals(
      tuple.occurrences.map(_.originalSpan),
      Vector(SourceSpan(1, 3), SourceSpan(5, 7), SourceSpan(9, 11))
    )
    assertEquals(function.occurrences.map(_.name), Vector("a", "b", "r"))
    assertEquals(
      function.occurrences.map(_.originalSpan),
      Vector(SourceSpan(1, 3), SourceSpan(5, 7), SourceSpan(12, 14))
    )
  }

  test("type pattern and construction template use distinct source identities and roles") {
    val pattern = TypePattern.rewriteSourceMapped("List[$t]")
    val template = TypeTemplate.rewriteSourceMapped("List[$t]")
    val simpleTemplate = TypeTemplate.rewriteSourceMapped("$t")
    val repeatedTemplate = TypeTemplate.rewriteSourceMapped("($t, $t)")

    assertEquals(pattern.originMap.generatedSourceId, SourceId.VirtualTypePatternParserInput)
    assertEquals(template.originMap.generatedSourceId, SourceId.VirtualTypeTemplateParserInput)
    assertEquals(pattern.occurrences.head.role, HoleRole.TypePattern)
    assertEquals(template.occurrences.head.role, HoleRole.TypeTemplate)
    assertEquals(simpleTemplate.occurrences.head.originalSpan, SourceSpan(0, 2))
    assertEquals(repeatedTemplate.occurrences.map(_.originalSpan), Vector(SourceSpan(1, 3), SourceSpan(5, 7)))
  }

  test("mapped rewriting preserves existing supported and unsupported behavior") {
    assert(TypePatternSource.fromSource("List[$t]").isRight)
    assert(TypeTemplateSource.fromSource("List[$t]").isRight)
    assert(TypePatternSource.fromSource("scala.Int").left.exists(_.message.contains("Selected type syntax is not supported")))
    assert(TypeTemplateSource.fromSource("scala.Int").left.exists(_.message.contains("Selected type syntax is not supported")))
  }

  test("mapped type rewriting preserves the former ASCII hole-name boundary") {
    assert(TypePatternSource.fromSource("$\u00e9").isLeft)
    assert(TypeTemplateSource.fromSource("$\u00e9").isLeft)
  }

  test("mapped type-pattern processing retains one parsed result and one source map") {
    val source = "List[$t]"
    val result = TypePatternSource.fromSourceWithMappingLocated(source).toOption.get

    assertEquals(result.pattern, TypePatternSource.fromSource(source).toOption.get)
    assertEquals(result.parsedType.source, result.mappedSource.generatedSource)
    assertEquals(result.mappedSource.occurrences.map(_.name), Vector("t"))
    assertEquals(
      result.mappedSource.originMap.generatedSourceId,
      SourceId.VirtualTypePatternParserInput
    )
  }

  test("mapped type-template processing retains the parsing map consumed by construction") {
    val source = "List[$t]"
    val result = TypeTemplateSource.fromSourceWithMappingLocated(source).toOption.get
    val constructed = QuasiTypeConstruct.fromTemplateLocated(source, "t" -> TypeNormalForm.STypeIdent("Int"))

    assertEquals(result.template, TypeTemplateSource.fromSource(source).toOption.get)
    assertEquals(result.mappedSource.occurrences.map(_.name), Vector("t"))
    assertEquals(constructed.toOption.map(_.source), Some("List[Int]"))
  }
