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
    assert(TypePattern.fromSource("List[$t]").isRight)
    assert(TypeTemplate.fromSource("List[$t]").isRight)
    assert(TypePattern.fromSource("scala.Int").left.exists(_.message.contains("Selected type syntax is not supported")))
    assert(TypeTemplate.fromSource("scala.Int").left.exists(_.message.contains("Selected type syntax is not supported")))
  }

  test("mapped type rewriting preserves the former ASCII hole-name boundary") {
    assert(TypePattern.fromSource("$\u00e9").isLeft)
    assert(TypeTemplate.fromSource("$\u00e9").isLeft)
  }
