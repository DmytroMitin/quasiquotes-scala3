package quasiquotes.definitions

import quasiquotes.source.SourceSpan

class DefinitionSpanProbeTest extends munit.FunSuite:
  test("parameterless def component spans are exact") {
    val source = "def answer: Int = 42"
    val summary = RawDefinitionProbe.compilationUnit(source).toOption.get.head

    assertEquals(summary.definitionSpan, Some(SourceSpan(0, source.length)))
    assertEquals(summary.nameSpan, Some(SourceSpan(4, 10)))
    assertEquals(summary.typeSpan, Some(SourceSpan(12, 15)))
    assertEquals(summary.bodySpan, Some(SourceSpan(18, 20)))
  }

  test("immutable val component spans are exact") {
    val source = "val answer: Int = 42"
    val summary = RawDefinitionProbe.compilationUnit(source).toOption.get.head

    assertEquals(summary.definitionSpan, Some(SourceSpan(0, source.length)))
    assertEquals(summary.nameSpan, Some(SourceSpan(4, 10)))
    assertEquals(summary.typeSpan, Some(SourceSpan(12, 15)))
    assertEquals(summary.bodySpan, Some(SourceSpan(18, 20)))
  }

  test("backticked name span preserves source spelling") {
    val source = "def `type`: Int = 42"
    val summary = RawDefinitionProbe.compilationUnit(source).toOption.get.head

    assertEquals(summary.name, "type")
    assertEquals(summary.sourceName, "`type`")
    assertEquals(summary.nameSpan, Some(SourceSpan(4, 10)))
  }
