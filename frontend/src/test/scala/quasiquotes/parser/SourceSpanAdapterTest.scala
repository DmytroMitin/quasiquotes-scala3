package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

import quasiquotes.source.SourceSpan

class SourceSpanAdapterTest extends munit.FunSuite:
  test("a valid parsed tree span converts to the neutral span model") {
    val parsed = TinyTermParser.parseOrThrow("foo")

    assertEquals(DottySourceSpanAdapter.fromTree(parsed.rawTree), Some(SourceSpan(0, 3)))
  }

  test("an absent or synthetic tree span converts to no neutral span") {
    assertEquals(DottySourceSpanAdapter.fromTree(untpd.EmptyTree), None)
  }

  test("trailing input exposes a structured generated span without changing summary text") {
    val error = TinyTermParser.parse("foo; bar").swap.toOption.get

    assertEquals(error.summary, "Trailing input after parsed expression at offset 3: '; bar'")
    assertEquals(error.diagnostics.map(_.generatedSpan), List(Some(SourceSpan(3, 8))))
  }

  test("syntax diagnostic span absence is explicit") {
    val error = TinyTermParser.parse("").swap.toOption.get

    assert(error.summary.nonEmpty)
    assert(error.diagnostics.nonEmpty)
    assert(error.diagnostics.forall(_.generatedSpan.isEmpty))
  }
