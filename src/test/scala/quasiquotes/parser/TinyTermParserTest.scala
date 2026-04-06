package quasiquotes.parser

class TinyTermParserTest extends munit.FunSuite:
  private val expectedShapes = List(
    "foo" -> "Ident(foo)",
    "1" -> "Literal(1)",
    "\"abc\"" -> "Literal(\"abc\")",
    "foo.bar" -> "Select(Ident(foo), bar)",
    "foo(x)" -> "Apply(Ident(foo), [Ident(x)])",
    "foo.bar(x)" -> "Apply(Select(Ident(foo), bar), [Ident(x)])",
    "__hole0" -> "Placeholder(__hole0)",
    "foo(__hole0)" -> "Apply(Ident(foo), [Placeholder(__hole0)])",
    "foo.bar(__hole0)" -> "Apply(Select(Ident(foo), bar), [Placeholder(__hole0)])",
    "__hole0(__hole1)" -> "Apply(Placeholder(__hole0), [Placeholder(__hole1)])"
  )

  expectedShapes.foreach { (input, expected) =>
    test(s"parse shape for $input") {
      val parsed = TinyTermParser.parseOrThrow(input)
      assertEquals(parsed.shape.render, expected)
    }
  }

  test("raw parse tree remains available for future lowering") {
    val parsed = TinyTermParser.parseOrThrow("foo.bar(__hole0)")
    assertEquals(parsed.rawTree.getClass.getSimpleName.nonEmpty, true)
    assert(clue(parsed.rawStructure).contains("Apply"))
    assert(clue(parsed.rawStructure).contains("__hole0"))
  }

  test("placeholder helper recognizes synthetic holes") {
    assert(Placeholder.isPlaceholder("__hole0"))
    assert(Placeholder.isPlaceholder("__hole12"))
    assert(!Placeholder.isPlaceholder("foo"))
    assert(!Placeholder.isPlaceholder("__hole"))
  }
