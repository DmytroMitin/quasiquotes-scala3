package quasiquotes.phase116

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TinyTermParser}

class P2LocalValParserTest extends munit.FunSuite:
  private def parsed(source: String): TermShape =
    TinyTermParser.parse(source).fold(error => fail(error.summary), _.shape)

  test("raw parser represents one explicitly typed immutable local val with a bound result reference") {
    assertEquals(
      parsed("{ val x: Int = 1; x }"),
      TermShape.Block(
        List(
          BlockStatement.LocalVal(
            BinderId(0),
            "x",
            "Int",
            TermShape.Literal("1")
          )
        ),
        TermShape.BoundReference(BinderId(0), "x")
      )
    )
  }

  test("initializer is inspected outside the local binder scope while the result is inside it") {
    assertEquals(
      parsed("{ val x: Int = x; x }"),
      TermShape.Block(
        List(
          BlockStatement.LocalVal(
            BinderId(0),
            "x",
            "Int",
            TermShape.Identifier("x", false)
          )
        ),
        TermShape.BoundReference(BinderId(0), "x")
      )
    )
  }

  test("unsupported local-value neighbors retain distinct block diagnostics") {
    val cases = List(
      "{ val x = 1; x }" -> "explicit",
      "{ var x: Int = 1; x }" -> "var",
      "{ lazy val x: Int = 1; x }" -> "lazy",
      "{ val (x, y): (Int, Int) = (1, 2); x }" -> "pattern",
      "{ val x: Int = 1; val y: Int = 2; y }" -> "exactly one",
      "{ def x: Int = 1; x }" -> "local def"
    )

    cases.foreach { case (source, expected) =>
      parsed(source) match
        case TermShape.Unsupported("Block", detail) =>
          assert(detail.toLowerCase.contains(expected), clues(source, detail))
        case other => fail(s"expected controlled block rejection for $source, obtained ${other.render}")
    }
  }
