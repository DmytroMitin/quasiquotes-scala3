package quasiquotes.phase132

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TinyTermParser, TypeShape}

class SourceOwnedLocalDefParserTest extends munit.FunSuite:
  test("parser assigns distinct method and parameter binders and resolves both uses by identity") {
    val parsed = TinyTermParser
      .parse(
        "{ def boundedIdentity(value: __qq_reflected_type_hole_0): __qq_reflected_type_hole_1 = value; boundedIdentity(__qq_term_hole_2) }"
      )
      .fold(error => fail(error.summary), _.shape)

    assertEquals(
      parsed,
      TermShape.Block(
        List(
          BlockStatement.LocalDef(
            methodBinderId = BinderId(0),
            methodDisplayName = "boundedIdentity",
            parameterBinderId = BinderId(1),
            parameterDisplayName = "value",
            parameterType = TypeShape.Identifier("__qq_reflected_type_hole_0"),
            resultType = TypeShape.Identifier("__qq_reflected_type_hole_1"),
            body = TermShape.BoundReference(BinderId(1), "value")
          )
        ),
        TermShape.Apply(
          TermShape.BoundReference(BinderId(0), "boundedIdentity"),
          List(TermShape.Identifier("__qq_term_hole_2", isPlaceholder = false))
        )
      )
    )
  }

  test("qq programmatic matching keeps local definitions construction-only") {
    val result = quasiquotes.matching.QuasiPattern.term(
      "{ def boundedIdentity(value: Int): Int = value; boundedIdentity(1) }"
    )
    assert(result.isLeft, result)
  }

  test("admission rejects a second, nested, or mixed source-owned statement topology") {
    val cases = List(
      "{ def first(value: Int): Int = value; def second(value: Int): Int = value; first(1) }",
      "{ def first(value: Int): Int = value; { def second(value: Int): Int = value; second(1) } }",
      "{ val seed: Int = 1; def first(value: Int): Int = value; first(seed) }"
    )

    cases.foreach { source =>
      val shape = TinyTermParser.parse(source).fold(error => fail(error.summary), _.shape)
      shape match
        case TermShape.Unsupported("Block", detail) =>
          assert(detail.contains("exactly one local method"), clues(source, detail))
        case other => fail(s"expected controlled local-def topology rejection for $source, obtained ${other.render}")
    }
  }
