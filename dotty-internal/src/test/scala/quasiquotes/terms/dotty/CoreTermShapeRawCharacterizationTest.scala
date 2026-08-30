package quasiquotes.terms.dotty

import quasiquotes.parser.TinyTermParser

class CoreTermShapeRawCharacterizationTest extends munit.FunSuite:
  private val expected = Vector(
    "1" -> "Number(1,Whole(10))",
    "-1" -> "Number(-1,Whole(10))",
    "1 + 1" ->
      "InfixOp(Number(1,Whole(10)),Ident(+),Number(1,Whole(10)))",
    "1 + 2 * 3" ->
      "InfixOp(Number(1,Whole(10)),Ident(+),InfixOp(Number(2,Whole(10)),Ident(*),Number(3,Whole(10))))",
    "(1 + 2) * 3" ->
      "InfixOp(Parens(InfixOp(Number(1,Whole(10)),Ident(+),Number(2,Whole(10)))),Ident(*),Number(3,Whole(10)))",
    "1 - 2" ->
      "InfixOp(Number(1,Whole(10)),Ident(-),Number(2,Whole(10)))"
  )

  expected.foreach { case (source, structure) =>
    test(s"characterizes parser-produced raw integer/infix structure: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)

      assertEquals(parsed.rawStructure, structure)
    }
  }
