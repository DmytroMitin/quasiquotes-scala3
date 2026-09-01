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
      "InfixOp(Number(1,Whole(10)),Ident(-),Number(2,Whole(10)))",
    "obj" -> "Ident(obj)",
    "obj.f" -> "Select(Ident(obj), f)",
    "f()" -> "Apply(Ident(f), [])",
    "f(1)" -> "Apply(Ident(f), [Number(1,Whole(10))])",
    "obj.f(1 + 2, 3)" ->
      "Apply(Select(Ident(obj), f), [InfixOp(Number(1,Whole(10)),Ident(+),Number(2,Whole(10))), Number(3,Whole(10))])",
    "f(g(1), 2)" ->
      "Apply(Ident(f), [Apply(Ident(g), [Number(1,Whole(10))]), Number(2,Whole(10))])",
    "true" -> "Literal(Boolean(true))",
    "\"text\"" -> "Literal(String(\"text\"))",
    "!flag" -> "PrefixOp(!,Ident(flag))",
    "~mask" -> "PrefixOp(~,Ident(mask))",
    "-value" -> "PrefixOp(-,Ident(value))",
    "(x, true, \"value\")" ->
      "Tuple([Ident(x), Literal(Boolean(true)), Literal(String(\"value\"))])",
    "if cond then \"yes\" else \"no\"" ->
      "If(Ident(cond),Literal(String(\"yes\")),Literal(String(\"no\")))"
  )

  expected.foreach { case (source, structure) =>
    test(s"characterizes parser-produced raw direct-lowerer structure: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)

      assertEquals(parsed.rawStructure, structure)
    }
  }
