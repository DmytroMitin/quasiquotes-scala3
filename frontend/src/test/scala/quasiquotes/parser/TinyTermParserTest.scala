package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import quasiquotes.source.SourceSpan

class TinyTermParserTest extends munit.FunSuite:
  private val acceptedShapes = List(
    "foo" -> "Ident(foo)",
    "1" -> "Literal(1)",
    "\"abc\"" -> "Literal(\"abc\")",
    "true" -> "Literal(true)",
    "+x" -> "Unary(+, Ident(x))",
    "-x" -> "Unary(-, Ident(x))",
    "!x" -> "Unary(!, Ident(x))",
    "~x" -> "Unary(~, Ident(x))",
    "+1" -> "Unary(+, Literal(1))",
    "-1" -> "Literal(-1)",
    "!true" -> "Unary(!, Literal(true))",
    "~1" -> "Unary(~, Literal(1))",
    "-(-x)" -> "Unary(-, Parens(Unary(-, Ident(x))))",
    "!(!x)" -> "Unary(!, Parens(Unary(!, Ident(x))))",
    "foo.bar" -> "Select(Ident(foo), bar)",
    "foo(x)" -> "Apply(Ident(foo), [Ident(x)])",
    "foo.bar(x)" -> "Apply(Select(Ident(foo), bar), [Ident(x)])",
    "foo + bar" -> "Infix(Ident(foo), +, Ident(bar))",
    "foo.bar + __hole0" -> "Infix(Select(Ident(foo), bar), +, Placeholder(__hole0))",
    "f(g(__hole0))" -> "Apply(Ident(f), [Apply(Ident(g), [Placeholder(__hole0)])])",
    "foo(bar(baz))" -> "Apply(Ident(foo), [Apply(Ident(bar), [Ident(baz)])])",
    "foo.bar(baz(__hole0))" -> "Apply(Select(Ident(foo), bar), [Apply(Ident(baz), [Placeholder(__hole0)])])",
    "(foo)" -> "Parens(Ident(foo))",
    "(foo.bar(__hole0))" -> "Parens(Apply(Select(Ident(foo), bar), [Placeholder(__hole0)]))",
    "f((__hole0))" -> "Apply(Ident(f), [Parens(Placeholder(__hole0))])",
    "(__hole0 + __hole1)" -> "Parens(Infix(Placeholder(__hole0), +, Placeholder(__hole1)))",
    "__hole0" -> "Placeholder(__hole0)",
    "foo(__hole0)" -> "Apply(Ident(foo), [Placeholder(__hole0)])",
    "foo.bar(__hole0)" -> "Apply(Select(Ident(foo), bar), [Placeholder(__hole0)])",
    "__hole0(__hole1)" -> "Apply(Placeholder(__hole0), [Placeholder(__hole1)])",
    "x: Int" -> "Typed(Ident(x), Type(Int))",
    "(x: Int)" -> "Parens(Typed(Ident(x), Type(Int)))",
    "foo(x: Int)" -> "Apply(Ident(foo), [Typed(Ident(x), Type(Int))])",
    "(a, b)" -> "Tuple([Ident(a), Ident(b)])",
    "(a, (b, c))" -> "Tuple([Ident(a), Tuple([Ident(b), Ident(c)])])",
    "foo((a, b))" -> "Apply(Ident(foo), [Tuple([Ident(a), Ident(b)])])",
    "foo(-x)" -> "Apply(Ident(foo), [Unary(-, Ident(x))])",
    "(-x, !b)" -> "Tuple([Unary(-, Ident(x)), Unary(!, Ident(b))])",
    "if cond then a else b" -> "If(Ident(cond), Ident(a), Ident(b))",
    "if !cond then -a else +b" -> "If(Unary(!, Ident(cond)), Unary(-, Ident(a)), Unary(+, Ident(b)))",
    "if (cond) a else b" -> "If(Parens(Ident(cond)), Ident(a), Ident(b))",
    "foo(if cond then a else b)" -> "Apply(Ident(foo), [If(Ident(cond), Ident(a), Ident(b))])"
  )

  private val rejectedCases = List(
    "foo bar" -> ParseErrorKind.SyntaxError,
    "foo(x) y" -> ParseErrorKind.SyntaxError,
    "foo; bar" -> ParseErrorKind.TrailingInput,
    "foo)" -> ParseErrorKind.TrailingInput,
    "foo(__hole0) junk" -> ParseErrorKind.SyntaxError,
    "__hole0 __hole1" -> ParseErrorKind.SyntaxError,
    "!!x" -> ParseErrorKind.SyntaxError
  )

  acceptedShapes.foreach { (input, expected) =>
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

  test("unary raw trees retain exact operator and operand spans") {
    val parsed = TinyTermParser.parseOrThrow("-(-x)")
    assert(clue(parsed.rawStructure).contains("PrefixOp(-"))
    assertEquals(DottySourceSpanAdapter.fromTree(parsed.rawTree), Some(SourceSpan(0, 5)))
    parsed.rawTree match
      case untpd.PrefixOp(_, outerOperand @ untpd.Parens(untpd.PrefixOp(_, innerOperand))) =>
        assertEquals(DottySourceSpanAdapter.fromTree(outerOperand), Some(SourceSpan(1, 5)))
        assertEquals(DottySourceSpanAdapter.fromTree(innerOperand), Some(SourceSpan(3, 4)))
      case other => fail(s"expected nested PrefixOp tree, got ${other.getClass.getSimpleName}")
  }

  test("interpolated strings remain an audited unsupported boundary") {
    val parsed = TinyTermParser.parseOrThrow("""s"a$x"""")
    assert(parsed.shape.isInstanceOf[TermShape.Unsupported])
    assert(clue(parsed.rawStructure).contains("InterpolatedString"))
  }

  test("placeholder helper recognizes synthetic holes") {
    assert(Placeholder.isPlaceholder("__hole0"))
    assert(Placeholder.isPlaceholder("__hole12"))
    assert(!Placeholder.isPlaceholder("foo"))
    assert(!Placeholder.isPlaceholder("__hole"))
  }

  rejectedCases.foreach { (input, expectedKind) =>
    test(s"reject malformed or trailing input for $input") {
      TinyTermParser.parse(input) match
        case Left(error) =>
          assertEquals(error.kind, expectedKind)
          assert(clue(error.summary).nonEmpty)
        case Right(parsed) =>
          fail(s"expected rejection, got ${parsed.shape.render}")
    }
  }

  test("reject empty input as syntax error") {
    TinyTermParser.parse("") match
      case Left(error) =>
        assertEquals(error.kind, ParseErrorKind.SyntaxError)
        assert(clue(error.summary).nonEmpty)
      case Right(parsed) =>
        fail(s"expected syntax failure, got ${parsed.shape.render}")
  }
