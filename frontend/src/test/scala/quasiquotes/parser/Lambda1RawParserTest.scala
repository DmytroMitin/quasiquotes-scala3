package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import quasiquotes.source.SourceSpan

class Lambda1RawParserTest extends munit.FunSuite:
  test("raw Lambda1 separates its binder declaration, bound references, and free identifiers") {
    val identity = TinyTermParser.parseOrThrow("(x: Int) => x")
    val application = TinyTermParser.parseOrThrow("(x: Int) => f(x)")

    assertEquals(identity.shape.render, "Lambda1(x: Int, BoundRef(x))")
    assertEquals(
      application.shape.render,
      "Lambda1(x: Int, Apply(Ident(f), [BoundRef(x)]))"
    )
  }

  test("raw Lambda1 preserves placeholders as holes instead of capturing by text") {
    val parsed = TinyTermParser.parseOrThrow("(x: Int) => __hole0")

    assertEquals(parsed.shape.render, "Lambda1(x: Int, Placeholder(__hole0))")
  }

  test("context-function spelling inside a Lambda1 body literal is not syntax") {
    val parsed = TinyTermParser.parseOrThrow("(x: Int) => \"?=>\"")

    assertEquals(parsed.shape.render, "Lambda1(x: Int, Literal(\"?=>\"))")
  }

  test("raw Lambda1 exposes exact lambda, parameter, type, body, and identifier spans") {
    val source = "(x: Int) => x + free"
    val parsed = TinyTermParser.parseOrThrow(source)

    parsed.rawTree match
      case function @ untpd.Function((parameter: untpd.ValDef) :: Nil, body) =>
        assertEquals(DottySourceSpanAdapter.fromTree(function), Some(SourceSpan(0, source.length)))
        assertEquals(DottySourceSpanAdapter.fromTree(parameter), Some(SourceSpan(1, 7)))
        val parameterNameStart = source.indexOf(parameter.name.toString, 1)
        assertEquals(
          SourceSpan(parameterNameStart, parameterNameStart + parameter.name.toString.length),
          SourceSpan(1, 2)
        )
        assertEquals(DottySourceSpanAdapter.fromTree(parameter.tpt), Some(SourceSpan(4, 7)))
        assertEquals(DottySourceSpanAdapter.fromTree(body), Some(SourceSpan(12, source.length)))
        body match
          case untpd.InfixOp(bound @ untpd.Ident(_), _, free @ untpd.Ident(_)) =>
            assertEquals(DottySourceSpanAdapter.fromTree(bound), Some(SourceSpan(12, 13)))
            assertEquals(DottySourceSpanAdapter.fromTree(free), Some(SourceSpan(16, 20)))
          case other => fail(s"expected infix Lambda1 body, got ${other.getClass.getSimpleName}")
      case other => fail(s"expected raw Function, got ${other.getClass.getSimpleName}")
  }

  test("raw parser deliberately rejects excluded Lambda1 variants") {
    val cases = Vector(
      "(x: Int, y: Int) => x + y" -> Lambda1DiagnosticMessages.ExactlyOneParameter,
      "x => x" -> Lambda1DiagnosticMessages.ExplicitParameterType,
      "(x: Int) => ((y: Int) => y)" -> Lambda1DiagnosticMessages.NestedLambda,
      "(x: Int) ?=> x" -> Lambda1DiagnosticMessages.ContextFunction
    )

    cases.foreach { case (source, expectedMessage) =>
      val rendered = TinyTermParser.parseOrThrow(source).shape.render
      assert(
        rendered.contains(s"Unsupported(Lambda1, $expectedMessage)"),
        clues(source, rendered)
      )
    }
  }
