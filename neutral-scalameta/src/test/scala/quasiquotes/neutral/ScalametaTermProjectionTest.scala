package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTermProjectionTest extends munit.FunSuite:
  test("projects the admitted direct identifier, selection, and one-list Apply family"):
    val fixtures = List(
      q"f" -> "Ident(f)",
      q"obj.f" -> "Select(Ident(obj), f)",
      q"f()" -> "Apply(Ident(f), [])",
      q"f(1)" -> "Apply(Ident(f), [Literal(1)])",
      q"f(1, 2)" -> "Apply(Ident(f), [Literal(1), Literal(2)])",
      q"obj.f(1)" -> "Apply(Select(Ident(obj), f), [Literal(1)])",
      q"obj.f(1 + 2)" ->
        "Apply(Select(Ident(obj), f), [Infix(Literal(1), +, Literal(2))])",
      q"obj.inner.f(1 + 2, 3)" ->
        "Apply(Select(Select(Ident(obj), inner), f), [Infix(Literal(1), +, Literal(2)), Literal(3)])"
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(source).shape.render, expected, clues(source.syntax))
    }

  test("recursively composes ordinary calls and selections with integer and infix projection"):
    val fixtures = List(
      q"f(1 + 2)" -> "Apply(Ident(f), [Infix(Literal(1), +, Literal(2))])",
      q"obj.f(1 + 2, 3)" ->
        "Apply(Select(Ident(obj), f), [Infix(Literal(1), +, Literal(2)), Literal(3)])",
      q"obj.inner.f(1 + 2 * 3)" ->
        "Apply(Select(Select(Ident(obj), inner), f), [Infix(Literal(1), +, Infix(Literal(2), *, Literal(3)))])",
      q"outer(inner(1), obj.f(2))" ->
        "Apply(Ident(outer), [Apply(Ident(inner), [Literal(1)]), Apply(Select(Ident(obj), f), [Literal(2)])])"
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(source).shape.render, expected, clues(source.syntax))
    }

  test("projects the admitted integer literal and binary infix family"):
    assertEquals(
      project(q"1").shape,
      TermShape.Literal("1")
    )

  test("projects canonical String and Boolean literals without collapsing literal kinds"):
    val integer = project(Lit.Int(1)).shape
    val string = project(Lit.String("1")).shape

    assertEquals(integer, TermShape.Literal("1"))
    assertEquals(string, TermShape.Literal("\"1\""))
    assertNotEquals(integer, string)
    assertEquals(project(Lit.Boolean(true)).shape, TermShape.Literal("true"))
    assertEquals(project(Lit.Boolean(false)).shape, TermShape.Literal("false"))
    assertEquals(
      project(Lit.String("a\"b\\c")).shape,
      TermShape.Literal("\"a\"b\\c\"")
    )

    val recursive = Term.Apply(
      Term.Name("f"),
      Term.ArgClause(List(Lit.String("1"), Lit.Boolean(true)))
    )
    assertEquals(
      project(recursive).shape,
      TermShape.Apply(
        TermShape.Identifier("f", false),
        List(TermShape.Literal("\"1\""), TermShape.Literal("true"))
      )
    )
    assertEquals(
      project(Term.Select(Lit.String("text"), Term.Name("length"))).shape,
      TermShape.Select(TermShape.Literal("\"text\""), "length")
    )
    assertEquals(
      project(
        Term.ApplyInfix(
          Lit.String("left"),
          Term.Name("+"),
          Type.ArgClause(Nil),
          Term.ArgClause(List(Lit.String("right")))
        )
      ).shape,
      TermShape.Infix(
        TermShape.Literal("\"left\""),
        "+",
        TermShape.Literal("\"right\"")
      )
    )

  test("projects exactly the admitted structural unary family and preserves signed-literal behavior"):
    List("+", "-", "!", "~").foreach { operator =>
      assertEquals(
        project(Term.ApplyUnary(Term.Name(operator), Term.Name("value"))).shape,
        TermShape.Unary(operator, TermShape.Identifier("value", false))
      )
    }

    val nested = Term.ApplyUnary(
      Term.Name("!"),
      Term.ApplyUnary(Term.Name("-"), Term.Name("value"))
    )
    assertEquals(
      project(nested).shape,
      TermShape.Unary(
        "!",
        TermShape.Unary("-", TermShape.Identifier("value", false))
      )
    )

    assertEquals(project(q"-1").shape, TermShape.Literal("-1"))
    assertEquals(project(q"+1").shape, TermShape.Literal("1"))
    assertEquals(
      project(Term.ApplyUnary(Term.Name("-"), Lit.Int(1))).shape,
      TermShape.Unary("-", TermShape.Literal("1"))
    )
    assertErrorCode(
      Term.ApplyUnary(Term.Name("!"), q"value match { case _ => 1 }"),
      "NEUTRAL_TERM_UNSUPPORTED"
    )

  test("projects tuples recursively only at the established arity 2 through 22"):
    val pair = Term.Tuple(List(Lit.Int(1), Lit.String("two")))
    assertEquals(
      project(pair).shape,
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("\"two\"")))
    )

    val nested = Term.Tuple(
      List(
        Term.Apply(Term.Name("f"), Term.ArgClause(List(Lit.Boolean(true)))),
        Term.ApplyUnary(Term.Name("!"), Term.Name("flag")),
        Term.Tuple(List(Lit.Int(1), Lit.Int(2)))
      )
    )
    assertEquals(
      project(nested).shape,
      TermShape.Tuple(
        List(
          TermShape.Apply(
            TermShape.Identifier("f", false),
            List(TermShape.Literal("true"))
          ),
          TermShape.Unary("!", TermShape.Identifier("flag", false)),
          TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2")))
        )
      )
    )

    val upperBoundary = Term.Tuple((1 to 22).toList.map(Lit.Int(_)))
    assertEquals(
      project(upperBoundary).shape,
      TermShape.Tuple((1 to 22).toList.map(value => TermShape.Literal(value.toString)))
    )

    assertErrorCode(
      Term.Tuple(List(Lit.Int(1))),
      "NEUTRAL_TUPLE_ARITY_UNSUPPORTED"
    )
    assertErrorCode(
      Term.Tuple((1 to 23).toList.map(Lit.Int(_))),
      "NEUTRAL_TUPLE_ARITY_UNSUPPORTED"
    )

  test("projects explicit three-branch if expressions recursively and rejects no-else topology"):
    val explicit = Input.String("if flag then (1, true) else (\"x\", !flag)").parse[Term].get
    assertEquals(
      project(explicit).shape,
      TermShape.If(
        TermShape.Identifier("flag", false),
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("true"))),
        TermShape.Tuple(
          List(
            TermShape.Literal("\"x\""),
            TermShape.Unary("!", TermShape.Identifier("flag", false))
          )
        )
      )
    )

    val nested = q"if true then if false then 1 else 2 else 3"
    assertEquals(
      project(nested).shape,
      TermShape.If(
        TermShape.Literal("true"),
        TermShape.If(
          TermShape.Literal("false"),
          TermShape.Literal("1"),
          TermShape.Literal("2")
        ),
        TermShape.Literal("3")
      )
    )

    val noElse = Input.String("if true then 1").parse[Term].get
    assertErrorCode(noElse, "NEUTRAL_IF_ELSE_UNSUPPORTED")
    assertEquals(
      project(q"1 + 1").shape,
      TermShape.Infix(
        TermShape.Literal("1"),
        "+",
        TermShape.Literal("1")
      )
    )

  test("preserves Scalameta precedence through recursive infix projection"):
    assertEquals(
      project(q"1 + 2 * 3").shape,
      TermShape.Infix(
        TermShape.Literal("1"),
        "+",
        TermShape.Infix(
          TermShape.Literal("2"),
          "*",
          TermShape.Literal("3")
        )
      )
    )

  test("preserves exact root offsets from a parsed input"):
    val parsed = Input.String("1 + 2 * 3").parse[Term].get

    assertEquals(parsed.pos.start, 0)
    assertEquals(parsed.pos.end, 9)
    assertEquals(
      project(parsed).sourceSpan,
      Some(NeutralSourceSpan(0, 9))
    )

    val applied = Input.String("obj.f(1)").parse[Term].get
    assertEquals(
      project(applied),
      ProjectedTermShape(
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("obj", false), "f"),
          List(TermShape.Literal("1"))
        ),
        Some(NeutralSourceSpan(0, 8))
      )
    )

    val conditionalSource = "if true then (1, false) else (\"x\", !flag)"
    val conditional = Input.String(conditionalSource).parse[Term].get
    assertEquals(
      project(conditional).sourceSpan,
      Some(NeutralSourceSpan(0, conditionalSource.length))
    )

  test("accepts an explicitly constructed unpositioned tree"):
    val source = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(List(Lit.Int(2)))
    )

    assertEquals(source.pos, Position.None)
    assertEquals(
      project(source),
      ProjectedTermShape(
        TermShape.Infix(
          TermShape.Literal("1"),
          "+",
          TermShape.Literal("2")
        ),
        None
      )
    )

    val applied = Term.Apply(
      Term.Select(Term.Name("obj"), Term.Name("f")),
      Term.ArgClause(List(Lit.Int(1)))
    )
    assertEquals(applied.pos, Position.None)
    assertEquals(
      project(applied),
      ProjectedTermShape(
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("obj", false), "f"),
          List(TermShape.Literal("1"))
        ),
        None
      )
    )

    val conditional = Term.If(Lit.Boolean(true), Lit.Int(1), Lit.Int(2))
    assertEquals(conditional.pos, Position.None)
    assertEquals(
      project(conditional),
      ProjectedTermShape(
        TermShape.If(
          TermShape.Literal("true"),
          TermShape.Literal("1"),
          TermShape.Literal("2")
        ),
        None
      )
    )

  test("rejects every representative non-admitted Term family deterministically"):
    val unsupported = List[(Term, String)](
      q"throw boom" -> "Term.Throw",
      q"(1: Int)" -> "Term.Ascribe",
      Lit.Unit() -> "Lit.Unit"
    )

    unsupported.foreach { (source, nodeKind) =>
      assertEquals(
        ScalametaTermProjection.project(source),
        Left(
          NeutralProjectionError(
            "NEUTRAL_TERM_UNSUPPORTED",
            s"unsupported Scalameta term node: $nodeKind."
          )
        )
      )
    }

  test("uses the Lit.Int semantic value when Scalameta folds a negative literal"):
    val source = q"-1"

    assert(source.isInstanceOf[Lit.Int])
    assertEquals(project(source).shape, TermShape.Literal("-1"))

  test("rejects missing input and unsupported infix topology"):
    val typeArguments = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(List(Type.Name("Int"))),
      Term.ArgClause(List(Lit.Int(2)))
    )
    val noArguments = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(Nil)
    )
    val multipleArguments = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(List(Lit.Int(2), Lit.Int(3)))
    )
    val contextualArguments = Term.ApplyInfix(
      Lit.Int(1),
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(List(Lit.Int(2)), Some(Mod.Using()))
    )

    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )
    assertErrorCode(typeArguments, "NEUTRAL_INFIX_TYPE_ARGUMENTS_UNSUPPORTED")
    assertErrorCode(noArguments, "NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED")
    assertErrorCode(multipleArguments, "NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED")
    assertErrorCode(contextualArguments, "NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED")

  test("rejects unsupported source names and neighboring Apply topology deterministically"):
    val invalidIdentifierNames = List("_", "if", "bad.name", "$hole", "<init>", "naïve")
    invalidIdentifierNames.foreach { name =>
      assertErrorCode(Term.Name(name), "NEUTRAL_IDENTIFIER_NAME_UNSUPPORTED")
    }

    val invalidSelectionNames = List("bad.name", "$hole", "<init>", "naïve")
    invalidSelectionNames.foreach { name =>
      assertErrorCode(
        Term.Select(Term.Name("obj"), Term.Name(name)),
        "NEUTRAL_SELECTION_NAME_UNSUPPORTED"
      )
    }

    assertErrorCode(q"f(1)(2)", "NEUTRAL_APPLY_MULTIPLE_LISTS_UNSUPPORTED")
    assertErrorCode(q"f[Int](1)", "NEUTRAL_APPLY_FUNCTION_UNSUPPORTED")
    assertErrorCode(q"f(using 1)", "NEUTRAL_APPLY_ARGUMENT_CLAUSE_UNSUPPORTED")
    assertErrorCode(q"f(value = 1)", "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED")
    assertErrorCode(q"f(values*)", "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED")

  test("propagates a nested unsupported child without manufacturing TermShape.Unsupported"):
    assertEquals(
      ScalametaTermProjection.project(q"f(value match { case _ => 1 })"),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_UNSUPPORTED",
          "unsupported Scalameta term node: Term.Match."
        )
      )
    )

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected)
    )
