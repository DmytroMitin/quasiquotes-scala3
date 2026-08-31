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

  test("rejects every representative non-admitted Term family deterministically"):
    val unsupported = List[(Term, String)](
      q"x => x" -> "Term.Function",
      q"new java.lang.StringBuilder(16)" -> "Term.New",
      Term.ApplyUnary(Term.Name("-"), Lit.Int(1)) -> "Term.ApplyUnary",
      q"(1, 2)" -> "Term.Tuple",
      q"if true then 1 else 2" -> "Term.If",
      q"{ val x = 1; x }" -> "Term.Block",
      q"(1: Int)" -> "Term.Ascribe",
      Input.String("s\"value=$f\"").parse[Term].get -> "Term.Interpolate",
      Lit.String("text") -> "Lit.String"
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
      ScalametaTermProjection.project(q"f(new java.lang.StringBuilder(16))"),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_UNSUPPORTED",
          "unsupported Scalameta term node: Term.New."
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
