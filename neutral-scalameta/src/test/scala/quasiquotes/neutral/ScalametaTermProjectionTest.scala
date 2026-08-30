package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTermProjectionTest extends munit.FunSuite:
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

  test("rejects every representative non-admitted Term family deterministically"):
    val unsupported = List[(Term, String)](
      q"identifier" -> "Term.Name",
      q"receiver.member" -> "Term.Select",
      q"function(1)" -> "Term.Apply",
      q"new java.lang.StringBuilder(16)" -> "Term.New",
      Term.ApplyUnary(Term.Name("-"), Lit.Int(1)) -> "Term.ApplyUnary",
      q"(1, 2)" -> "Term.Tuple",
      q"if true then 1 else 2" -> "Term.If",
      q"{ val x = 1; x }" -> "Term.Block",
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

  test("propagates a nested unsupported child without manufacturing TermShape.Unsupported"):
    assertEquals(
      ScalametaTermProjection.project(q"1 + identifier"),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_UNSUPPORTED",
          "unsupported Scalameta term node: Term.Name."
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
