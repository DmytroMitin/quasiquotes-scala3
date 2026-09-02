package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaP1BlockProjectionTest extends munit.FunSuite:
  test("Scalameta exposes P0, P1, and neighboring local definitions through block stats"):
    parsed("{ result }") match
      case block: Term.Block =>
        assertEquals(block.stats.size, 1)
        assert(block.stats.head.isInstanceOf[Term.Name])
      case other => fail(s"expected P0 Term.Block, got ${other.productPrefix}")

    parsed("{ first(); second(2); result }") match
      case block: Term.Block =>
        assertEquals(block.stats.size, 3)
        assert(block.stats.forall(_.isInstanceOf[Term]))
      case other => fail(s"expected P1 Term.Block, got ${other.productPrefix}")

    parsed("{ val x = 1; x }") match
      case block: Term.Block =>
        assert(block.stats.head.isInstanceOf[Defn.Val])
        assert(block.stats.last.isInstanceOf[Term.Name])
      case other => fail(s"expected local-val Term.Block, got ${other.productPrefix}")

    assertEquals(Term.Block(Nil).stats, Nil)

  test("projects transparent P0 braces while preserving the positioned block root span"):
    val source = "{ result }"
    val projected = project(parsed(source))

    assertEquals(projected.shape, TermShape.Identifier("result", false))
    assertEquals(projected.sourceSpan, Some(NeutralSourceSpan(0, source.length)))

  test("projects ordered P1 prefixes and keeps the final result structurally distinct"):
    val original = project(parsed("{ first(); second(2); result }")).shape
    val swapped = project(parsed("{ second(2); first(); result }")).shape
    val movedResult = project(parsed("{ first(); result; second(2) }")).shape

    assertEquals(
      original,
      TermShape.Block(
        List(
          TermShape.Apply(TermShape.Identifier("first", false), Nil),
          TermShape.Apply(
            TermShape.Identifier("second", false),
            List(TermShape.Literal("2"))
          )
        ),
        TermShape.Identifier("result", false)
      )
    )
    assertNotEquals(original, swapped)
    assertNotEquals(original, movedResult)

  test("recursively projects N003 children and nested transparent or P1 blocks"):
    assertEquals(
      project(parsed("{ (1, true); if flag then \"yes\" else \"no\" }")).shape,
      TermShape.Block(
        List(
          TermShape.Tuple(
            List(TermShape.Literal("1"), TermShape.Literal("true"))
          )
        ),
        TermShape.If(
          TermShape.Identifier("flag", false),
          TermShape.Literal("\"yes\""),
          TermShape.Literal("\"no\"")
        )
      )
    )

    val nestedP1 = Term.Block(
      List(
        Term.Block(
          List(
            Term.Apply(Term.Name("first"), Term.ArgClause(Nil)),
            Term.Name("middle")
          )
        ),
        Term.Name("result")
      )
    )
    assertEquals(
      project(nestedP1).shape,
      TermShape.Block(
        List(
          TermShape.Block(
            List(TermShape.Apply(TermShape.Identifier("first", false), Nil)),
            TermShape.Identifier("middle", false)
          )
        ),
        TermShape.Identifier("result", false)
      )
    )

    val nestedP0 = Term.Block(List(Term.Block(List(Term.Name("result")))))
    assertEquals(project(nestedP0).shape, TermShape.Identifier("result", false))

  test("propagates an active Lambda1 binder through every P1 child"):
    assertEquals(
      project(parsed("(x: Int) => { consume(x); x }")).shape,
      TermShape.Lambda1(
        BinderId(0),
        "x",
        "Int",
        TermShape.Block(
          List(
            TermShape.Apply(
              TermShape.Identifier("consume", false),
              List(TermShape.BoundReference(BinderId(0), "x"))
            )
          ),
          TermShape.BoundReference(BinderId(0), "x")
        )
      )
    )

  test("allows an independent Lambda1 child in a top-level binder-free P1"):
    assertEquals(
      project(parsed("{ ((x: Int) => x); result }")).shape,
      TermShape.Block(
        List(
          TermShape.Lambda1(
            BinderId(0),
            "x",
            "Int",
            TermShape.BoundReference(BinderId(0), "x")
          )
        ),
        TermShape.Identifier("result", false)
      )
    )

  test("preserves positioned and unpositioned P1 root span conventions"):
    val source = "{ first(); result }"
    assertEquals(
      project(parsed(source)).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = Term.Block(
      List(
        Term.Apply(Term.Name("first"), Term.ArgClause(Nil)),
        Term.Name("result")
      )
    )
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("rejects empty and non-Term block topology with stable neutral categories"):
    val cases = List(
      Term.Block(Nil) -> "NEUTRAL_BLOCK_EMPTY_UNSUPPORTED",
      parsed("{ val x = 1; x }") -> "NEUTRAL_P2_TYPE_REQUIRED",
      parsed("{ var x = 1; x }") -> "NEUTRAL_P2_MUTABLE_UNSUPPORTED",
      parsed("{ def f = 1; f }") -> "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
      parsed("{ import scala.util.Try; result }") -> "NEUTRAL_BLOCK_STATEMENT_UNSUPPORTED"
    )

    cases.foreach { (term, expectedCode) => assertErrorCode(term, expectedCode) }

  test("propagates unsupported children and the N004 nested-lambda boundary"):
    assertErrorCode(
      parsed("{ new java.lang.StringBuilder(16); result }"),
      "NEUTRAL_TERM_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("{ first(); new java.lang.StringBuilder(16) }"),
      "NEUTRAL_TERM_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("(x: Int) => { ((y: Int) => y); x }"),
      "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED"
    )

  test("retains the existing null-root failure"):
    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )

  private def parsed(source: String): Term =
    Input.String(source).parse[Term].get

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected),
      clues(source.structure)
    )
