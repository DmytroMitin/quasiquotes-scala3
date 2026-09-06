package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.terms.ConstructedTerm

final class TermUntypedLoweringInternalTest extends munit.FunSuite:
  test("contradictory and malformed private binder graphs fail as malformed semantic values"):
    withContext:
      val malformed = List[TermShape](
        TermShape.BoundReference(BinderId(0), "x"),
        TermShape.Lambda1(null, "x", "Int", ident("x")),
        TermShape.Lambda1(
          BinderId(0),
          "x",
          "Int",
          TermShape.BoundReference(BinderId(1), "x")
        ),
        TermShape.Lambda1(
          BinderId(0),
          "x",
          "Int",
          TermShape.Lambda1(
            BinderId(0),
            "y",
            "Int",
            TermShape.BoundReference(BinderId(0), "y")
          )
        ),
        TermShape.Block(
          List(BlockStatement.LocalVal(BinderId(0), "x", null, TermShape.Literal("1"))),
          TermShape.BoundReference(BinderId(0), "x")
        ),
        TermShape.Block(
          List(
            BlockStatement.LocalDef(
              BinderId(0),
              "id",
              BinderId(0),
              "x",
              TypeShape.Identifier("Int"),
              TypeShape.Identifier("Int"),
              TermShape.BoundReference(BinderId(0), "x")
            )
          ),
          TermShape.BoundReference(BinderId(0), "id")
        ),
        TermShape.Block(
          List(
            BlockStatement.LocalDef(
              BinderId(0),
              "id",
              BinderId(1),
              "x",
              TypeShape.Apply(null, Nil),
              TypeShape.Identifier("Int"),
              TermShape.BoundReference(BinderId(1), "x")
            )
          ),
          TermShape.BoundReference(BinderId(0), "id")
        )
      )

      malformed.foreach { semantic =>
        assertEquals(lowerFailure(semantic).code, "MALFORMED_SEMANTIC_VALUE", clues(semantic))
      }

  test("well-formed nested and multi-local binder topologies remain unsupported"):
    withContext:
      val nestedLambda = TermShape.Lambda1(
        BinderId(0),
        "x",
        "Int",
        TermShape.Lambda1(
          BinderId(1),
          "y",
          "Int",
          TermShape.BoundReference(BinderId(1), "y")
        )
      )
      val multipleLocals = TermShape.Block(
        List(
          BlockStatement.LocalVal(BinderId(0), "x", "Int", TermShape.Literal("1")),
          BlockStatement.LocalVal(BinderId(1), "y", "Int", TermShape.Literal("2"))
        ),
        TermShape.BoundReference(BinderId(1), "y")
      )

      List(nestedLambda, multipleLocals).foreach { semantic =>
        assertEquals(lowerFailure(semantic).code, "UNSUPPORTED_SEMANTIC_VALUE", clues(semantic))
      }

  test("private exact-topology validator rejects null and wrong raw results"):
    withContext:
      given SourceFile = NoSource
      val semantic = TermShape.Literal("1")
      val constructed = ConstructedTerm.fromShape(semantic).toOption.get

      List[untpd.Tree](null, untpd.Ident(termName("wrong"))).foreach { raw =>
        val failure = validateRaw(constructed, raw)
          .left
          .toOption
          .getOrElse(fail(s"wrong raw result unexpectedly passed: $raw"))
        assertEquals(failure.code, "INTERNAL_INVARIANT_FAILED", clues(raw, failure))
      }

  private def validateRaw(
      constructed: ConstructedTerm,
      raw: untpd.Tree
  )(using Context): Either[TermUntypedLowering.Failure, Unit] =
    val method = TermUntypedLowering.getClass.getDeclaredMethods
      .find(method => method.getName == "validateRaw" && method.getParameterCount == 4)
      .getOrElse(fail("private Term raw validator was not found"))
    method.setAccessible(true)
    method
      .invoke(TermUntypedLowering, constructed, raw, "term", summon[Context])
      .asInstanceOf[Either[TermUntypedLowering.Failure, Unit]]

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def lowerFailure(
      semantic: TermShape
  )(using Context): TermUntypedLowering.Failure =
    TermUntypedLowering
      .lower(semantic)
      .left
      .toOption
      .getOrElse(fail(s"semantic Term unexpectedly lowered: $semantic"))

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
