package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaP1BlockAuthoringCharacterizationTest extends munit.FunSuite:
  test("direct Scalameta 4.17.3 Block keeps ordered prefixes and the final result in stats"):
    val first = Term.Apply(Term.Name("first"), Term.ArgClause(Nil))
    val second = Term.Apply(Term.Name("second"), Term.ArgClause(List(Lit.Int(2))))
    val result = Term.Name("result")
    val block = Term.Block(List(first, second, result))

    assertEquals(block.stats.map(_.productPrefix), List("Term.Apply", "Term.Apply", "Term.Name"))
    assertEquals(block.stats.last, result)
    assertEquals(
      project(block),
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

  test("direct nested P1 Blocks remain structural children and wholly unpositioned"):
    val nestedPrefix = Term.Block(List(Lit.Int(1), Lit.Boolean(true)))
    val nestedResult = Term.Block(
      List(
        Term.Tuple(List(Lit.Int(2), Lit.Int(3))),
        Term.If(Term.Name("flag"), Lit.String("yes"), Lit.String("no"))
      )
    )
    val root = Term.Block(List(nestedPrefix, nestedResult))

    assertEquals(root.stats.map(_.productPrefix), List("Term.Block", "Term.Block"))
    assertEquals(
      project(root),
      TermShape.Block(
        List(
          TermShape.Block(List(TermShape.Literal("1")), TermShape.Literal("true"))
        ),
        TermShape.Block(
          List(TermShape.Tuple(List(TermShape.Literal("2"), TermShape.Literal("3")))),
          TermShape.If(
            TermShape.Identifier("flag", false),
            TermShape.Literal("\"yes\""),
            TermShape.Literal("\"no\"")
          )
        )
      )
    )
    assert(allTrees(root).forall(_.pos == Position.None))

  test("P0 is transparent while empty and definition-bearing blocks stay outside P1"):
    val result = Term.Name("result")
    assertEquals(project(Term.Block(List(result))), TermShape.Identifier("result", false))
    assertEquals(
      ScalametaTermProjection.project(Term.Block(Nil)).left.toOption.map(_.code),
      Some("NEUTRAL_BLOCK_EMPTY_UNSUPPORTED")
    )

    val localVal = Input.String("{ val x = 1; x }").parse[Term].get.asInstanceOf[Term.Block]
    val localDef = Input.String("{ def f = 1; f }").parse[Term].get.asInstanceOf[Term.Block]
    assert(!localVal.stats.head.isInstanceOf[Term])
    assert(!localDef.stats.head.isInstanceOf[Term])

  private def project(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(projected) => projected.shape
      case Left(error) => fail(error.message)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
