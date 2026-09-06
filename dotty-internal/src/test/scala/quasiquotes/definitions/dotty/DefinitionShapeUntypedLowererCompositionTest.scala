package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import scala.meta.*
import scala.meta.dialects.Scala3

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.neutral.ScalametaDefinitionProjection
import _root_.quasiquotes.parser.TypeShapeInspector

class DefinitionShapeUntypedLowererCompositionTest extends munit.FunSuite:
  test("composes the accepted N definition dispatcher with the reusable U exact lowerer for all five families") {
    withContext {
      val fixtures = Vector(
        ("val answer: Int = 42", classOf[DefinitionShape.ImmutableVal], classOf[untpd.ValDef]),
        (
          "def answer: Int = 42",
          classOf[DefinitionShape.ParameterlessDef],
          classOf[untpd.DefDef]
        ),
        (
          "def id(x: Int): Int = x",
          classOf[DefinitionShape.SingleParameterDef],
          classOf[untpd.DefDef]
        ),
        (
          "def pair(x: Int, y: String): (Int, String) = (x, y)",
          classOf[DefinitionShape.TwoParameterDef],
          classOf[untpd.DefDef]
        ),
        (
          "type Result = Option[Int]",
          classOf[DefinitionShape.SimpleTypeAlias],
          classOf[untpd.TypeDef]
        )
      )

      fixtures.foreach { (source, shapeClass, treeClass) =>
        val projected = ScalametaDefinitionProjection
          .projectShape(parsed(source))
          .fold(error => fail(error.message), identity)
        assert(shapeClass.isInstance(projected.shape), clues(source, projected.shape))

        val raw = DefinitionShapeUntypedLowerer
          .lower(projected.shape)
          .fold(error => fail(error.message), identity)
        assert(treeClass.isInstance(raw), clues(source, raw.getClass.getName))
        assertEquals(
          DefinitionShapeUntypedLowerer.validateRawInvariant(raw, "N-to-U composition"),
          Right(()),
          clues(source)
        )
      }

      val alias = DefinitionShapeUntypedLowerer
        .lower(
          ScalametaDefinitionProjection
            .projectShape(parsed("type Result = Option[Int]"))
            .toOption
            .get
            .shape
        )
        .toOption
        .get
        .asInstanceOf[untpd.TypeDef]
      assertEquals(
        TypeShapeInspector.rawStructure(alias.rhs),
        "AppliedTypeTree(Ident(Option), [Ident(Int)])"
      )
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")
