package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaPrimitiveTypedAscriptionAuthoringCharacterizationTest
    extends munit.FunSuite:
  private val identifier = TermShape.Identifier("x", isPlaceholder = false)

  test("direct primitive ascription construction exposes fresh structural fields"):
    val expression = Term.Name("x")
    val authoredType = Type.Name("Int")
    val ascription = Term.Ascribe(expression, authoredType)

    assert(ascription.expr eq expression)
    assert(ascription.tpe eq authoredType)
    assertEquals(ascription.expr.asInstanceOf[Term.Name].value, "x")
    assertEquals(ascription.tpe.asInstanceOf[Type.Name].value, "Int")
    assert(allTrees(ascription).forall(_.pos == Position.None))
    assertEquals(
      ScalametaTermProjection.project(ascription),
      Right(ProjectedTermShape(TermShape.Typed(identifier, "Int"), None))
    )

  test("fresh primitive Type.Name descendants project to their canonical strings"):
    List("String", "Boolean").foreach { typeName =>
      val ascription = Term.Ascribe(Term.Name("x"), Type.Name(typeName))

      assertEquals(ascription.tpe.asInstanceOf[Type.Name].value, typeName)
      assert(allTrees(ascription).forall(_.pos == Position.None))
      assertEquals(
        ScalametaTermProjection.project(ascription),
        Right(ProjectedTermShape(TermShape.Typed(identifier, typeName), None))
      )
    }

  test("parsed qualified spelling stays outside N010 while fresh authoring is canonical"):
    val parsed = Input.String("(x: scala.Int)").parse[Term].get.asInstanceOf[Term.Ascribe]
    val fresh = Term.Ascribe(Term.Name("x"), Type.Name("Int"))

    assert(parsed.tpe.isInstanceOf[Type.Select])
    assertEquals(
      ScalametaTermProjection.project(parsed).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_NORMAL_FORM_REJECTED")
    )
    assert(fresh.tpe.isInstanceOf[Type.Name])
    assertEquals(
      ScalametaTermProjection.project(fresh),
      Right(ProjectedTermShape(TermShape.Typed(identifier, "Int"), None))
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
