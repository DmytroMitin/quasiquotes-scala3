package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTermAscriptionAstCharacterizationTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      expressionKind: String,
      typeKind: String
  )

  test("ordinary expression ascriptions expose structural expression and Type fields"):
    val fixtures = List(
      Fixture("x: Int", "Term.Name", "Type.Name"),
      Fixture("foo(x): String", "Term.Apply", "Type.Name"),
      Fixture("(x + y): Int", "Term.ApplyInfix", "Type.Name"),
      Fixture("(if cond then x else y): Int", "Term.If", "Type.Name"),
      Fixture("x: List[Int]", "Term.Name", "Type.Apply"),
      Fixture("x: Either[Int, String]", "Term.Name", "Type.Apply"),
      Fixture("x: (Int, String)", "Term.Name", "Type.Tuple"),
      Fixture("x: (Int => String)", "Term.Name", "Type.Function"),
      Fixture("x: ((Int, String) => Boolean)", "Term.Name", "Type.Function")
    )

    fixtures.foreach { fixture =>
      val root = parseTerm(fixture.source)
      root match
        case ascription: Term.Ascribe =>
          assertEquals(ascription.expr.productPrefix, fixture.expressionKind, clues(fixture.source))
          assertEquals(ascription.tpe.productPrefix, fixture.typeKind, clues(fixture.source))
          assertEquals(ascription.pos.start, 0, clues(fixture.source))
          assertEquals(ascription.pos.end, fixture.source.length, clues(fixture.source))
        case other =>
          fail(s"expected Term.Ascribe for ${fixture.source}, found ${other.productPrefix}")
    }

  test("applied tuple function and selected Types retain their exact Scalameta categories"):
    val applied = ascription("x: Either[List[Int], Option[String]]").tpe
      .asInstanceOf[Type.Apply]
    assertEquals(applied.tpe.productPrefix, "Type.Name")
    assertEquals(applied.args.map(_.productPrefix), List("Type.Apply", "Type.Apply"))

    val tuple = ascription("x: (Int, String, Boolean)").tpe
      .asInstanceOf[Type.Tuple]
    assertEquals(tuple.args.map(_.productPrefix), List.fill(3)("Type.Name"))

    val function1 = ascription("x: (Int => String)").tpe
      .asInstanceOf[Type.Function]
    assertEquals(function1.params.map(_.productPrefix), List("Type.Name"))
    assertEquals(function1.res.productPrefix, "Type.Name")

    val function2 = ascription("x: ((Int, String) => Boolean)").tpe
      .asInstanceOf[Type.Function]
    assertEquals(function2.params.map(_.productPrefix), List("Type.Name", "Type.Name"))
    assertEquals(function2.res.productPrefix, "Type.Name")

    val selected = ascription("x: scala.Int").tpe.asInstanceOf[Type.Select]
    assertEquals(selected.qual.productPrefix, "Term.Name")
    assertEquals(selected.name.value, "Int")

    val refined = ascription("x: Int { type Out = String }").tpe
    assertEquals(refined.productPrefix, "Type.Refine")

  test("parentheses are transparent to the ascription category while positions remain truthful"):
    val plain = ascription("x: Int")
    val parenthesizedExpression = ascription("(x): Int")
    val parenthesizedRoot = parseTerm("(x: Int)")

    assertEquals(plain.expr.productPrefix, "Term.Name")
    assertEquals(parenthesizedExpression.expr.productPrefix, "Term.Name")
    assertEquals(parenthesizedRoot.productPrefix, "Term.Ascribe")
    assertEquals(parenthesizedExpression.pos.start, 0)
    assertEquals(parenthesizedExpression.pos.end, 8)
    assertEquals(parenthesizedRoot.pos.start, 0)
    assertEquals(parenthesizedRoot.pos.end, 8)

  test("an unparenthesized function arrow belongs to the surrounding Term grammar"):
    val unary = parseTerm("x: Int => String")
    val binary = parseTerm("x: (Int, String) => Boolean")

    assertEquals(unary.productPrefix, "Term.Apply", clues(unary.structure))
    assertEquals(binary.productPrefix, "Term.Apply", clues(binary.structure))

    val unaryFunction = bareArrowFunction(unary)
    assertEquals(unaryFunction.paramClause.values.map(_.name.value), List("Int"))
    assertEquals(unaryFunction.paramClause.values.map(_.decltpe), List(None))
    assertEquals(unaryFunction.body.asInstanceOf[Term.Name].value, "String")

    val binaryFunction = bareArrowFunction(binary)
    assertEquals(binaryFunction.paramClause.values.map(_.name.value), List("Int", "String"))
    assertEquals(binaryFunction.paramClause.values.map(_.decltpe), List(None, None))
    assertEquals(binaryFunction.body.asInstanceOf[Term.Name].value, "Boolean")

  test("nested ascriptions remain recursive Term.Ascribe nodes"):
    val outer = ascription("((x: Int): AnyVal)")
    assertEquals(outer.tpe.productPrefix, "Type.Name")
    outer.expr match
      case inner: Term.Ascribe =>
        assertEquals(inner.expr.productPrefix, "Term.Name")
        assertEquals(inner.tpe.productPrefix, "Type.Name")
      case other => fail(s"expected nested Term.Ascribe, found ${other.productPrefix}")

  private def parseTerm(source: String): Term =
    Input.String(source).parse[Term].get

  private def ascription(source: String): Term.Ascribe =
    parseTerm(source) match
      case value: Term.Ascribe => value
      case other => fail(s"expected Term.Ascribe, found ${other.productPrefix}")

  private def bareArrowFunction(term: Term): Term.Function =
    val application = term.asInstanceOf[Term.Apply]
    assertEquals(application.fun.asInstanceOf[Term.Name].value, "x")
    val block = application.argClause.values.head.asInstanceOf[Term.Block]
    block.stats.head.asInstanceOf[Term.Function]
