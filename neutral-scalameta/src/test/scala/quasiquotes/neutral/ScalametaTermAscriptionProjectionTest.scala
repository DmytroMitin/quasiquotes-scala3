package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTermAscriptionProjectionTest extends munit.FunSuite:
  private final case class TypeFixture(
      source: String,
      normalForm: TypeNormalForm,
      canonical: String
  )

  private val id: String => TermShape = TermShape.Identifier(_, isPlaceholder = false)

  private val typeFixtures = List(
    TypeFixture("Int", TypeNormalForm.STypeIdent("Int"), "Int"),
    TypeFixture("String", TypeNormalForm.STypeIdent("String"), "String"),
    TypeFixture("Boolean", TypeNormalForm.STypeIdent("Boolean"), "Boolean"),
    TypeFixture("AnyVal", TypeNormalForm.STypeIdent("AnyVal"), "AnyVal"),
    TypeFixture(
      "List[Int]",
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(TypeNormalForm.STypeIdent("Int"))
      ),
      "List[Int]"
    ),
    TypeFixture(
      "Option[String]",
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Option"),
        List(TypeNormalForm.STypeIdent("String"))
      ),
      "Option[String]"
    ),
    TypeFixture(
      "Either[Int, String]",
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Either"),
        List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))
      ),
      "Either[Int, String]"
    ),
    TypeFixture(
      "(Int, String)",
      TypeNormalForm.STypeTuple(
        List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))
      ),
      "(Int, String)"
    ),
    TypeFixture(
      "(Int, String, Boolean)",
      TypeNormalForm.STypeTuple(
        List(
          TypeNormalForm.STypeIdent("Int"),
          TypeNormalForm.STypeIdent("String"),
          TypeNormalForm.STypeIdent("Boolean")
        )
      ),
      "(Int, String, Boolean)"
    ),
    TypeFixture(
      "Int => String",
      TypeNormalForm.STypeFunction(
        List(TypeNormalForm.STypeIdent("Int")),
        TypeNormalForm.STypeIdent("String")
      ),
      "Int => String"
    ),
    TypeFixture(
      "(Int, String) => Boolean",
      TypeNormalForm.STypeFunction(
        List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String")),
        TypeNormalForm.STypeIdent("Boolean")
      ),
      "(Int, String) => Boolean"
    )
  )

  test("projects the basic expression-ascription matrix recursively"):
    val fixtures = List(
      "x: Int" -> TermShape.Typed(id("x"), "Int"),
      "foo(x): String" ->
        TermShape.Typed(TermShape.Apply(id("foo"), List(id("x"))), "String"),
      "(x + y): Int" ->
        TermShape.Typed(TermShape.Infix(id("x"), "+", id("y")), "Int"),
      "(if cond then x else y): Int" ->
        TermShape.Typed(TermShape.If(id("cond"), id("x"), id("y")), "Int")
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parseTerm(source)).shape, expected, clues(source))
    }

  test("reuses N002 normal forms and the shared canonical renderer for every admitted Type"):
    typeFixtures.foreach { fixture =>
      val sourceType = parseType(fixture.source)
      val projectedType = ScalametaTypeNormalFormProjection.project(sourceType).toOption.get
      val projectedTerm = project(parseTerm(s"x: (${fixture.source})"))

      assertEquals(projectedType.normalForm, fixture.normalForm, clues(fixture.source))
      assertEquals(
        TermShapeTraversal.renderNormalForm(projectedType.normalForm),
        fixture.canonical,
        clues(fixture.source)
      )
      assertEquals(
        projectedTerm.shape,
        TermShape.Typed(id("x"), fixture.canonical),
        clues(fixture.source)
      )
      assertNotEquals(fixture.normalForm.render, fixture.canonical, clues(fixture.source))
    }

  test("projects nested admitted Types and nested ascriptions without a second grammar"):
    val sourceType = "Either[List[Int], Option[((String, Boolean)) => AnyVal]]"
    val canonical = "Either[List[Int], Option[(String, Boolean) => AnyVal]]"
    val projectedType = ScalametaTypeNormalFormProjection.project(parseType(sourceType)).toOption.get

    assertEquals(TermShapeTraversal.renderNormalForm(projectedType.normalForm), canonical)
    assertEquals(
      project(parseTerm(s"x: ($sourceType)")).shape,
      TermShape.Typed(id("x"), canonical)
    )
    assertEquals(
      project(parseTerm("((x: Int): AnyVal)")).shape,
      TermShape.Typed(TermShape.Typed(id("x"), "Int"), "AnyVal")
    )

  test("retains Lambda1 and P2 binder identity through an ascription"):
    val binder = BinderId(0)
    assertEquals(
      project(parseTerm("(x: Int) => (x: Int)")).shape,
      TermShape.Lambda1(
        binder,
        "x",
        "Int",
        TermShape.Typed(TermShape.BoundReference(binder, "x"), "Int")
      )
    )

    assertEquals(
      project(parseTerm("{ val x: Int = 1; (x: Int) }")).shape,
      TermShape.Block(
        List(BlockStatement.LocalVal(binder, "x", "Int", TermShape.Literal("1"))),
        TermShape.Typed(TermShape.BoundReference(binder, "x"), "Int")
      )
    )

  test("composes naturally inside corrected standard s interpolation arguments"):
    assertEquals(
      project(parseTerm("s\"${(x: Int)}\"")).shape,
      TermShape.InterpolatedString(
        "s",
        List("", ""),
        List(TermShape.Typed(id("x"), "Int"))
      )
    )

  test("propagates exact N002 failure categories and expression child failures"):
    val typeFailures = List(
      "x: Double" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: scala.Int" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: Map[Int, String]" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: Either[Int]" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: (Int, String, Boolean, AnyVal)" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: ((Int, String, Boolean) => AnyVal)" -> "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
      "x: Int { type Out = String }" -> "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
      "x: Int | String" -> "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED",
      "x: List[?]" -> "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED"
    )
    typeFailures.foreach { (source, expectedCode) =>
      assertErrorCode(parseTerm(source), expectedCode)
    }

    assertErrorCode(
      parseTerm("(value match { case _ => 1 }): Int"),
      "NEUTRAL_TERM_UNSUPPORTED"
    )
    assertErrorCode(
      parseTerm("(((x: Int) => ((y: Int) => y)): Int)"),
      "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED"
    )

  test("does not weaken the source-owned local-def direct body and result restrictions"):
    assertErrorCode(
      parseTerm("{ def id(x: Int): Int = (x: Int); id }"),
      "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED"
    )
    assertErrorCode(
      parseTerm("{ def id(x: Int): Int = x; (id: Int) }"),
      "NEUTRAL_LOCAL_DEF_RESULT_UNSUPPORTED"
    )

  test("preserves the complete positioned root span and None for an unpositioned root"):
    val source = "foo(x): Either[Int, String]"
    assertEquals(
      project(parseTerm(source)).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = Term.Ascribe(Term.Name("x"), Type.Name("Int"))
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(
      project(unpositioned),
      ProjectedTermShape(TermShape.Typed(id("x"), "Int"), None)
    )

  private def parseTerm(source: String): Term =
    Input.String(source).parse[Term].get

  private def parseType(source: String): Type =
    Input.String(source).parse[Type].get

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected)
    )
