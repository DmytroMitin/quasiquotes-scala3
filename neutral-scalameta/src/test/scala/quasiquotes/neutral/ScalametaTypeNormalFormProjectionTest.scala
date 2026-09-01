package quasiquotes.neutral

import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypeNormalFormProjectionTest extends munit.FunSuite:
  test("projects a supported direct Type name through the existing Core normal form"):
    assertEquals(
      ScalametaTypeNormalFormProjection.project(Type.Name("Int")),
      Right(ProjectedTypeNormalForm(STypeIdent("Int"), None))
    )

  test("projects every supported simple name from the Core normal-form matrix"):
    List("Int", "String", "Boolean", "AnyVal").foreach { name =>
      assertEquals(project(Type.Name(name)).normalForm, STypeIdent(name))
    }

  test("projects every admitted direct applied constructor at its fixed arity"):
    val fixtures = List(
      "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      "Option[String]" -> STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
      "Either[Int, Boolean]" ->
        STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("Boolean")))
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parseType(source)).normalForm, expected, clues(source))
    }

  test("projects Tuple2, Tuple3, Function1, and Function2 through Core semantics"):
    val fixtures = List(
      "(Int, String)" -> STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      "(Int, String, Boolean)" ->
        STypeTuple(List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))),
      "Int => String" -> STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
      "(Int, String) => Boolean" ->
        STypeFunction(List(STypeIdent("Int"), STypeIdent("String")), STypeIdent("Boolean"))
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parseType(source)).normalForm, expected, clues(source))
    }

  test("recursively projects nested applications, tuples, and functions"):
    val expected =
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
          STypeApply(
            STypeIdent("Option"),
            List(
              STypeFunction(
                List(STypeTuple(List(STypeIdent("String"), STypeIdent("Boolean")))),
                STypeIdent("AnyVal")
              )
            )
          )
        )
      )

    assertEquals(
      project(parseType("Either[List[Int], Option[((String, Boolean)) => AnyVal]]")).normalForm,
      expected
    )

  test("preserves the exact positioned root span and never synthesizes an unpositioned span"):
    val positioned = parseType("Either[List[Int], Option[String]]")
    assertEquals(
      project(positioned).sourceSpan,
      Some(NeutralSourceSpan(0, 33))
    )

    val unpositioned = positioned match
      case applied: Type.Apply => applied.copy()
      case other => fail(s"expected Type.Apply fixture, found ${other.productPrefix}")
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("separates Core normal-form rejection from unsupported Scalameta topology"):
    val coreRejected = List(
      "Double",
      "scala.Int",
      "scala.List[Int]",
      "Map[Int, String]",
      "Either[Int]",
      "(Int, String, Boolean, AnyVal)",
      "(Int, String, Boolean) => AnyVal"
    )
    coreRejected.foreach { source =>
      assertErrorCode(source, "NEUTRAL_TYPE_NORMAL_FORM_REJECTED")
    }

    val structurallyUnsupported = List(
      "Int { type Out = String }",
      "Int | String",
      "Int & String",
      "?",
      "List[Int | String]"
    )
    structurallyUnsupported.foreach { source =>
      assertErrorCode(source, "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED")
    }

  test("rejects missing input deterministically"):
    assertEquals(
      ScalametaTypeNormalFormProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TYPE_MISSING",
          "the Scalameta type must be present."
        )
      )
    )

  private def parseType(source: String): Type =
    Input.String(source).parse[Type].get

  private def project(sourceType: Type): ProjectedTypeNormalForm =
    ScalametaTypeNormalFormProjection.project(sourceType) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: String, expected: String): Unit =
    assertEquals(
      ScalametaTypeNormalFormProjection.project(parseType(source)).left.toOption.map(_.code),
      Some(expected),
      clues(source)
    )
