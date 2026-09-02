package quasiquotes.neutral

import quasiquotes.parser.TermShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypeNormalFormAuthoringTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      normalForm: TypeNormalForm,
      expectedKind: String
  )

  private val fixtures = List(
    Fixture("Int", STypeIdent("Int"), "Type.Name"),
    Fixture("String", STypeIdent("String"), "Type.Name"),
    Fixture("Boolean", STypeIdent("Boolean"), "Type.Name"),
    Fixture("AnyVal", STypeIdent("AnyVal"), "Type.Name"),
    Fixture(
      "List[Int]",
      STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      "Type.Apply"
    ),
    Fixture(
      "Option[String]",
      STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
      "Type.Apply"
    ),
    Fixture(
      "Either[Int, String]",
      STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("String"))),
      "Type.Apply"
    ),
    Fixture(
      "Either[List[Int], Option[String]]",
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
          STypeApply(STypeIdent("Option"), List(STypeIdent("String")))
        )
      ),
      "Type.Apply"
    ),
    Fixture(
      "(Int, String)",
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      "Type.Tuple"
    ),
    Fixture(
      "(Int, String, Boolean)",
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))),
      "Type.Tuple"
    ),
    Fixture(
      "Int => String",
      STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
      "Type.Function"
    ),
    Fixture(
      "(Int, String) => Boolean",
      STypeFunction(List(STypeIdent("Int"), STypeIdent("String")), STypeIdent("Boolean")),
      "Type.Function"
    )
  )

  test("authors every admitted family to the expected Scalameta category"):
    fixtures.foreach { fixture =>
      assertEquals(author(fixture.normalForm).productPrefix, fixture.expectedKind, clues(fixture.source))
    }

  test("preserves nested application tuple and function topology in order"):
    val normalForm = STypeApply(
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

    val either = author(normalForm).asInstanceOf[Type.Apply]
    assertEquals(either.tpe.asInstanceOf[Type.Name].value, "Either")
    assertEquals(either.args.map(_.productPrefix), List("Type.Apply", "Type.Apply"))

    val list = either.args.head.asInstanceOf[Type.Apply]
    assertEquals(list.tpe.asInstanceOf[Type.Name].value, "List")
    assertEquals(list.args.map(_.asInstanceOf[Type.Name].value), List("Int"))

    val option = either.args(1).asInstanceOf[Type.Apply]
    assertEquals(option.tpe.asInstanceOf[Type.Name].value, "Option")
    val function = option.args.head.asInstanceOf[Type.Function]
    val tuple = function.params.head.asInstanceOf[Type.Tuple]
    assertEquals(tuple.args.map(_.asInstanceOf[Type.Name].value), List("String", "Boolean"))
    assertEquals(function.res.asInstanceOf[Type.Name].value, "AnyVal")

  test("authors fresh unpositioned roots children and synthetic clauses"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.normalForm)
      assert(allTypeNodes(authored).forall(_.pos == Position.None), clues(fixture.source))
      allTypeNodes(authored).foreach {
        case applied: Type.Apply => assertEquals(applied.argClause.pos, Position.None)
        case function: Type.Function => assertEquals(function.paramClause.pos, Position.None)
        case _ => ()
      }
    }

  test("round-trips every admitted N011 normal form through N002 unchanged"):
    fixtures.foreach { fixture =>
      assertEquals(project(author(fixture.normalForm)), fixture.normalForm, clues(fixture.source))
    }

  test("round-trips representative N002 source Types through N011 and N002 unchanged"):
    fixtures.foreach { fixture =>
      val initial = project(parseType(fixture.source))
      assertEquals(initial, fixture.normalForm, clues(fixture.source))
      assertEquals(project(author(initial)), initial, clues(fixture.source))
    }

  test("does not collapse distinct normal forms to the same authored Type"):
    val listInt = author(STypeApply(STypeIdent("List"), List(STypeIdent("Int"))))
    val optionInt = author(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))
    val tuple = author(STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))))
    val function = author(STypeFunction(List(STypeIdent("Int")), STypeIdent("String")))

    assertNotEquals(listInt.structure, optionInt.structure)
    assertNotEquals(tuple.structure, function.structure)

  test("rejects a missing root with a bounded authoring error"):
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(null),
      Left(
        ScalametaTypeNormalFormAuthoring.Error(
          "NEUTRAL_TYPE_AUTHORING_MISSING",
          "the Type normal form must be present."
        )
      )
    )

  test("fails closed for malformed unresolved normal forms"):
    val rejected = List(
      STypeIdent("Double"),
      STypeApply(STypeIdent("Map"), List(STypeIdent("Int"), STypeIdent("String"))),
      STypeApply(STypeIdent("List"), Nil),
      STypeApply(STypeIdent("List"), List(STypeIdent("Int"), STypeIdent("String"))),
      STypeApply(STypeIdent("Option"), Nil),
      STypeApply(STypeIdent("Option"), List(STypeIdent("Int"), STypeIdent("String"))),
      STypeApply(STypeIdent("Either"), List(STypeIdent("Int"))),
      STypeApply(
        STypeIdent("Either"),
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ),
      STypeTuple(List(STypeIdent("Int"))),
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"), STypeIdent("AnyVal"))),
      STypeFunction(Nil, STypeIdent("String")),
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean")),
        STypeIdent("AnyVal")
      ),
      STypeApply(STypeIdent("List"), List(STypeIdent("Double")))
    )

    rejected.foreach { normalForm =>
      assertEquals(
        ScalametaTypeNormalFormAuthoring.author(normalForm).left.toOption.map(_.code),
        Some("NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED"),
        clues(normalForm)
      )
    }

  test("rejects resolved roots constructors and recursive children without re-spelling identity"):
    val resolved = STypeResolved(
      ResolvedTypeNameId(
        Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")),
        "Int"
      )
    )
    val rejected = List(
      resolved,
      STypeApply(resolved, List(STypeIdent("Int"))),
      STypeApply(STypeIdent("List"), List(resolved)),
      STypeTuple(List(STypeIdent("Int"), resolved)),
      STypeFunction(List(resolved), STypeIdent("String"))
    )

    rejected.foreach { normalForm =>
      val error = ScalametaTypeNormalFormAuthoring.author(normalForm).left.toOption.get
      assertEquals(error.code, "NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED", clues(normalForm))
      assert(!error.detail.contains("scala.Int"), clues(error.detail))
    }

  test("composes with N010 ascription projection without adding Term authoring"):
    val source = parseTerm("x: Either[List[Int], Option[String]]").asInstanceOf[Term.Ascribe]
    val projectedTerm = ScalametaTermProjection.project(source).toOption.get
    val normalForm = project(source.tpe)

    assertEquals(
      projectedTerm.shape,
      TermShape.Typed(
        TermShape.Identifier("x", isPlaceholder = false),
        "Either[List[Int], Option[String]]"
      )
    )
    assertEquals(project(author(normalForm)), normalForm)

  private def parseType(source: String): Type =
    Input.String(source).parse[Type].get

  private def parseTerm(source: String): Term =
    Input.String(source).parse[Term].get

  private def author(normalForm: TypeNormalForm): Type =
    ScalametaTypeNormalFormAuthoring.author(normalForm) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def project(sourceType: Type): TypeNormalForm =
    ScalametaTypeNormalFormProjection.project(sourceType) match
      case Right(value) => value.normalForm
      case Left(error) => fail(error.message)

  private def allTypeNodes(root: Type): List[Type] =
    root :: (root match
      case _: Type.Name => Nil
      case applied: Type.Apply =>
        allTypeNodes(applied.tpe) ++ applied.args.flatMap(allTypeNodes)
      case tuple: Type.Tuple =>
        tuple.args.flatMap(allTypeNodes)
      case function: Type.Function =>
        function.params.flatMap(allTypeNodes) ++ allTypeNodes(function.res)
      case other => fail(s"unexpected authored node: ${other.productPrefix}")
    )
