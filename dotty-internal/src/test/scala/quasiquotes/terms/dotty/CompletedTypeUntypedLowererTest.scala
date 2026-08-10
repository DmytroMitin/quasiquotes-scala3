package quasiquotes.terms.dotty

import quasiquotes.parser.{TinyTypeParser, TypeShapeInspector}
import quasiquotes.types.TypeNormalForm

class CompletedTypeUntypedLowererTest extends munit.FunSuite:
  import CompletedTypeUntypedLoweringError.*
  import TypeNormalForm.*

  private val fixtures = Vector(
    "Int" -> STypeIdent("Int"),
    "String" -> STypeIdent("String"),
    "Boolean" -> STypeIdent("Boolean"),
    "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
    "Option[String]" ->
      STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
    "List[Option[Int]]" ->
      STypeApply(
        STypeIdent("List"),
        List(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))
      ),
    "Either[Int, String]" ->
      STypeApply(
        STypeIdent("Either"),
        List(STypeIdent("Int"), STypeIdent("String"))
      ),
    "Either[List[Int], Option[String]]" ->
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
          STypeApply(STypeIdent("Option"), List(STypeIdent("String")))
        )
      ),
    "(Int, String)" ->
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
    "(Int, String, Boolean)" ->
      STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ),
    "Int => String" ->
      STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
    "(Int, String) => Boolean" ->
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      )
  )

  fixtures.foreach { case (source, normalForm) =>
    test(s"authoritative completed-type lowerer matches parser raw structure: $source") {
      val raw = CompletedTypeUntypedLowerer.lower(normalForm).toOption.get

      assertEquals(
        TypeShapeInspector.rawStructure(raw),
        TinyTypeParser.parseOrThrow(source).rawStructure
      )
      assert(!raw.source.exists)
      assert(!raw.span.exists)
    }
  }

  test("authoritative completed-type lowerer rejects a defensive invalid normal form") {
    val invalid = STypeIdent("AnyVal")

    assertEquals(
      CompletedTypeUntypedLowerer.lower(invalid),
      Left(UnsupportedCompletedType(invalid.render))
    )
    assertEquals(
      UnsupportedCompletedType(invalid.render).message,
      "Unsupported completed type at the exact-version untyped backend boundary: STypeIdent(AnyVal)."
    )
  }

  test("authoritative completed-type lowerer rejects wrong applied arities without MatchError") {
    val invalid = List(
      STypeApply(STypeIdent("Either"), List(STypeIdent("Int"))),
      STypeApply(
        STypeIdent("Either"),
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ),
      STypeApply(
        STypeIdent("List"),
        List(STypeIdent("Int"), STypeIdent("String"))
      )
    )

    invalid.foreach { normalForm =>
      assertEquals(
        CompletedTypeUntypedLowerer.lower(normalForm),
        Left(UnsupportedCompletedType(normalForm.render))
      )
    }
  }
