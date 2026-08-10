package quasiquotes.types

import quasiquotes.parser.TypeShape

class AppliedTypeStructuralCoreTest extends munit.FunSuite:
  import TypeNormalForm.*
  import TypePattern.*
  import TypeTemplate.*

  private val intForm = STypeIdent("Int")
  private val stringForm = STypeIdent("String")
  private val booleanForm = STypeIdent("Boolean")

  test("central policy admits only fixed constructors at fixed arity"):
    assertEquals(
      AppliedTypeConstructorPolicy.forNormalFormSource("List", 1).map(_.requiredArity),
      Some(1)
    )
    assertEquals(
      AppliedTypeConstructorPolicy.forConstruction("Either", 2).map(_.requiredArity),
      Some(2)
    )
    assertEquals(AppliedTypeConstructorPolicy.forNormalFormSource("Either", 1), None)
    assertEquals(AppliedTypeConstructorPolicy.forNormalFormSource("Map", 2), None)

  test("normal form recursively admits nested unary and binary applications"):
    val shape = apply(
      "Either",
      apply("Option", ident("Int")),
      apply("List", apply("Either", ident("String"), ident("Boolean")))
    )
    assertEquals(
      TypeNormalForm.fromShape(shape),
      Right(
        STypeApply(
          STypeIdent("Either"),
          List(
            STypeApply(STypeIdent("Option"), List(intForm)),
            STypeApply(
              STypeIdent("List"),
              List(
                STypeApply(
                  STypeIdent("Either"),
                  List(stringForm, booleanForm)
                )
              )
            )
          )
        )
      )
    )

  test("normal form rejects unsupported constructors and wrong arities"):
    List(
      apply("Map", ident("Int"), ident("String")),
      apply("Either", ident("Int")),
      apply("Either", ident("Int"), ident("String"), ident("Boolean")),
      apply("List", ident("Int"), ident("String"))
    ).foreach(shape => assert(TypeNormalForm.fromShape(shape).isLeft))

  test("generic TPApply matches ordered nested arguments and repeated holes"):
    val repeated = TPApply(
      TPIdent("Either"),
      List(
        TPApply(TPIdent("List"), List(TPHole("a"))),
        TPApply(TPIdent("Option"), List(TPHole("a")))
      )
    )
    val success = STypeApply(
      STypeIdent("Either"),
      List(
        STypeApply(STypeIdent("List"), List(intForm)),
        STypeApply(STypeIdent("Option"), List(intForm))
      )
    )
    val failure = STypeApply(
      STypeIdent("Either"),
      List(
        STypeApply(STypeIdent("List"), List(intForm)),
        STypeApply(STypeIdent("Option"), List(stringForm))
      )
    )
    assertEquals(TypePattern.matchNormalForm(repeated, success), Some(TypeMatchResult(Map("a" -> intForm))))
    assertEquals(TypePattern.matchNormalForm(repeated, failure), None)

  test("generic TTApply constructs and validates recursive Either values"):
    val template = TTApply(
      TTIdent("Either"),
      List(
        TTApply(TTIdent("List"), List(TTHole("a"))),
        TTApply(TTIdent("Option"), List(TTHole("b")))
      )
    )
    val constructed = TypeTemplate
      .construct(template, Map("a" -> intForm, "b" -> stringForm))
      .toOption
      .get
    assertEquals(ConstructedType(constructed).source, "Either[List[Int], Option[String]]")
    assertEquals(TypeTemplate.validateConstructed(constructed), Right(()))
    assertEquals(TypeTemplate.requiredBindings(template), Vector("a", "b"))

  test("structural equality preserves constructor, nesting, grouping, and order"):
    val eitherIntString = STypeApply(STypeIdent("Either"), List(intForm, stringForm))
    assertNotEquals(eitherIntString, STypeApply(STypeIdent("Either"), List(stringForm, intForm)))
    assertNotEquals(eitherIntString, STypeApply(STypeIdent("List"), List(intForm)))
    assertNotEquals(
      STypeApply(STypeIdent("List"), List(STypeApply(STypeIdent("Option"), List(intForm)))),
      STypeApply(STypeIdent("Option"), List(STypeApply(STypeIdent("List"), List(intForm))))
    )
    assertNotEquals(
      STypeApply(STypeIdent("Either"), List(STypeTuple(List(intForm, stringForm)), booleanForm)),
      STypeApply(STypeIdent("Either"), List(intForm, STypeTuple(List(stringForm, booleanForm))))
    )

  private def ident(name: String): TypeShape = TypeShape.Identifier(name)

  private def apply(name: String, arguments: TypeShape*): TypeShape =
    TypeShape.Apply(TypeShape.Identifier(name), arguments.toList)
