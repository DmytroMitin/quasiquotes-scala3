package quasiquotes.publicapi

import quasiquotes.parser.TypeShape
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class C010CompletedTypeRepresentationContractProbeTest extends munit.FunSuite:
  import TypeNormalForm.*

  private val int = STypeIdent("Int")
  private val string = STypeIdent("String")
  private val boolean = STypeIdent("Boolean")

  test("structured shapes distinguish tuple and function kinds from explicit generic applications"):
    val tupleSyntax: TypeShape = TypeShape.Tuple(
      List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
    )
    val tupleExplicit: TypeShape = TypeShape.Apply(
      TypeShape.Identifier("Tuple2"),
      List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
    )
    val functionSyntax: TypeShape = TypeShape.Function(
      List(TypeShape.Identifier("Int")),
      TypeShape.Identifier("String")
    )
    val functionExplicit: TypeShape = TypeShape.Apply(
      TypeShape.Identifier("Function1"),
      List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
    )

    assertNotEquals(tupleSyntax, tupleExplicit)
    assertNotEquals(functionSyntax, functionExplicit)

    assertEquals(
      TypeNormalForm.fromShape(tupleSyntax),
      Right(STypeTuple(List(int, string)))
    )
    assertEquals(
      TypeNormalForm.fromShape(functionSyntax),
      Right(STypeFunction(List(int), string))
    )
    assert(TypeNormalForm.fromShape(tupleExplicit).isLeft)
    assert(TypeNormalForm.fromShape(functionExplicit).isLeft)

  test("current CompletedType transport collides semantic tuple and function kinds with generic applications"):
    val cases = List(
      (
        "tuple2",
        STypeTuple(List(int, string)),
        STypeApply(STypeIdent("Tuple2"), List(int, string)),
        "Tuple2[Int, String]"
      ),
      (
        "function1",
        STypeFunction(List(int), string),
        STypeApply(STypeIdent("Function1"), List(int, string)),
        "Function1[Int, String]"
      ),
      (
        "function2",
        STypeFunction(List(int, boolean), string),
        STypeApply(STypeIdent("Function2"), List(int, boolean, string)),
        "Function2[Int, Boolean, String]"
      )
    )

    cases.foreach { (label, semantic, explicitGeneric, source) =>
      assert(TypeTemplate.validateConstructed(semantic).isRight, label)
      assert(TypeTemplate.validateConstructed(explicitGeneric).isLeft, label)

      val semanticTransport = currentTransport(semantic)
      val explicitTransport = currentTransport(explicitGeneric)
      assertEquals(semanticTransport, explicitTransport, label)
      assertEquals(semanticTransport.kindCode, "applied", label)
      assertEquals(semanticTransport.source, source, label)
    }

  test("nested tuple and function identity is also lost by current CompletedType transport"):
    val semantic = List(
      STypeApply(STypeIdent("Option"), List(STypeTuple(List(int, string)))),
      STypeApply(STypeIdent("List"), List(STypeFunction(List(int), string))),
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeTuple(List(int, string)),
          STypeFunction(List(boolean), int)
        )
      )
    )
    val explicitGeneric = List(
      STypeApply(
        STypeIdent("Option"),
        List(STypeApply(STypeIdent("Tuple2"), List(int, string)))
      ),
      STypeApply(
        STypeIdent("List"),
        List(STypeApply(STypeIdent("Function1"), List(int, string)))
      ),
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeApply(STypeIdent("Tuple2"), List(int, string)),
          STypeApply(STypeIdent("Function1"), List(boolean, int))
        )
      )
    )

    semantic.zip(explicitGeneric).foreach { (semanticForm, explicitForm) =>
      assert(TypeTemplate.validateConstructed(semanticForm).isRight)
      assert(TypeTemplate.validateConstructed(explicitForm).isLeft)
      assertEquals(currentTransport(semanticForm), currentTransport(explicitForm))
    }

    assertEquals(
      semantic.map(currentTransport(_).source),
      List(
        "Option[Tuple2[Int, String]]",
        "List[Function1[Int, String]]",
        "Either[Tuple2[Int, String], Function1[Boolean, Int]]"
      )
    )

  private def currentTransport(normalForm: TypeNormalForm): CompletedType =
    normalForm match
      case STypeIdent(name) => named(name)
      case STypeResolved(id) => fail(s"C010 does not transport resolved Type `${id.canonicalSource}`")
      case STypeApply(constructor, arguments) =>
        applied(currentTransport(constructor), arguments.map(currentTransport).toVector)
      case STypeTuple(elements) =>
        applied(named(s"Tuple${elements.size}"), elements.map(currentTransport).toVector)
      case STypeFunction(arguments, result) =>
        applied(
          named(s"Function${arguments.size}"),
          (arguments :+ result).map(currentTransport).toVector
        )

  private def named(value: String): CompletedType =
    CompletedType.named(value).fold(failure => fail(failure.message), identity)

  private def applied(
      constructor: CompletedType,
      arguments: Vector[CompletedType]
  ): CompletedType =
    CompletedType.applied(constructor, arguments).fold(failure => fail(failure.message), identity)
