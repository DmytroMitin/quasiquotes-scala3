package quasiquotes.publicapi

final class Q010FixedAppliedSingleParameterDefinitionCoreTest extends munit.FunSuite:
  private val body = right(CompletedTerm.definitionParameterReference("value"))
  private val int = named("Int")
  private val string = named("String")

  private val admitted = Vector(
    applied("List", int) -> "List[Int]",
    applied("Option", string) -> "Option[String]",
    applied("Either", int, string) -> "Either[Int, String]",
    applied("List", applied("Option", int)) -> "List[Option[Int]]",
    applied(
      "Either",
      applied("List", int),
      applied("Option", string)
    ) -> "Either[List[Int], Option[String]]"
  )

  test("single-parameter Definition construction admits the fixed applied family"):
    admitted.foreach { (completedType, source) =>
      val result = right(
        DefinitionConstruction.singleParameterMethod(
          "identity",
          "value",
          completedType,
          completedType,
          body
        )
      )

      assertEquals(result.kindCode, "single-parameter-method", source)
      assertEquals(result.name, "identity", source)
      assertEquals(result.parameterName, "value", source)
      assertEquals(result.parameterType, completedType, source)
      assertEquals(result.resultType, completedType, source)
      assertEquals(result.body, body, source)
      assertEquals(
        result.source,
        s"def identity(value: $source): $source = value",
        source
      )
    }

  test("single-parameter Definition construction keeps unsupported applied families closed"):
    val rejected = Vector(
      applied("Vector", int) -> "Vector[Int]",
      applied("List", int, string) -> "List[Int, String]",
      applied("Either", int) -> "Either[Int]",
      applied("Tuple2", int, string) -> "Tuple2[Int, String]",
      applied("Tuple3", int, string, named("Boolean")) -> "Tuple3[Int, String, Boolean]",
      applied("Function1", int, string) -> "Function1[Int, String]",
      applied("Function2", int, named("Boolean"), string) -> "Function2[Int, Boolean, String]"
    )

    rejected.foreach { (completedType, source) =>
      val failure = DefinitionConstruction
        .singleParameterMethod(
          "identity",
          "value",
          completedType,
          completedType,
          body
        )
        .left
        .toOption
        .getOrElse(fail(s"expected $source rejection"))

      assertEquals(failure.code, "invalid-single-parameter-method-contract", source)
      assertEquals(failure.anchor.map(_.componentCode), Some("parameter-type"), source)
    }

  test("single-parameter Definition construction keeps unequal unsupported and malformed inputs closed"):
    val unequal = DefinitionConstruction.singleParameterMethod(
      "identity",
      "value",
      applied("List", int),
      applied("List", string),
      body
    )
    assertEquals(unequal.left.map(_.code), Left("invalid-single-parameter-method-contract"))
    assertEquals(unequal.left.toOption.flatMap(_.anchor).map(_.componentCode), Some("result-type"))

    val unsupported = named("AnyVal")
    assert(DefinitionConstruction.singleParameterMethod(
      "identity", "value", unsupported, unsupported, body
    ).isLeft)

    val typeParameter = right(CompletedType.typeParameter("A"))
    assert(DefinitionConstruction.singleParameterMethod(
      "identity", "value", typeParameter, typeParameter, body
    ).isLeft)

    assert(DefinitionConstruction.singleParameterMethod(
      "identity", "value", null, int, body
    ).isLeft)
    assert(DefinitionConstruction.singleParameterMethod(
      "identity", "value", int, null, body
    ).isLeft)
    assert(DefinitionConstruction.singleParameterMethod(
      "identity", "value", int, int, null
    ).isLeft)
    assert(CompletedType.applied(null, Vector(int)).isLeft)
    assert(CompletedType.applied(named("List"), Vector.empty).isLeft)
    assert(CompletedType.applied(named("List"), Vector(null)).isLeft)

  test("two-parameter fixed applied behavior remains the Q009 bare-constructor rejection"):
    val listInt = applied("List", int)
    val failure = DefinitionConstruction
      .twoParameterMethod(
        "first",
        "left",
        listInt,
        "right",
        int,
        listInt,
        right(CompletedTerm.definitionParameterReference("left"))
      )
      .left
      .toOption
      .getOrElse(fail("expected unchanged two-parameter rejection"))

    assertEquals(failure.code, "invalid-two-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("parameter-type"))
    assert(failure.message.contains("Unsupported type-construction identifier `List`"))

  test("contextual applied transport remains semantically unchanged"):
    val listInt = applied("List", int)
    val service = right(CompletedTerm.reference("service"))
    val result = right(
      DefinitionConstruction.contextualMethod(
        "apply",
        "A",
        "service",
        listInt,
        listInt,
        service
      )
    )

    assertEquals(result.contextualParameterType, listInt)
    assertEquals(result.resultType, listInt)
    assertEquals(result.body, service)
    assertEquals(
      result.toString,
      "def apply[A](using service: List[Int]): List[Int] = service"
    )

  private def named(value: String): CompletedType =
    right(CompletedType.named(value))

  private def applied(constructor: String, arguments: CompletedType*): CompletedType =
    right(CompletedType.applied(named(constructor), arguments.toVector))

  private def right[A](value: Either[PublicFailure, A]): A =
    value.fold(failure => fail(failure.message), identity)
