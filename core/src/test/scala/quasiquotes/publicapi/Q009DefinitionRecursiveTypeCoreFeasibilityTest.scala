package quasiquotes.publicapi

import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class Q009DefinitionRecursiveTypeCoreFeasibilityTest extends munit.FunSuite:
  private final case class Family(
      label: String,
      normalForm: TypeNormalForm,
      completedSource: String,
      firstRejectedConstructor: Option[String]
  )

  private val int = TypeNormalForm.STypeIdent("Int")
  private val string = TypeNormalForm.STypeIdent("String")
  private val boolean = TypeNormalForm.STypeIdent("Boolean")

  private val families = List(
    Family("Int", int, "Int", None),
    Family("String", string, "String", None),
    Family("Boolean", boolean, "Boolean", None),
    Family(
      "List[Int]",
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(int)),
      "List[Int]",
      Some("List")
    ),
    Family(
      "Option[String]",
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(string)),
      "Option[String]",
      Some("Option")
    ),
    Family(
      "Either[Int, String]",
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Either"), List(int, string)),
      "Either[Int, String]",
      Some("Either")
    ),
    Family(
      "List[Option[Int]]",
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(int)))
      ),
      "List[Option[Int]]",
      Some("List")
    ),
    Family(
      "Either[List[Int], Option[String]]",
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Either"),
        List(
          TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(int)),
          TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(string))
        )
      ),
      "Either[List[Int], Option[String]]",
      Some("Either")
    ),
    Family(
      "(Int, String)",
      TypeNormalForm.STypeTuple(List(int, string)),
      "Tuple2[Int, String]",
      Some("Tuple2")
    ),
    Family(
      "(Int, String, Boolean)",
      TypeNormalForm.STypeTuple(List(int, string, boolean)),
      "Tuple3[Int, String, Boolean]",
      Some("Tuple3")
    ),
    Family(
      "Int => String",
      TypeNormalForm.STypeFunction(List(int), string),
      "Function1[Int, String]",
      Some("Function1")
    ),
    Family(
      "(Int, Boolean) => String",
      TypeNormalForm.STypeFunction(List(int, boolean), string),
      "Function2[Int, Boolean, String]",
      Some("Function2")
    )
  )

  test("complete normal forms admit every required recursive family"):
    families.foreach { family =>
      assert(
        TypeTemplate.validateConstructed(family.normalForm).isRight,
        s"${family.label} complete normal form was unexpectedly rejected"
      )
    }

  test("the current CompletedType conversion preserves only named and generic applied kinds"):
    families.foreach { family =>
      val completed = toCurrentCompletedType(family.normalForm)
      assertEquals(completed.source, family.completedSource, family.label)
      assertEquals(
        completed.kindCode,
        if family.firstRejectedConstructor.isEmpty then "named" else "applied",
        family.label
      )
    }

    val tuple = toCurrentCompletedType(TypeNormalForm.STypeTuple(List(int, string)))
    val function = toCurrentCompletedType(TypeNormalForm.STypeFunction(List(int), string))
    assertEquals(tuple.constructor.flatMap(_.name), Some("Tuple2"))
    assertEquals(function.constructor.flatMap(_.name), Some("Function1"))

  test("public DefinitionConstruction rejects recursive children before the complete form"):
    val body = CompletedTerm.definitionParameterReference("value").toOption.get

    families.foreach { family =>
      val completed = toCurrentCompletedType(family.normalForm)
      val result = DefinitionConstruction.singleParameterMethod(
        "identity",
        "value",
        completed,
        completed,
        body
      )

      family.firstRejectedConstructor match
        case None => assert(result.isRight, family.label)
        case Some(constructor) =>
          assert(result.isLeft, family.label)
          assert(
            result.left.toOption.get.message.contains(
              s"Unsupported type-construction identifier `$constructor`"
            ),
            s"${family.label}: ${result.left.toOption.get.message}"
          )
    }

  test("negative controls retain unequal-Type and unsupported-Type rejection"):
    val body = CompletedTerm.definitionParameterReference("value").toOption.get
    val completedInt = CompletedType.named("Int").toOption.get
    val completedString = CompletedType.named("String").toOption.get
    val unequal = DefinitionConstruction.singleParameterMethod(
      "identity",
      "value",
      completedInt,
      completedString,
      body
    )
    assert(unequal.isLeft)
    assert(unequal.left.toOption.get.message.contains("result type to equal the parameter type"))

    val anyVal = TypeNormalForm.STypeIdent("AnyVal")
    assert(TypeTemplate.validateConstructed(anyVal).isLeft)
    val unsupported = DefinitionConstruction.singleParameterMethod(
      "identity",
      "value",
      CompletedType.named("AnyVal").toOption.get,
      CompletedType.named("AnyVal").toOption.get,
      body
    )
    assert(unsupported.isLeft)
    assert(unsupported.left.toOption.get.message.contains("Unsupported type-construction identifier `AnyVal`"))

  private def toCurrentCompletedType(normalForm: TypeNormalForm): CompletedType =
    normalForm match
      case TypeNormalForm.STypeIdent(name) =>
        CompletedType.named(name).toOption.get
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        CompletedType
          .applied(
            toCurrentCompletedType(constructor),
            arguments.map(toCurrentCompletedType).toVector
          )
          .toOption
          .get
      case TypeNormalForm.STypeTuple(elements) =>
        CompletedType
          .applied(
            CompletedType.named(s"Tuple${elements.size}").toOption.get,
            elements.map(toCurrentCompletedType).toVector
          )
          .toOption
          .get
      case TypeNormalForm.STypeFunction(arguments, result) =>
        CompletedType
          .applied(
            CompletedType.named(s"Function${arguments.size}").toOption.get,
            (arguments :+ result).map(toCurrentCompletedType).toVector
          )
          .toOption
          .get
      case TypeNormalForm.STypeResolved(id) =>
        fail(s"Q009 does not admit resolved Type `${id.canonicalSource}` into CompletedType")
