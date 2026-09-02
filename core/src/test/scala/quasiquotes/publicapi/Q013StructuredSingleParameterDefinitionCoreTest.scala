package quasiquotes.publicapi

import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}
import quasiquotes.types.TypeNormalForm.*

final class Q013StructuredSingleParameterDefinitionCoreTest extends munit.FunSuite:
  private val int = STypeIdent("Int")
  private val string = STypeIdent("String")
  private val boolean = STypeIdent("Boolean")
  private val body = CompletedTerm.definitionParameterReference("value").toOption.get

  private val admitted = List(
    STypeTuple(List(int, string)),
    STypeTuple(List(int, string, boolean)),
    STypeFunction(List(int), string),
    STypeFunction(List(int, boolean), string),
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

  test("package-private Definition seam preserves complete tuple function normal forms"):
    admitted.foreach { normalForm =>
      val result = DefinitionConstruction
        .constructSingleParameterMethodFromNormalForms(
          "identity",
          "value",
          normalForm,
          normalForm,
          body
        )
        .fold(failure => fail(failure.message), identity)

      assertEquals(result.name.decoded, "identity", normalForm.render)
      assertEquals(result.parameterName.decoded, "value", normalForm.render)
      assertEquals(result.parameterType, normalForm, normalForm.render)
      assertEquals(result.resultType, normalForm, normalForm.render)
    }

  test("package-private Definition seam keeps unsupported unequal selected and malformed forms closed"):
    val scalaOwner = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")
    val resolved = STypeResolved(ResolvedTypeNameId(Vector(scalaOwner), "Int"))
    val malformed = STypeTuple(List(int, null.asInstanceOf[TypeNormalForm]))
    val rejected = List(
      (STypeIdent("AnyVal"), STypeIdent("AnyVal")),
      (STypeIdent("Double"), STypeIdent("Double")),
      (STypeApply(STypeIdent("Map"), List(int, string)), STypeApply(STypeIdent("Map"), List(int, string))),
      (STypeApply(STypeIdent("List"), List(int, string)), STypeApply(STypeIdent("List"), List(int, string))),
      (STypeTuple(List(int)), STypeTuple(List(int))),
      (STypeFunction(List.empty, string), STypeFunction(List.empty, string)),
      (resolved, resolved),
      (malformed, malformed),
      (null.asInstanceOf[TypeNormalForm], int),
      (int, null.asInstanceOf[TypeNormalForm]),
      (STypeTuple(List(int, string)), STypeTuple(List(int, boolean)))
    )

    rejected.foreach { (parameterType, resultType) =>
      val result = DefinitionConstruction.constructSingleParameterMethodFromNormalForms(
        "identity",
        "value",
        parameterType,
        resultType,
        body
      )
      assert(result.isLeft, String.valueOf(parameterType))
      assertEquals(result.left.toOption.map(_.code), Some("invalid-single-parameter-method-contract"))
    }

  test("public CompletedType generic TupleN and FunctionN applications remain rejected"):
    val completedInt = CompletedType.named("Int").toOption.get
    val completedString = CompletedType.named("String").toOption.get
    val genericTypes = List(
      CompletedType.applied(
        CompletedType.named("Tuple2").toOption.get,
        Vector(completedInt, completedString)
      ).toOption.get,
      CompletedType.applied(
        CompletedType.named("Function1").toOption.get,
        Vector(completedInt, completedString)
      ).toOption.get
    )

    genericTypes.foreach { completedType =>
      val result = DefinitionConstruction.singleParameterMethod(
        "identity",
        "value",
        completedType,
        completedType,
        body
      )
      assert(result.isLeft, completedType.source)
      assertEquals(result.left.toOption.flatMap(_.anchor).map(_.componentCode), Some("parameter-type"))
    }
