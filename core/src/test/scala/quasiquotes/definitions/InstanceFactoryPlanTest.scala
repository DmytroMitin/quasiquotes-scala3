package quasiquotes.definitions

import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

final class InstanceFactoryPlanTest extends munit.FunSuite:
  test("creates the exact five-binder factory role graph"):
    val plan = validPlan()

    assertEquals(plan.factoryDisplayName, "instance")
    assertEquals(plan.typeParameter, TypeParameter(BinderId(0), "A"))
    assertEquals(plan.emptyValue.mode, ParameterMode.ByName)
    assertEquals(
      plan.emptyValue.valueType,
      ValueType(TypeParameterReference(BinderId(0), "A"))
    )
    assertEquals(plan.combineFunction.mode, ParameterMode.ByValue)
    assertEquals(
      plan.combineFunction.functionType,
      BinaryFunctionType(
        TypeParameterReference(BinderId(0), "A"),
        TypeParameterReference(BinderId(0), "A"),
        TypeParameterReference(BinderId(0), "A")
      )
    )
    assertEquals(
      plan.targetType,
      Applied(SourceName("Monoid"), Vector(TypeParameterReference(BinderId(0), "A")))
    )
    assertEquals(plan.emptyOverride.memberDisplayName, "empty")
    assertEquals(plan.emptyOverride.body, TermReference(BinderId(1)))
    assertEquals(plan.combineOverride.memberDisplayName, "combine")
    assertEquals(plan.combineOverride.firstParameter.binderId, BinderId(3))
    assertEquals(plan.combineOverride.secondParameter.binderId, BinderId(4))
    assertEquals(plan.combineOverride.body.callee, TermReference(BinderId(2)))
    assertEquals(
      plan.combineOverride.body.arguments,
      Vector(TermReference(BinderId(3)), TermReference(BinderId(4)))
    )

  test("fully renamed roles retain the same deterministic relationship graph"):
    val plan = validPlan(
      factoryName = "make",
      typeName = "Element",
      emptyCarrierName = "fallbackValue",
      functionCarrierName = "selection",
      targetName = "Choice",
      emptyMemberName = "fallback",
      combineMemberName = "select",
      firstNestedName = "left",
      secondNestedName = "right"
    )

    assertEquals(plan.typeParameter.binderId, BinderId(0))
    assertEquals(plan.emptyValue.binderId, BinderId(1))
    assertEquals(plan.combineFunction.binderId, BinderId(2))
    assertEquals(plan.combineOverride.firstParameter.binderId, BinderId(3))
    assertEquals(plan.combineOverride.secondParameter.binderId, BinderId(4))
    assertEquals(plan.emptyOverride.body.binderId, plan.emptyValue.binderId)
    assertEquals(plan.combineOverride.body.callee.binderId, plan.combineFunction.binderId)
    assertEquals(
      plan.combineOverride.body.arguments.map(_.binderId),
      Vector(
        plan.combineOverride.firstParameter.binderId,
        plan.combineOverride.secondParameter.binderId
      )
    )

  test("rejects missing values illegal names and non-distinct declaration binders"):
    val rows = List(
      invalidPlan(typeParameter = null) -> "TYPE_PARAMETER_MISSING",
      invalidPlan(emptyValue = null) -> "EMPTY_VALUE_CARRIER_MISSING",
      invalidPlan(combineFunction = null) -> "COMBINE_FUNCTION_CARRIER_MISSING",
      invalidPlan(targetType = null) -> "TARGET_TYPE_MISSING",
      invalidPlan(emptyOverride = null) -> "EMPTY_OVERRIDE_MISSING",
      invalidPlan(combineOverride = null) -> "COMBINE_OVERRIDE_MISSING",
      invalidPlan(factoryName = "bad-name") -> "FACTORY_NAME_INVALID",
      invalidPlan(typeName = "bad-name") -> "TYPE_PARAMETER_NAME_INVALID",
      invalidPlan(emptyCarrierName = "bad-name") -> "EMPTY_VALUE_CARRIER_NAME_INVALID",
      invalidPlan(functionCarrierName = "bad-name") -> "COMBINE_FUNCTION_CARRIER_NAME_INVALID",
      invalidPlan(targetName = "bad-name") -> "TARGET_TYPE_CONSTRUCTOR_INVALID",
      invalidPlan(emptyMemberName = "bad-name") -> "EMPTY_OVERRIDE_NAME_INVALID",
      invalidPlan(combineMemberName = "bad-name") -> "COMBINE_OVERRIDE_NAME_INVALID",
      invalidPlan(firstNestedName = "bad-name") -> "COMBINE_FIRST_PARAMETER_NAME_INVALID",
      invalidPlan(secondNestedName = "bad-name") -> "COMBINE_SECOND_PARAMETER_NAME_INVALID",
      invalidPlan(emptyBinder = BinderId(0)) -> "DECLARATION_BINDERS_MUST_BE_DISTINCT",
      invalidPlan(secondNestedBinder = BinderId(3)) -> "DECLARATION_BINDERS_MUST_BE_DISTINCT"
    )
    rows.foreach { case (result, code) => assertCode(result, code) }

  test("rejects every detached Type mode function and target relationship"):
    val rows = List(
      invalidPlan(emptyMode = ParameterMode.ByValue) -> "EMPTY_VALUE_MODE_MISMATCH",
      invalidPlan(emptyTypeBinder = BinderId(9)) -> "EMPTY_VALUE_TYPE_BINDER_MISMATCH",
      invalidPlan(emptyTypeName = "Other") -> "EMPTY_VALUE_TYPE_BINDER_MISMATCH",
      invalidPlan(functionMode = ParameterMode.ByName) -> "COMBINE_FUNCTION_MODE_MISMATCH",
      invalidPlan(functionFirstBinder = BinderId(9)) -> "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH",
      invalidPlan(functionSecondBinder = BinderId(9)) -> "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH",
      invalidPlan(functionResultBinder = BinderId(9)) -> "COMBINE_FUNCTION_TYPE_BINDER_MISMATCH",
      invalidPlan(targetType = SourceName("Monoid")) -> "TARGET_TYPE_UNSUPPORTED",
      invalidPlan(targetType = Applied(SourceName("Monoid"), Vector.empty)) ->
        "TARGET_TYPE_UNSUPPORTED",
      invalidPlan(targetType = Applied(SourceName("Monoid"), Vector(SourceName("A")))) ->
        "TARGET_TYPE_BINDER_MISMATCH",
      invalidPlan(targetTypeBinder = BinderId(9)) -> "TARGET_TYPE_BINDER_MISMATCH"
    )
    rows.foreach { case (result, code) => assertCode(result, code) }

  test("rejects wrong nested Types and every wrong body role or argument order"):
    val rows = List(
      invalidPlan(firstNestedTypeBinder = BinderId(9)) ->
        "COMBINE_PARAMETER_TYPE_BINDER_MISMATCH",
      invalidPlan(secondNestedTypeBinder = BinderId(9)) ->
        "COMBINE_PARAMETER_TYPE_BINDER_MISMATCH",
      invalidPlan(combineResultTypeBinder = BinderId(9)) ->
        "COMBINE_RESULT_TYPE_BINDER_MISMATCH",
      invalidPlan(emptyBodyBinder = BinderId(2)) -> "EMPTY_BODY_BINDER_MISMATCH",
      invalidPlan(combineCalleeBinder = BinderId(1)) -> "COMBINE_BODY_CALLEE_BINDER_MISMATCH",
      invalidPlan(combineArgumentBinders = Vector(BinderId(4), BinderId(3))) ->
        "COMBINE_BODY_ARGUMENT_BINDER_MISMATCH",
      invalidPlan(combineArgumentBinders = Vector(BinderId(3), BinderId(3))) ->
        "COMBINE_BODY_ARGUMENT_BINDER_MISMATCH",
      invalidPlan(combineArgumentBinders = Vector(BinderId(1), BinderId(4))) ->
        "COMBINE_BODY_ARGUMENT_BINDER_MISMATCH",
      invalidPlan(combineArgumentBinders = Vector(BinderId(3))) ->
        "COMBINE_BODY_ARGUMENT_BINDER_MISMATCH"
    )
    rows.foreach { case (result, code) => assertCode(result, code) }

  private def validPlan(
      factoryName: String = "instance",
      typeName: String = "A",
      emptyCarrierName: String = "emptyValue",
      functionCarrierName: String = "combineFunction",
      targetName: String = "Monoid",
      emptyMemberName: String = "empty",
      combineMemberName: String = "combine",
      firstNestedName: String = "a",
      secondNestedName: String = "a1"
  ): Plan =
    invalidPlan(
      factoryName = factoryName,
      typeName = typeName,
      emptyCarrierName = emptyCarrierName,
      functionCarrierName = functionCarrierName,
      targetName = targetName,
      emptyMemberName = emptyMemberName,
      combineMemberName = combineMemberName,
      firstNestedName = firstNestedName,
      secondNestedName = secondNestedName
    ).fold(problem => fail(problem.message), identity)

  private def invalidPlan(
      factoryName: String = "instance",
      typeName: String = "A",
      emptyCarrierName: String = "emptyValue",
      functionCarrierName: String = "combineFunction",
      targetName: String = "Monoid",
      emptyMemberName: String = "empty",
      combineMemberName: String = "combine",
      firstNestedName: String = "a",
      secondNestedName: String = "a1",
      typeBinder: BinderId = BinderId(0),
      emptyBinder: BinderId = BinderId(1),
      functionBinder: BinderId = BinderId(2),
      firstNestedBinder: BinderId = BinderId(3),
      secondNestedBinder: BinderId = BinderId(4),
      emptyMode: ParameterMode = ParameterMode.ByName,
      functionMode: ParameterMode = ParameterMode.ByValue,
      emptyTypeBinder: BinderId = BinderId(0),
      emptyTypeName: String = null,
      functionFirstBinder: BinderId = BinderId(0),
      functionSecondBinder: BinderId = BinderId(0),
      functionResultBinder: BinderId = BinderId(0),
      targetTypeBinder: BinderId = BinderId(0),
      firstNestedTypeBinder: BinderId = BinderId(0),
      secondNestedTypeBinder: BinderId = BinderId(0),
      combineResultTypeBinder: BinderId = BinderId(0),
      emptyBodyBinder: BinderId = BinderId(1),
      combineCalleeBinder: BinderId = BinderId(2),
      combineArgumentBinders: Vector[BinderId] = Vector(BinderId(3), BinderId(4)),
      typeParameter: TypeParameter = TypeParameter(BinderId(0), "A"),
      emptyValue: ByNameCarrier = ByNameCarrier(
        BinderId(1),
        "emptyValue",
        ParameterMode.ByName,
        ValueType(TypeParameterReference(BinderId(0), "A"))
      ),
      combineFunction: BinaryFunctionCarrier = BinaryFunctionCarrier(
        BinderId(2),
        "combineFunction",
        ParameterMode.ByValue,
        BinaryFunctionType(
          TypeParameterReference(BinderId(0), "A"),
          TypeParameterReference(BinderId(0), "A"),
          TypeParameterReference(BinderId(0), "A")
        )
      ),
      targetType: ScopedType = Applied(
        SourceName("Monoid"),
        Vector(TypeParameterReference(BinderId(0), "A"))
      ),
      emptyOverride: EmptyOverride = EmptyOverride("empty", TermReference(BinderId(1))),
      combineOverride: CombineOverride = CombineOverride(
        "combine",
        NestedParameter(BinderId(3), "a", TypeParameterReference(BinderId(0), "A")),
        NestedParameter(BinderId(4), "a1", TypeParameterReference(BinderId(0), "A")),
        TypeParameterReference(BinderId(0), "A"),
        CombineBody(
          TermReference(BinderId(2)),
          Vector(TermReference(BinderId(3)), TermReference(BinderId(4)))
        )
      )
  ): Either[ModelError, Plan] =
    val actualTypeParameter =
      if typeParameter == null then null else TypeParameter(typeBinder, typeName)
    val actualEmptyValue =
      if emptyValue == null then null
      else
        ByNameCarrier(
          emptyBinder,
          emptyCarrierName,
          emptyMode,
          ValueType(
            TypeParameterReference(
              emptyTypeBinder,
              Option(emptyTypeName).getOrElse(typeName)
            )
          )
        )
    val actualCombineFunction =
      if combineFunction == null then null
      else
        BinaryFunctionCarrier(
          functionBinder,
          functionCarrierName,
          functionMode,
          BinaryFunctionType(
            TypeParameterReference(functionFirstBinder, typeName),
            TypeParameterReference(functionSecondBinder, typeName),
            TypeParameterReference(functionResultBinder, typeName)
          )
        )
    val actualTarget =
      if targetType == null then null
      else
        targetType match
          case Applied(SourceName("Monoid"), Vector(_: TypeParameterReference)) =>
            Applied(
              SourceName(targetName),
              Vector(TypeParameterReference(targetTypeBinder, typeName))
            )
          case other => other
    val actualEmptyOverride =
      if emptyOverride == null then null
      else EmptyOverride(emptyMemberName, TermReference(emptyBodyBinder))
    val actualCombineOverride =
      if combineOverride == null then null
      else
        CombineOverride(
          combineMemberName,
          NestedParameter(
            firstNestedBinder,
            firstNestedName,
            TypeParameterReference(firstNestedTypeBinder, typeName)
          ),
          NestedParameter(
            secondNestedBinder,
            secondNestedName,
            TypeParameterReference(secondNestedTypeBinder, typeName)
          ),
          TypeParameterReference(combineResultTypeBinder, typeName),
          CombineBody(
            TermReference(combineCalleeBinder),
            combineArgumentBinders.map(TermReference(_))
          )
        )
    create(
      factoryName,
      actualTypeParameter,
      actualEmptyValue,
      actualCombineFunction,
      actualTarget,
      actualEmptyOverride,
      actualCombineOverride
    )

  private def assertCode(result: Either[ModelError, Plan], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
