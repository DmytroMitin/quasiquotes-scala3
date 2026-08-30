package quasiquotes.definitions

import quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

class Phase143Auxify043SemanticModelTest extends munit.FunSuite:
  test("coherent 043 carrier keeps three binder roles distinct and one method identity shared") {
    val plan = validPlan()

    assertEquals(plan.typeParameter.binderId, BinderId(0))
    assertEquals(plan.ordinaryParameter.binderId, BinderId(1))
    assertEquals(plan.contextualParameter.binderId, BinderId(2))
    assertEquals(plan.ordinaryParameter.parameterType, TypeParameterReference(BinderId(0), "A"))
    assertEquals(
      plan.contextualParameter.parameterType,
      Applied(SourceName("Show"), Vector(TypeParameterReference(BinderId(0), "A")))
    )
    assertEquals(plan.resultType, SourceName("String"))
    assert(plan.methodIdentity eq plan.body.selectedMethodIdentity)
    assertEquals(plan.body.receiver.binderId, BinderId(2))
    assertEquals(plan.body.argument.binderId, BinderId(1))
  }

  test("carrier rejects binder, reference, selected-name, and result-shape incoherence separately") {
    val rows = List(
      invalidPlan(ordinaryBinder = BinderId(0)) -> "BINDER_ROLES_MUST_BE_DISTINCT",
      invalidPlan(ordinaryTypeBinder = BinderId(9)) ->
        "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH",
      invalidPlan(contextualTypeBinder = BinderId(9)) ->
        "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
      invalidPlan(bodyReceiverBinder = BinderId(9)) ->
        "BODY_RECEIVER_BINDER_MISMATCH",
      invalidPlan(bodySelectedName = "render") -> "BODY_SELECTED_METHOD_MISMATCH",
      invalidPlan(bodyArgumentBinder = BinderId(9)) ->
        "BODY_ARGUMENT_BINDER_MISMATCH",
      invalidPlan(resultType = Applied(SourceName("Box"), Vector(SourceName("String")))) ->
        "RESULT_TYPE_UNSUPPORTED"
    )

    rows.foreach { case (result, expectedCode) =>
      assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))
    }
  }

  test("carrier reports role-specific illegal source names") {
    val rows = List(
      invalidPlan(methodName = "bad-name", bodySelectedName = "bad-name") ->
        "METHOD_NAME_INVALID",
      invalidPlan(typeParameterName = "bad-name") ->
        "TYPE_PARAMETER_NAME_INVALID",
      invalidPlan(ordinaryName = "bad-name") ->
        "ORDINARY_PARAMETER_NAME_INVALID",
      invalidPlan(contextualName = "bad-name") ->
        "CONTEXTUAL_PARAMETER_NAME_INVALID",
      invalidPlan(contextualConstructorName = "bad-name") ->
        "CONTEXTUAL_TYPE_CONSTRUCTOR_INVALID",
      invalidPlan(resultType = SourceName("bad-name")) ->
        "RESULT_TYPE_NAME_INVALID"
    )

    rows.foreach { case (result, expectedCode) =>
      assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))
    }
  }

  private def validPlan(): Plan =
    invalidPlan().fold(error => fail(error.message), identity)

  private def invalidPlan(
      methodName: String = "show",
      typeParameterName: String = "A",
      ordinaryName: String = "a",
      contextualName: String = "inst",
      contextualConstructorName: String = "Show",
      ordinaryBinder: BinderId = BinderId(1),
      ordinaryTypeBinder: BinderId = BinderId(0),
      contextualTypeBinder: BinderId = BinderId(0),
      bodyReceiverBinder: BinderId = BinderId(2),
      bodySelectedName: String = "show",
      bodyArgumentBinder: BinderId = BinderId(1),
      resultType: ScopedType = SourceName("String")
  ): Either[ModelError, Plan] =
    create(
      methodDisplayName = methodName,
      typeParameter = TypeParameter(BinderId(0), typeParameterName),
      ordinaryParameter = OrdinaryParameter(
        ordinaryBinder,
        ordinaryName,
        TypeParameterReference(ordinaryTypeBinder, typeParameterName)
      ),
      contextualParameter = ContextualParameter(
        BinderId(2),
        contextualName,
        Applied(
          SourceName(contextualConstructorName),
          Vector(TypeParameterReference(contextualTypeBinder, typeParameterName))
        )
      ),
      resultType = resultType,
      body = ForwardingBody(
        ContextualReference(bodyReceiverBinder),
        bodySelectedName,
        OrdinaryReference(bodyArgumentBinder)
      )
    )
