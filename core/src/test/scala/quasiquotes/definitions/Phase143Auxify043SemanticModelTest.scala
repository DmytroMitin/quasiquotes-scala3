package quasiquotes.definitions

import quasiquotes.definitions.Phase143DelegatedForwardingModel.*
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

  private def validPlan(): Plan =
    invalidPlan().fold(error => fail(error.message), identity)

  private def invalidPlan(
      ordinaryBinder: BinderId = BinderId(1),
      ordinaryTypeBinder: BinderId = BinderId(0),
      contextualTypeBinder: BinderId = BinderId(0),
      bodyReceiverBinder: BinderId = BinderId(2),
      bodySelectedName: String = "show",
      bodyArgumentBinder: BinderId = BinderId(1),
      resultType: ScopedType = SourceName("String")
  ): Either[ModelError, Plan] =
    create(
      methodDisplayName = "show",
      typeParameter = TypeParameter(BinderId(0), "A"),
      ordinaryParameter = OrdinaryParameter(
        ordinaryBinder,
        "a",
        TypeParameterReference(ordinaryTypeBinder, "A")
      ),
      contextualParameter = ContextualParameter(
        BinderId(2),
        "inst",
        Applied(
          SourceName("Show"),
          Vector(TypeParameterReference(contextualTypeBinder, "A"))
        )
      ),
      resultType = resultType,
      body = ForwardingBody(
        ContextualReference(bodyReceiverBinder),
        bodySelectedName,
        OrdinaryReference(bodyArgumentBinder)
      )
    )
