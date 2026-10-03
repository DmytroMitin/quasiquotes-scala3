package quasiquotes.definitions.dotty

import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.ExtensionModulePlan
import quasiquotes.definitions.ExtensionModulePlan.Plan as CorePlan
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.Plan as BoundedPlan

/** Mechanical bridge from the accepted Core extension plan to the U024 backend. */
private[quasiquotes] object ExtensionModulePlanU024Adapter:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def adapt(plan: CorePlan): Either[Error, BoundedPlan] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the Core extension-module plan must be present."
      ))
      _ <- requireCoreFields(present)
      adapted <- createBoundedPlan(present)
      _ <- verifyMapping(present, adapted)
    yield adapted

  def lowerGenerated(
      plan: CorePlan,
      virtualSourceName: String
  )(using Context): Either[Error, GeneratedOriginDefinitionResult] =
    adapt(plan).flatMap { adapted =>
      BoundedExtensionModuleGeneratedOriginAdapter
        .lower(adapted, virtualSourceName)
        .left
        .map(problem => error(
          "GENERATED_ORIGIN_LOWERING_FAILED",
          s"U024 ${problem.code}: ${problem.detail}"
        ))
    }

  private def createBoundedPlan(plan: CorePlan): Either[Error, BoundedPlan] =
    import BoundedExtensionModulePlan.*

    BoundedExtensionModulePlan
      .create(
        plan.moduleDisplayName,
        plan.methodIdentity.sourceName,
        TypeParameter(
          plan.typeParameter.binderId,
          plan.typeParameter.displayName
        ),
        ReceiverParameter(
          plan.receiverParameter.binderId,
          plan.receiverParameter.displayName,
          plan.receiverParameter.parameterType
        ),
        OrdinaryArgument(
          plan.ordinaryArgument.binderId,
          plan.ordinaryArgument.displayName,
          plan.ordinaryArgument.parameterType
        ),
        ContextualParameter(
          plan.contextualParameter.binderId,
          plan.contextualParameter.displayName,
          plan.contextualParameter.parameterType
        ),
        plan.resultType,
        DelegatedBody(
          BodyTermReference(plan.body.receiver.binderId),
          plan.body.selectedMethodIdentity.sourceName,
          plan.body.arguments.map(reference => BodyTermReference(reference.binderId))
        )
      )
      .left
      .map(problem => error(
        "BOUNDED_PLAN_ADAPTATION_FAILED",
        s"U024 ${problem.code}: ${problem.detail}"
      ))

  private def requireCoreFields(plan: CorePlan): Either[Error, Unit] =
    val complete =
      plan.moduleDisplayName != null &&
        plan.methodIdentity != null &&
        plan.methodIdentity.sourceName != null &&
        plan.typeParameter != null &&
        plan.typeParameter.binderId != null &&
        plan.typeParameter.displayName != null &&
        plan.receiverParameter != null &&
        plan.receiverParameter.binderId != null &&
        plan.receiverParameter.displayName != null &&
        plan.receiverParameter.parameterType != null &&
        plan.ordinaryArgument != null &&
        plan.ordinaryArgument.binderId != null &&
        plan.ordinaryArgument.displayName != null &&
        plan.ordinaryArgument.parameterType != null &&
        plan.contextualParameter != null &&
        plan.contextualParameter.binderId != null &&
        plan.contextualParameter.displayName != null &&
        plan.contextualParameter.parameterType != null &&
        plan.resultType != null &&
        plan.body != null &&
        plan.body.receiver != null &&
        plan.body.receiver.binderId != null &&
        plan.body.selectedMethodIdentity != null &&
        plan.body.selectedMethodIdentity.sourceName != null &&
        plan.body.arguments != null &&
        plan.body.arguments.forall(reference =>
          reference != null && reference.binderId != null
        )

    Either.cond(
      complete,
      (),
      error(
        "CORE_PLAN_MALFORMED",
        "the Core extension-module plan contains an unavailable required field."
      )
    )

  private def verifyMapping(
      core: CorePlan,
      bounded: BoundedPlan
  ): Either[Error, Unit] =
    val exact =
      bounded != null &&
        bounded.moduleDisplayName == core.moduleDisplayName &&
        bounded.methodDisplayName == core.methodIdentity.sourceName &&
        bounded.typeParameter.binderId == core.typeParameter.binderId &&
        bounded.typeParameter.displayName == core.typeParameter.displayName &&
        bounded.receiverParameter.binderId == core.receiverParameter.binderId &&
        bounded.receiverParameter.displayName == core.receiverParameter.displayName &&
        (bounded.receiverParameter.parameterType eq core.receiverParameter.parameterType) &&
        bounded.ordinaryArgument.binderId == core.ordinaryArgument.binderId &&
        bounded.ordinaryArgument.displayName == core.ordinaryArgument.displayName &&
        (bounded.ordinaryArgument.parameterType eq core.ordinaryArgument.parameterType) &&
        bounded.contextualParameter.binderId == core.contextualParameter.binderId &&
        bounded.contextualParameter.displayName == core.contextualParameter.displayName &&
        (bounded.contextualParameter.parameterType eq core.contextualParameter.parameterType) &&
        (bounded.resultType eq core.resultType) &&
        bounded.body.receiver.binderId == core.body.receiver.binderId &&
        bounded.body.selectedMethodDisplayName == core.body.selectedMethodIdentity.sourceName &&
        bounded.body.arguments.map(_.binderId) == core.body.arguments.map(_.binderId)

    Either.cond(
      exact,
      (),
      error(
        "INTERNAL_INVARIANT_FAILED",
        "the accepted bounded plan did not retain the exact Core role mapping."
      )
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
