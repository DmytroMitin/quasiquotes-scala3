package quasiquotes.construct

import quasiquotes.definitions.DefinitionName

private[quasiquotes] final case class GeneratedParameterBinderId(value: Int) derives CanEqual

private[quasiquotes] enum GeneratedClassOwnerPlan derives CanEqual:
  case ActiveSplice

private[quasiquotes] enum GeneratedParentPlan derives CanEqual:
  case CallerProvidedCompleteType

private[quasiquotes] enum GeneratedMethodOwnerPlan derives CanEqual:
  case GeneratedClass
  case ActiveSplice

private[quasiquotes] enum GeneratedConstructorPlan derives CanEqual:
  case ParameterlessPrimary

private[quasiquotes] final case class GeneratedParameterPlan(
    displayName: String,
    binderId: GeneratedParameterBinderId
) derives CanEqual

private[quasiquotes] enum GeneratedMethodBodyPlan derives CanEqual:
  case CapturedTermPlusParameter(parameterBinderId: GeneratedParameterBinderId)

private[quasiquotes] final case class OverrideMethodPlan(
    displayName: String,
    owner: GeneratedMethodOwnerPlan,
    parameter: GeneratedParameterPlan,
    body: GeneratedMethodBodyPlan
) derives CanEqual

private[quasiquotes] final case class GeneratedClassPlan(
    classDisplayName: String,
    owner: GeneratedClassOwnerPlan,
    parent: GeneratedParentPlan,
    overrideMethod: OverrideMethodPlan,
    constructor: GeneratedConstructorPlan
) derives CanEqual

private[quasiquotes] final case class GeneratedClassLoweringError(
    code: String,
    message: String
) derives CanEqual

private[construct] object GeneratedClassPlanValidation:
  def validate(plan: GeneratedClassPlan): Either[GeneratedClassLoweringError, Unit] =
    if plan == null then
      Left(GeneratedClassLoweringError("NULL_PLAN", "The generated class plan must not be null."))
    else if plan.overrideMethod == null || plan.overrideMethod.parameter == null then
      Left(
        GeneratedClassLoweringError(
          "MALFORMED_OVERRIDE_PLAN",
          "The generated class plan must contain one complete override-method plan."
        )
      )
    else
      for
        _ <- validateFixedRoles(plan)
        _ <- validateName("CLASS", plan.classDisplayName)
        _ <- validateName("METHOD", plan.overrideMethod.displayName)
        _ <- validateName("PARAMETER", plan.overrideMethod.parameter.displayName)
        _ <- validateMethodOwner(plan.overrideMethod.owner)
        _ <- validateBinder(plan.overrideMethod)
      yield ()

  private def validateFixedRoles(
      plan: GeneratedClassPlan
  ): Either[GeneratedClassLoweringError, Unit] =
    if plan.owner != GeneratedClassOwnerPlan.ActiveSplice then
      Left(GeneratedClassLoweringError("CLASS_OWNER_UNSUPPORTED", "The generated class owner must be the active splice."))
    else if plan.parent != GeneratedParentPlan.CallerProvidedCompleteType then
      Left(GeneratedClassLoweringError("PARENT_ROLE_UNSUPPORTED", "The generated parent must be the caller-provided complete TypeRepr."))
    else if plan.constructor != GeneratedConstructorPlan.ParameterlessPrimary then
      Left(GeneratedClassLoweringError("CONSTRUCTOR_ROLE_UNSUPPORTED", "Only the parameterless primary constructor is supported."))
    else Right(())

  private def validateName(
      role: String,
      value: String
  ): Either[GeneratedClassLoweringError, Unit] =
    if value != null && DefinitionName.plain(value).isRight then Right(())
    else
      Left(
        GeneratedClassLoweringError(
          s"INVALID_${role}_DISPLAY_NAME",
          s"The generated ${role.toLowerCase} display name must be a plain Scala identifier."
        )
      )

  private def validateMethodOwner(
      owner: GeneratedMethodOwnerPlan
  ): Either[GeneratedClassLoweringError, Unit] =
    owner match
      case GeneratedMethodOwnerPlan.GeneratedClass => Right(())
      case GeneratedMethodOwnerPlan.ActiveSplice =>
        Left(
          GeneratedClassLoweringError(
            "DETACHED_METHOD_OWNER",
            "The generated override must be owned by the generated class, not the active splice."
          )
        )

  private def validateBinder(
      method: OverrideMethodPlan
  ): Either[GeneratedClassLoweringError, Unit] =
    method.body match
      case GeneratedMethodBodyPlan.CapturedTermPlusParameter(bodyBinder)
          if bodyBinder != null && method.parameter.binderId != null &&
            bodyBinder == method.parameter.binderId && bodyBinder.value >= 0 =>
        Right(())
      case _ =>
        Left(
          GeneratedClassLoweringError(
            "MALFORMED_BODY_BINDER",
            "The generated method body must reference its declared generated parameter binder exactly."
          )
        )
