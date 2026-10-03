package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.util.control.NonFatal

/** Compiler-free semantic plan for the exact AUXify-081 one-value factory. */
private[quasiquotes] object ValueInstanceFactoryPlan:
  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  enum ParameterMode derives CanEqual:
    case ByValue

  enum DeclarationRole derives CanEqual:
    case TypeParameter, StrictCarrier

  enum ExternalTypeRole derives CanEqual:
    case TargetConstructor

  final case class RoleTopology(
      carrierTypeRole: DeclarationRole,
      targetArgumentRole: DeclarationRole,
      resultTargetRole: ExternalTypeRole,
      parentTargetRole: ExternalTypeRole,
      memberTypeRole: DeclarationRole,
      rhsCarrierRole: DeclarationRole
  ) derives CanEqual

  final case class RoleSnapshot(
      factorySourceName: String,
      typeParameterSourceName: String,
      carrierSourceName: String,
      targetConstructorSourceName: String,
      memberSourceName: String,
      topology: RoleTopology
  ) derives CanEqual

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final case class StrictCarrier(
      binderId: BinderId,
      displayName: String,
      mode: ParameterMode,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class TermReference(binderId: BinderId) derives CanEqual

  final case class ValueOverride(
      memberDisplayName: String,
      valueType: TypeParameterReference,
      body: TermReference
  ) derives CanEqual

  final class Plan private[definitions] (
      val factoryDisplayName: String,
      val typeParameter: TypeParameter,
      val strictCarrier: StrictCarrier,
      val resultTarget: Applied,
      val anonymousParentTarget: Applied,
      val valueOverride: ValueOverride,
      val roleSnapshot: RoleSnapshot
  )

  private var nextBinderValue = 0

  def create(
      factoryDisplayName: String,
      typeParameterDisplayName: String,
      carrierDisplayName: String,
      targetTypeConstructorDisplayName: String,
      memberDisplayName: String
  ): Either[ModelError, Plan] =
    for
      _ <- legalName(factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
      _ <- legalName(
        typeParameterDisplayName,
        "TYPE_PARAMETER_NAME_INVALID",
        "Type parameter"
      )
      _ <- legalName(carrierDisplayName, "CARRIER_NAME_INVALID", "strict carrier")
      _ <- legalName(
        targetTypeConstructorDisplayName,
        "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
        "target Type constructor"
      )
      _ <- legalName(memberDisplayName, "MEMBER_NAME_INVALID", "override member")
      _ <- require(
        targetTypeConstructorDisplayName != typeParameterDisplayName,
        "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE",
        "the target Type constructor must remain distinct from the local non-higher-kinded Type parameter."
      )
      _ <- require(
        carrierDisplayName != memberDisplayName,
        "CARRIER_MEMBER_ROLE_COLLAPSE",
        "the override member must not shadow the outer carrier used by its initializer."
      )
      binders <- allocateBinders()
      typeBinder = binders(0)
      carrierBinder = binders(1)
      typeReference = TypeParameterReference(typeBinder, typeParameterDisplayName)
      target = Applied(
        SourceName(targetTypeConstructorDisplayName),
        Vector(typeReference)
      )
      topology = RoleTopology(
        carrierTypeRole = DeclarationRole.TypeParameter,
        targetArgumentRole = DeclarationRole.TypeParameter,
        resultTargetRole = ExternalTypeRole.TargetConstructor,
        parentTargetRole = ExternalTypeRole.TargetConstructor,
        memberTypeRole = DeclarationRole.TypeParameter,
        rhsCarrierRole = DeclarationRole.StrictCarrier
      )
      snapshot = RoleSnapshot(
        factoryDisplayName,
        typeParameterDisplayName,
        carrierDisplayName,
        targetTypeConstructorDisplayName,
        memberDisplayName,
        topology
      )
    yield new Plan(
      factoryDisplayName,
      TypeParameter(typeBinder, typeParameterDisplayName),
      StrictCarrier(
        carrierBinder,
        carrierDisplayName,
        ParameterMode.ByValue,
        typeReference
      ),
      target,
      target,
      ValueOverride(
        memberDisplayName,
        typeReference,
        TermReference(carrierBinder)
      ),
      snapshot
    )

  /** Revalidates every retained semantic edge without exposing raw identity inputs. */
  def validate(plan: Plan): Either[ModelError, RoleSnapshot] =
    Option(plan)
      .toRight(error("PLAN_MISSING", "the one-value factory plan must be present."))
      .flatMap(validatePresent)

  private def validatePresent(plan: Plan): Either[ModelError, RoleSnapshot] =
    try
      for
        _ <- require(
          plan.typeParameter != null &&
            plan.strictCarrier != null &&
            plan.resultTarget != null &&
            plan.anonymousParentTarget != null &&
            plan.valueOverride != null &&
            plan.roleSnapshot != null,
          "PLAN_GRAPH_INVALID",
          "every one-value factory declaration and semantic edge must be present."
        )
        _ <- legalName(plan.factoryDisplayName, "FACTORY_NAME_INVALID", "factory method")
        _ <- legalName(
          plan.typeParameter.displayName,
          "TYPE_PARAMETER_NAME_INVALID",
          "Type parameter"
        )
        _ <- legalName(plan.strictCarrier.displayName, "CARRIER_NAME_INVALID", "strict carrier")
        _ <- require(
          plan.typeParameter.binderId != null &&
            plan.strictCarrier.binderId != null &&
            plan.typeParameter.binderId != plan.strictCarrier.binderId,
          "PLAN_GRAPH_INVALID",
          "the Type and strict-carrier declarations must retain distinct non-null identities."
        )
        _ <- require(
          plan.strictCarrier.mode == ParameterMode.ByValue &&
            exactTypeReference(plan.strictCarrier.parameterType, plan.typeParameter),
          "PLAN_GRAPH_INVALID",
          "the carrier must remain strict/by-value and reference the exact Type declaration."
        )
        resultConstructor <- targetConstructor(plan.resultTarget, plan.typeParameter)
        parentConstructor <- targetConstructor(plan.anonymousParentTarget, plan.typeParameter)
        _ <- require(
          resultConstructor == parentConstructor &&
            plan.resultTarget == plan.anonymousParentTarget,
          "PLAN_GRAPH_INVALID",
          "the result and anonymous-parent targets must retain the same exact unary role."
        )
        _ <- legalName(
          resultConstructor,
          "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
          "target Type constructor"
        )
        _ <- legalName(
          plan.valueOverride.memberDisplayName,
          "MEMBER_NAME_INVALID",
          "override member"
        )
        _ <- require(
          exactTypeReference(plan.valueOverride.valueType, plan.typeParameter) &&
            plan.valueOverride.body != null &&
            plan.valueOverride.body.binderId == plan.strictCarrier.binderId,
          "PLAN_GRAPH_INVALID",
          "the override Type and RHS must reference the exact Type and carrier declarations."
        )
        _ <- require(
          resultConstructor != plan.typeParameter.displayName &&
            plan.strictCarrier.displayName != plan.valueOverride.memberDisplayName,
          "PLAN_GRAPH_INVALID",
          "the validated lexical roles must remain non-collapsing."
        )
        expected = RoleSnapshot(
          plan.factoryDisplayName,
          plan.typeParameter.displayName,
          plan.strictCarrier.displayName,
          resultConstructor,
          plan.valueOverride.memberDisplayName,
          canonicalTopology
        )
        _ <- require(
          plan.roleSnapshot == expected,
          "PLAN_GRAPH_INVALID",
          "the stored role snapshot must exactly describe the retained semantic graph."
        )
      yield expected
    catch case NonFatal(_) =>
      Left(error("PLAN_GRAPH_INVALID", "the one-value factory semantic graph is malformed."))

  private def targetConstructor(
      target: Applied,
      declaration: TypeParameter
  ): Either[ModelError, String] =
    target match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference))
          if exactTypeReference(reference, declaration) =>
        Right(constructor)
      case _ =>
        Left(
          error(
            "PLAN_GRAPH_INVALID",
            "each target must be one direct unary source-named application of the exact Type declaration."
          )
        )

  private def exactTypeReference(
      reference: TypeParameterReference,
      declaration: TypeParameter
  ): Boolean =
    reference != null &&
      reference.binderId != null &&
      reference.binderId == declaration.binderId &&
      reference.displayName == declaration.displayName

  private def canonicalTopology: RoleTopology =
    RoleTopology(
      carrierTypeRole = DeclarationRole.TypeParameter,
      targetArgumentRole = DeclarationRole.TypeParameter,
      resultTargetRole = ExternalTypeRole.TargetConstructor,
      parentTargetRole = ExternalTypeRole.TargetConstructor,
      memberTypeRole = DeclarationRole.TypeParameter,
      rhsCarrierRole = DeclarationRole.StrictCarrier
    )

  private def allocateBinders(): Either[ModelError, Vector[BinderId]] = synchronized:
    if nextBinderValue > Int.MaxValue - 2 then
      Left(
        error(
          "BINDER_IDENTITY_EXHAUSTED",
          "the graph-local one-value factory BinderId space is exhausted."
        )
      )
    else
      val allocated = Vector.tabulate(2)(offset => BinderId(nextBinderValue + offset))
      nextBinderValue += 2
      Right(allocated)

  private def legalName(
      value: String,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem => error(code, s"the $role must be one legal Scala source name: ${problem.detail}"))
      .map(_ => ())

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[ModelError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): ModelError =
    ModelError(code, detail)
