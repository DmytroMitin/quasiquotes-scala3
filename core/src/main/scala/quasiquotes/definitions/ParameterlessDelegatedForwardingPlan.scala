package quasiquotes.definitions

import quasiquotes.definitions.DelegatedForwardingMethodPlan.{
  ContextualParameter,
  ContextualReference,
  MethodIdentity,
  TypeParameter
}
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.util.control.NonFatal

/** Compiler-free semantic plan for the exact AUXify-083 parameterless delegated forwarder. */
private[quasiquotes] object ParameterlessDelegatedForwardingPlan:
  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  enum DeclarationRole derives CanEqual:
    case TypeParameter, ContextualParameter

  enum SelectedMemberRole derives CanEqual:
    case GeneratedMethod

  final case class RoleTopology(
      evidenceTypeArgumentRole: DeclarationRole,
      resultTypeRole: DeclarationRole,
      bodyReceiverRole: DeclarationRole,
      selectedMemberRole: SelectedMemberRole
  ) derives CanEqual

  final case class RoleSnapshot(
      methodSourceName: String,
      typeParameterSourceName: String,
      contextualParameterSourceName: String,
      contextualTypeConstructorSourceName: String,
      topology: RoleTopology
  ) derives CanEqual

  final case class ValidatedBody(
      receiver: ContextualReference,
      selectedMethodIdentity: MethodIdentity
  )

  final class Plan private[definitions] (
      val methodIdentity: MethodIdentity,
      val typeParameter: TypeParameter,
      val contextualParameter: ContextualParameter,
      val evidenceTypeArgument: TypeParameterReference,
      val resultType: TypeParameterReference,
      val body: ValidatedBody,
      val roleSnapshot: RoleSnapshot
  )

  private var nextBinderValue = 0

  def create(
      methodDisplayName: String,
      typeParameterDisplayName: String,
      contextualParameterDisplayName: String,
      contextualTypeConstructorDisplayName: String
  ): Either[ModelError, Plan] =
    for
      _ <- legalName(methodDisplayName, "METHOD_NAME_INVALID", "generated method")
      _ <- legalName(
        typeParameterDisplayName,
        "TYPE_PARAMETER_NAME_INVALID",
        "method Type parameter"
      )
      _ <- legalName(
        contextualParameterDisplayName,
        "CONTEXTUAL_PARAMETER_NAME_INVALID",
        "contextual evidence parameter"
      )
      _ <- legalName(
        contextualTypeConstructorDisplayName,
        "CONTEXTUAL_TYPE_CONSTRUCTOR_INVALID",
        "contextual evidence Type constructor"
      )
      _ <- require(
        contextualTypeConstructorDisplayName != typeParameterDisplayName,
        "CONTEXTUAL_TYPE_CONSTRUCTOR_ROLE_COLLAPSE",
        "the unary evidence Type constructor must remain distinct from the local non-higher-kinded Type parameter."
      )
      binders <- allocateBinders()
      methodIdentity = new MethodIdentity(methodDisplayName)
      typeParameter = TypeParameter(binders(0), typeParameterDisplayName)
      evidenceTypeArgument = TypeParameterReference(
        typeParameter.binderId,
        typeParameter.displayName
      )
      resultType = TypeParameterReference(typeParameter.binderId, typeParameter.displayName)
      contextualParameter = ContextualParameter(
        binders(1),
        contextualParameterDisplayName,
        Applied(
          SourceName(contextualTypeConstructorDisplayName),
          Vector(evidenceTypeArgument)
        )
      )
      body = ValidatedBody(
        ContextualReference(contextualParameter.binderId),
        methodIdentity
      )
      snapshot = RoleSnapshot(
        methodDisplayName,
        typeParameterDisplayName,
        contextualParameterDisplayName,
        contextualTypeConstructorDisplayName,
        canonicalTopology
      )
    yield new Plan(
      methodIdentity,
      typeParameter,
      contextualParameter,
      evidenceTypeArgument,
      resultType,
      body,
      snapshot
    )

  /** Revalidates every retained edge without accepting or exposing raw BinderId factory inputs. */
  def validate(plan: Plan): Either[ModelError, RoleSnapshot] =
    Option(plan)
      .toRight(error("PLAN_MISSING", "the parameterless delegated-forwarding plan must be present."))
      .flatMap(validatePresent)

  private def validatePresent(plan: Plan): Either[ModelError, RoleSnapshot] =
    try
      for
        _ <- require(
          plan.methodIdentity != null &&
            plan.typeParameter != null &&
            plan.contextualParameter != null &&
            plan.evidenceTypeArgument != null &&
            plan.resultType != null &&
            plan.body != null &&
            plan.roleSnapshot != null,
          "PLAN_GRAPH_INVALID",
          "every retained parameterless delegated-forwarding edge must be present."
        )
        _ <- legalName(plan.methodIdentity.sourceName, "METHOD_NAME_INVALID", "generated method")
        _ <- legalName(
          plan.typeParameter.displayName,
          "TYPE_PARAMETER_NAME_INVALID",
          "method Type parameter"
        )
        _ <- legalName(
          plan.contextualParameter.displayName,
          "CONTEXTUAL_PARAMETER_NAME_INVALID",
          "contextual evidence parameter"
        )
        _ <- require(
          plan.typeParameter.binderId != null &&
            plan.contextualParameter.binderId != null &&
            plan.typeParameter.binderId != plan.contextualParameter.binderId,
          "PLAN_GRAPH_INVALID",
          "the Type and contextual declarations must retain distinct non-null identities."
        )
        constructor <- contextualConstructor(plan)
        _ <- require(
          exactTypeReference(plan.evidenceTypeArgument, plan.typeParameter),
          "PLAN_GRAPH_INVALID",
          "the evidence Type argument must reference the exact method Type declaration."
        )
        _ <- require(
          exactTypeReference(plan.resultType, plan.typeParameter),
          "PLAN_GRAPH_INVALID",
          "the result Type must reference the exact method Type declaration."
        )
        _ <- require(
          plan.body.receiver != null &&
            plan.body.receiver.binderId != null &&
            plan.body.receiver.binderId == plan.contextualParameter.binderId,
          "PLAN_GRAPH_INVALID",
          "the selection receiver must reference the exact contextual declaration."
        )
        _ <- require(
          plan.body.selectedMethodIdentity != null &&
            plan.body.selectedMethodIdentity.eq(plan.methodIdentity),
          "PLAN_GRAPH_INVALID",
          "the selected member must retain the generated MethodIdentity by reference."
        )
        _ <- require(
          constructor != plan.typeParameter.displayName,
          "PLAN_GRAPH_INVALID",
          "the evidence constructor and non-higher-kinded Type parameter must not collapse."
        )
        expected = RoleSnapshot(
          plan.methodIdentity.sourceName,
          plan.typeParameter.displayName,
          plan.contextualParameter.displayName,
          constructor,
          canonicalTopology
        )
        _ <- require(
          plan.roleSnapshot == expected,
          "PLAN_GRAPH_INVALID",
          "the stored role snapshot must exactly describe the retained semantic graph."
        )
      yield expected
    catch case NonFatal(_) =>
      Left(error("PLAN_GRAPH_INVALID", "the parameterless delegated-forwarding graph is malformed."))

  private def contextualConstructor(plan: Plan): Either[ModelError, String] =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference))
          if reference.eq(plan.evidenceTypeArgument) &&
            exactTypeReference(reference, plan.typeParameter) =>
        legalName(
          constructor,
          "CONTEXTUAL_TYPE_CONSTRUCTOR_INVALID",
          "contextual evidence Type constructor"
        ).map(_ => constructor)
      case _ =>
        Left(
          error(
            "PLAN_GRAPH_INVALID",
            "the contextual parameter must retain one direct unary evidence application over the exact evidence argument edge."
          )
        )

  private def exactTypeReference(value: ScopedType, declaration: TypeParameter): Boolean =
    value match
      case TypeParameterReference(binderId, displayName) =>
        binderId != null &&
          binderId == declaration.binderId &&
          displayName == declaration.displayName
      case _ => false

  private def canonicalTopology: RoleTopology =
    RoleTopology(
      evidenceTypeArgumentRole = DeclarationRole.TypeParameter,
      resultTypeRole = DeclarationRole.TypeParameter,
      bodyReceiverRole = DeclarationRole.ContextualParameter,
      selectedMemberRole = SelectedMemberRole.GeneratedMethod
    )

  private def allocateBinders(): Either[ModelError, Vector[BinderId]] = synchronized:
    if nextBinderValue > Int.MaxValue - 2 then
      Left(
        error(
          "BINDER_IDENTITY_EXHAUSTED",
          "the graph-local parameterless delegated-forwarding BinderId space is exhausted."
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
