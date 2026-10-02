package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

/** Compiler-free semantic plan for the exact AUXify-045 extension module. */
private[quasiquotes] object ExtensionModulePlan:
  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  enum DeclarationRole derives CanEqual:
    case TypeParameter, ExtensionReceiver, OrdinaryArgument, ContextualEvidence

  enum ExternalTypeRole derives CanEqual:
    case EvidenceConstructor

  enum SelectedMemberRole derives CanEqual:
    case GeneratedMethod

  final case class RoleTopology(
      receiverTypeRole: DeclarationRole,
      ordinaryTypeRole: DeclarationRole,
      contextualConstructorRole: ExternalTypeRole,
      contextualArgumentRole: DeclarationRole,
      resultTypeRole: DeclarationRole,
      bodyReceiverRole: DeclarationRole,
      selectedMemberRole: SelectedMemberRole,
      bodyArgumentRoles: Vector[DeclarationRole]
  ) derives CanEqual

  final case class RoleSnapshot(
      moduleSourceName: String,
      typeParameterSourceName: String,
      receiverSourceName: String,
      methodSourceName: String,
      ordinaryArgumentSourceName: String,
      contextualSourceName: String,
      evidenceConstructorSourceName: String,
      topology: RoleTopology
  ) derives CanEqual

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final case class ReceiverParameter(
      binderId: BinderId,
      displayName: String,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class OrdinaryArgument(
      binderId: BinderId,
      displayName: String,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class ContextualParameter(
      binderId: BinderId,
      displayName: String,
      parameterType: Applied
  ) derives CanEqual

  final case class BodyTermReference(binderId: BinderId) derives CanEqual

  final class MethodIdentity private[definitions] (
      val sourceName: String
  ):
    override def toString: String = s"MethodIdentity($sourceName)"

  final case class DelegatedBody(
      receiver: BodyTermReference,
      selectedMethodIdentity: MethodIdentity,
      arguments: Vector[BodyTermReference]
  )

  final class Plan private[definitions] (
      val moduleDisplayName: String,
      val methodIdentity: MethodIdentity,
      val typeParameter: TypeParameter,
      val receiverParameter: ReceiverParameter,
      val ordinaryArgument: OrdinaryArgument,
      val contextualParameter: ContextualParameter,
      val resultType: TypeParameterReference,
      val body: DelegatedBody,
      val roleSnapshot: RoleSnapshot
  )

  private var nextBinderValue = 0

  def create(
      moduleDisplayName: String,
      typeParameterDisplayName: String,
      receiverDisplayName: String,
      methodDisplayName: String,
      ordinaryArgumentDisplayName: String,
      contextualDisplayName: String,
      evidenceTypeConstructorDisplayName: String
  ): Either[ModelError, Plan] =
    for
      _ <- legalName(moduleDisplayName, "MODULE_NAME_INVALID", "module")
      _ <- legalName(
        typeParameterDisplayName,
        "TYPE_PARAMETER_NAME_INVALID",
        "extension Type parameter"
      )
      _ <- legalName(receiverDisplayName, "RECEIVER_NAME_INVALID", "extension receiver")
      _ <- legalName(methodDisplayName, "METHOD_NAME_INVALID", "nested method")
      _ <- legalName(
        ordinaryArgumentDisplayName,
        "ORDINARY_ARGUMENT_NAME_INVALID",
        "ordinary argument"
      )
      _ <- legalName(
        contextualDisplayName,
        "CONTEXTUAL_NAME_INVALID",
        "contextual evidence parameter"
      )
      _ <- legalName(
        evidenceTypeConstructorDisplayName,
        "EVIDENCE_TYPE_CONSTRUCTOR_NAME_INVALID",
        "evidence Type constructor"
      )
      _ <- require(
        Vector(
          receiverDisplayName,
          ordinaryArgumentDisplayName,
          contextualDisplayName
        ).distinct.size == 3,
        "TERM_ROLE_NAMES_MUST_BE_DISTINCT",
        "the extension receiver, ordinary argument, and contextual evidence names must be pairwise distinct."
      )
      _ <- require(
        evidenceTypeConstructorDisplayName != typeParameterDisplayName,
        "EVIDENCE_TYPE_CONSTRUCTOR_ROLE_COLLAPSE",
        "the evidence Type constructor must remain distinct from the local non-higher-kinded Type parameter."
      )
      binders <- allocateBinders()
      typeBinder = binders(0)
      receiverBinder = binders(1)
      ordinaryBinder = binders(2)
      contextualBinder = binders(3)
      typeReference = TypeParameterReference(typeBinder, typeParameterDisplayName)
      methodIdentity = new MethodIdentity(methodDisplayName)
      topology = RoleTopology(
        receiverTypeRole = DeclarationRole.TypeParameter,
        ordinaryTypeRole = DeclarationRole.TypeParameter,
        contextualConstructorRole = ExternalTypeRole.EvidenceConstructor,
        contextualArgumentRole = DeclarationRole.TypeParameter,
        resultTypeRole = DeclarationRole.TypeParameter,
        bodyReceiverRole = DeclarationRole.ContextualEvidence,
        selectedMemberRole = SelectedMemberRole.GeneratedMethod,
        bodyArgumentRoles = Vector(
          DeclarationRole.ExtensionReceiver,
          DeclarationRole.OrdinaryArgument
        )
      )
      snapshot = RoleSnapshot(
        moduleDisplayName,
        typeParameterDisplayName,
        receiverDisplayName,
        methodDisplayName,
        ordinaryArgumentDisplayName,
        contextualDisplayName,
        evidenceTypeConstructorDisplayName,
        topology
      )
    yield new Plan(
      moduleDisplayName,
      methodIdentity,
      TypeParameter(typeBinder, typeParameterDisplayName),
      ReceiverParameter(receiverBinder, receiverDisplayName, typeReference),
      OrdinaryArgument(ordinaryBinder, ordinaryArgumentDisplayName, typeReference),
      ContextualParameter(
        contextualBinder,
        contextualDisplayName,
        Applied(SourceName(evidenceTypeConstructorDisplayName), Vector(typeReference))
      ),
      typeReference,
      DelegatedBody(
        BodyTermReference(contextualBinder),
        methodIdentity,
        Vector(
          BodyTermReference(receiverBinder),
          BodyTermReference(ordinaryBinder)
        )
      ),
      snapshot
    )

  private def allocateBinders(): Either[ModelError, Vector[BinderId]] = synchronized:
    if nextBinderValue > Int.MaxValue - 4 then
      Left(
        error(
          "BINDER_IDENTITY_EXHAUSTED",
          "the graph-local extension-module BinderId space is exhausted."
        )
      )
    else
      val allocated = Vector.tabulate(4)(offset => BinderId(nextBinderValue + offset))
      nextBinderValue += 4
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
