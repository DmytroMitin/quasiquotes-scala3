package quasiquotes.definitions.dotty

import quasiquotes.definitions.{DefinitionName, ScopedType}
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

/** U-local mechanical name and binder roles for the exact AUXify-045 first slice. */
private[quasiquotes] object BoundedExtensionModulePlan:
  final case class TypeParameter(binderId: BinderId, displayName: String)
      derives CanEqual

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

  final case class DelegatedBody(
      receiver: BodyTermReference,
      selectedMethodDisplayName: String,
      arguments: Vector[BodyTermReference]
  ) derives CanEqual

  final class Plan private[dotty] (
      val moduleDisplayName: String,
      val methodDisplayName: String,
      val typeParameter: TypeParameter,
      val receiverParameter: ReceiverParameter,
      val ordinaryArgument: OrdinaryArgument,
      val contextualParameter: ContextualParameter,
      val resultType: TypeParameterReference,
      val body: DelegatedBody
  )

  def create(
      moduleDisplayName: String,
      methodDisplayName: String,
      typeParameter: TypeParameter,
      receiverParameter: ReceiverParameter,
      ordinaryArgument: OrdinaryArgument,
      contextualParameter: ContextualParameter,
      resultType: TypeParameterReference,
      body: DelegatedBody
  ): Either[BoundedExtensionModuleError, Plan] =
    for
      _ <- present(typeParameter, "the Type parameter")
      _ <- present(receiverParameter, "the extension receiver")
      _ <- present(ordinaryArgument, "the ordinary argument")
      _ <- present(contextualParameter, "the contextual parameter")
      _ <- present(resultType, "the result Type")
      _ <- present(body, "the delegated body")
      _ <- legalName(moduleDisplayName, "MODULE_NAME_INVALID", "module")
      _ <- legalName(methodDisplayName, "METHOD_NAME_INVALID", "method")
      _ <- legalName(
        typeParameter.displayName,
        "TYPE_PARAMETER_RECEIVER_INVALID",
        "Type parameter"
      )
      _ <- legalName(
        receiverParameter.displayName,
        "TYPE_PARAMETER_RECEIVER_INVALID",
        "receiver"
      )
      _ <- legalName(
        ordinaryArgument.displayName,
        "ORDINARY_ARGUMENT_INVALID",
        "ordinary argument"
      )
      _ <- legalName(
        contextualParameter.displayName,
        "CONTEXTUAL_PARAMETER_INVALID",
        "contextual parameter"
      )
      _ <- require(
        Vector(
          typeParameter.binderId,
          receiverParameter.binderId,
          ordinaryArgument.binderId,
          contextualParameter.binderId
        ).forall(_ != null) &&
          Vector(
            typeParameter.binderId,
            receiverParameter.binderId,
            ordinaryArgument.binderId,
            contextualParameter.binderId
          ).distinct.size == 4,
        "TYPE_PARAMETER_RECEIVER_INVALID",
        "the Type, receiver, ordinary, and contextual roles require four distinct BinderIds."
      )
      _ <- typeReferenceMatches(
        receiverParameter.parameterType,
        typeParameter,
        "TYPE_PARAMETER_RECEIVER_INVALID"
      )
      _ <- typeReferenceMatches(
        ordinaryArgument.parameterType,
        typeParameter,
        "ORDINARY_ARGUMENT_INVALID"
      )
      _ <- validateEvidence(contextualParameter.parameterType, typeParameter)
      _ <- typeReferenceMatches(
        resultType,
        typeParameter,
        "TYPE_PARAMETER_RECEIVER_INVALID"
      )
      _ <- require(
        body.arguments != null && body.arguments.size == 2,
        "UNSUPPORTED_TOPOLOGY",
        "the exact delegated application requires receiver and ordinary argument in one two-argument Apply."
      )
      _ <- require(
        body.receiver != null &&
          body.receiver.binderId == contextualParameter.binderId &&
          body.selectedMethodDisplayName == methodDisplayName &&
          body.arguments == Vector(
            BodyTermReference(receiverParameter.binderId),
            BodyTermReference(ordinaryArgument.binderId)
          ),
        "DELEGATED_BODY_INVALID",
        "the body must be evidence.method(receiver, argument) with the exact declared roles."
      )
      _ <- require(
        Vector(
          receiverParameter.displayName,
          ordinaryArgument.displayName,
          contextualParameter.displayName
        ).distinct.size == 3,
        "CONTEXTUAL_PARAMETER_INVALID",
        "the receiver, ordinary argument, and contextual parameter names must be distinct."
      )
    yield new Plan(
      moduleDisplayName,
      methodDisplayName,
      typeParameter,
      receiverParameter,
      ordinaryArgument,
      contextualParameter,
      resultType,
      body
    )

  private def validateEvidence(
      value: Applied,
      declaration: TypeParameter
  ): Either[BoundedExtensionModuleError, Unit] =
    value match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        for
          _ <- legalName(
            constructor,
            "UNARY_EVIDENCE_TYPE_INVALID",
            "evidence Type constructor"
          )
          _ <- typeReferenceMatches(
            reference,
            declaration,
            "UNARY_EVIDENCE_TYPE_INVALID"
          )
        yield ()
      case _ =>
        Left(error(
          "UNARY_EVIDENCE_TYPE_INVALID",
          "the contextual parameter Type must apply one source-named constructor to the declared Type parameter."
        ))

  private def typeReferenceMatches(
      reference: TypeParameterReference,
      declaration: TypeParameter,
      code: String
  ): Either[BoundedExtensionModuleError, Unit] =
    require(
      reference != null &&
        reference.binderId == declaration.binderId &&
        reference.displayName == declaration.displayName,
      code,
      "the Type reference must retain the exact declared Type binder and display spelling."
    )

  private def present[A](
      value: A,
      role: String
  ): Either[BoundedExtensionModuleError, Unit] =
    require(value != null, "MISSING_FIELD", s"$role must be present.")

  private def legalName(
      value: String,
      code: String,
      role: String
  ): Either[BoundedExtensionModuleError, Unit] =
    if value == null then Left(error("MISSING_FIELD", s"the $role name must be present."))
    else
      DefinitionName
        .fromSource(value)
        .left
        .map(problem => error(code, problem.message))
        .map(_ => ())

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[BoundedExtensionModuleError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): BoundedExtensionModuleError =
    BoundedExtensionModuleError(code, detail)
