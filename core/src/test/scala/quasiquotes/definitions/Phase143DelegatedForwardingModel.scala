package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

/** Test-only semantic carrier probe for the exact AUXify-043 topology. */
private[quasiquotes] object Phase143DelegatedForwardingModel:
  final case class TypeParameter(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final case class OrdinaryParameter(
      binderId: BinderId,
      displayName: String,
      parameterType: TypeParameterReference
  ) derives CanEqual

  final case class ContextualParameter(
      binderId: BinderId,
      displayName: String,
      parameterType: Applied
  ) derives CanEqual

  final case class ContextualReference(binderId: BinderId) derives CanEqual
  final case class OrdinaryReference(binderId: BinderId) derives CanEqual

  final case class ForwardingBody(
      receiver: ContextualReference,
      selectedMethodDisplayName: String,
      argument: OrdinaryReference
  ) derives CanEqual

  final case class ModelError(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final class MethodIdentity private[definitions] (
      val sourceName: String
  ):
    override def toString: String = s"MethodIdentity($sourceName)"

  final case class ValidatedBody(
      receiver: ContextualReference,
      selectedMethodIdentity: MethodIdentity,
      argument: OrdinaryReference
  )

  final class Plan private[definitions] (
      val methodIdentity: MethodIdentity,
      val typeParameter: TypeParameter,
      val ordinaryParameter: OrdinaryParameter,
      val contextualParameter: ContextualParameter,
      val resultType: SourceName,
      val body: ValidatedBody
  )

  def create(
      methodDisplayName: String,
      typeParameter: TypeParameter,
      ordinaryParameter: OrdinaryParameter,
      contextualParameter: ContextualParameter,
      resultType: ScopedType,
      body: ForwardingBody
  ): Either[ModelError, Plan] =
    for
      _ <- present(typeParameter, "TYPE_PARAMETER_MISSING", "the Type parameter")
      _ <- present(
        ordinaryParameter,
        "ORDINARY_PARAMETER_MISSING",
        "the ordinary parameter"
      )
      _ <- present(
        contextualParameter,
        "CONTEXTUAL_PARAMETER_MISSING",
        "the contextual parameter"
      )
      _ <- present(body, "BODY_MISSING", "the forwarding body")
      _ <- require(
        Vector(
          typeParameter.binderId,
          ordinaryParameter.binderId,
          contextualParameter.binderId
        ).forall(_ != null) &&
          Vector(
            typeParameter.binderId,
            ordinaryParameter.binderId,
            contextualParameter.binderId
          ).distinct.size == 3,
        "BINDER_ROLES_MUST_BE_DISTINCT",
        "the Type, ordinary-Term, and contextual-Term declarations require three distinct BinderIds."
      )
      _ <- legalName(methodDisplayName, "METHOD_NAME_INVALID", "method")
      _ <- legalName(
        typeParameter.displayName,
        "TYPE_PARAMETER_NAME_INVALID",
        "Type parameter"
      )
      _ <- legalName(
        ordinaryParameter.displayName,
        "ORDINARY_PARAMETER_NAME_INVALID",
        "ordinary parameter"
      )
      _ <- legalName(
        contextualParameter.displayName,
        "CONTEXTUAL_PARAMETER_NAME_INVALID",
        "contextual parameter"
      )
      _ <- validateTypeParameterReference(
        ordinaryParameter.parameterType,
        typeParameter,
        "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH"
      )
      _ <- validateContextualType(contextualParameter.parameterType, typeParameter)
      namedResult <- resultType match
        case source: SourceName =>
          legalName(source.value, "RESULT_TYPE_NAME_INVALID", "result type")
            .map(_ => source)
        case _ =>
          Left(
            error(
              "RESULT_TYPE_UNSUPPORTED",
              "the exact 043 result type must be one direct source name."
            )
          )
      _ <- require(
        body.receiver.binderId == contextualParameter.binderId,
        "BODY_RECEIVER_BINDER_MISMATCH",
        "the selected receiver must reference the exact contextual Term binder."
      )
      _ <- require(
        body.selectedMethodDisplayName == methodDisplayName,
        "BODY_SELECTED_METHOD_MISMATCH",
        "the selected member must be the exact generated method name."
      )
      _ <- require(
        body.argument.binderId == ordinaryParameter.binderId,
        "BODY_ARGUMENT_BINDER_MISMATCH",
        "the applied argument must reference the exact ordinary Term binder."
      )
      methodIdentity = new MethodIdentity(methodDisplayName)
    yield new Plan(
      methodIdentity,
      typeParameter,
      ordinaryParameter,
      contextualParameter,
      namedResult,
      ValidatedBody(body.receiver, methodIdentity, body.argument)
    )

  private def validateContextualType(
      value: Applied,
      declaration: TypeParameter
  ): Either[ModelError, Unit] =
    value match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        for
          _ <- legalName(
            constructor,
            "CONTEXTUAL_TYPE_CONSTRUCTOR_INVALID",
            "contextual type constructor"
          )
          _ <- validateTypeParameterReference(
            reference,
            declaration,
            "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH"
          )
        yield ()
      case _ =>
        Left(
          error(
            "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
            "the contextual parameter type must apply one source-named constructor to the declared Type parameter."
          )
        )

  private def validateTypeParameterReference(
      reference: TypeParameterReference,
      declaration: TypeParameter,
      code: String
  ): Either[ModelError, Unit] =
    require(
      reference != null &&
        reference.binderId == declaration.binderId &&
        reference.displayName == declaration.displayName,
      code,
      "the Type reference must retain the declared Type binder and display spelling."
    )

  private def present[A](
      value: A,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    require(value != null, code, s"$role must be present.")

  private def legalName(
      value: String,
      code: String,
      role: String
  ): Either[ModelError, Unit] =
    require(
      value != null && DefinitionName.fromSource(value).isRight,
      code,
      s"the $role must be one legal Scala source name."
    )

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[ModelError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): ModelError =
    ModelError(code, detail)
