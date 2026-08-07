package quasiquotes.definitions.dotty

private[quasiquotes] sealed trait PublicContextualMethodUntypedBackendError
    derives CanEqual:
  def message: String

private[quasiquotes] object PublicContextualMethodUntypedBackendError:
  case object NullDefinitionResult
      extends PublicContextualMethodUntypedBackendError:
    def message: String =
      "Public contextual-method lowering failed: the definition result was null."

  final case class ProjectionInvariantFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String =
      s"Public contextual-method projection invariant failed: $detail"

  final case class MethodNameLoweringFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Public contextual-method name lowering failed: $detail"

  final case class TypeParameterLoweringFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Public contextual-method type-parameter lowering failed: $detail"

  final case class ContextualParameterLoweringFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Public contextual-method contextual-parameter lowering failed: $detail"

  final case class TypeLoweringFailure(anchor: String, detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Public contextual-method $anchor type lowering failed: $detail"

  final case class BodyLoweringFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Public contextual-method body lowering failed: $detail"

  final case class UnsupportedCompletedTypeProjection(kindCode: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String =
      s"Unsupported completed-type projection at the exact-version contextual-method backend boundary: $kindCode."

  final case class RawConstructionInvariantFailure(detail: String)
      extends PublicContextualMethodUntypedBackendError:
    def message: String = s"Raw public contextual-method invariant failed: $detail"
