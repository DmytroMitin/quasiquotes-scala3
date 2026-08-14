package quasiquotes.definitions.dotty

private[quasiquotes] sealed trait ConstructedDefinitionUntypedBackendError
    derives CanEqual:
  def message: String

private[quasiquotes] object ConstructedDefinitionUntypedBackendError:
  final case class DefinitionNameLoweringFailure(detail: String)
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      s"Constructed-definition name lowering failed: $detail"

  final case class DefinitionTypeLoweringFailure(detail: String)
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      s"Constructed-definition type lowering failed: $detail"

  final case class DefinitionBodyLoweringFailure(detail: String)
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      s"Constructed-definition body lowering failed: $detail"

  final case class RawDefinitionConstructionInvariantFailure(detail: String)
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      s"Raw constructed-definition invariant failed: $detail"

  case object TwoParameterDefinitionExactBackendDeferred
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      "Exact backend lowering for two-parameter definitions is deliberately deferred."

  final case class UnsupportedConstructedDefinitionVariant(variant: String)
      extends ConstructedDefinitionUntypedBackendError:
    def message: String =
      s"Unsupported constructed-definition variant at the exact-version untyped backend boundary: $variant."
