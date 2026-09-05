package quasiquotes.definitions.dotty

import quasiquotes.terms.dotty.{
  CompletedTypeUntypedLoweringError,
  ConstructedTermGeneratedOriginError
}
import quasiquotes.types.TypeQuasiquoteError

private[quasiquotes] sealed trait SimpleTypeAliasGeneratedOriginError
    derives CanEqual:
  def message: String

private[quasiquotes] object SimpleTypeAliasGeneratedOriginError:
  case object MissingDefinitionShape
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      "Simple type-alias generated-origin lowering requires a non-null DefinitionShape."

  final case class WrongDefinitionShapeFamily(actual: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias generated-origin lowering does not accept $actual."

  final case class AliasCompletionFailure(cause: TypeQuasiquoteError)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias completion failed before provenance work: ${cause.message}"

  final case class AliasNameFailure(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias name validation failed before provenance work: $detail"

  final case class CompletedTypeExactLoweringFailure(
      cause: CompletedTypeUntypedLoweringError
  ) extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias completed-type lowering failed before provenance work: ${cause.message}"

  final case class SourceFreeInvariantFailure(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias source-free invariant failed before provenance work: $detail"

  final case class InvalidVirtualSourceName(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String = s"Invalid simple type-alias virtual source name: $detail"

  final case class GeneratedTypePlanningFailure(
      cause: ConstructedTermGeneratedOriginError
  ) extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias generated Type plan failed: ${cause.message}"

  final case class GeneratedSourcePlanMismatch(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias generated source/plan mismatch: $detail"

  final case class RawTopologyMismatch(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias raw tree/plan mismatch: $detail"

  final case class GeneratedOriginPositioningFailure(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias generated-origin positioning failed: $detail"

  final case class PositionedInvariantFailure(detail: String)
      extends SimpleTypeAliasGeneratedOriginError:
    def message: String =
      s"Simple type-alias positioned invariant failed: $detail"
