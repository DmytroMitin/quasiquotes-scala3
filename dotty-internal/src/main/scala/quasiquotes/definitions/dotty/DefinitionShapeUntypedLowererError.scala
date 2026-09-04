package quasiquotes.definitions.dotty

import quasiquotes.definitions.DefinitionConstructionError
import quasiquotes.terms.dotty.CompletedTypeUntypedLoweringError
import quasiquotes.types.TypeQuasiquoteError

private[quasiquotes] sealed trait DefinitionShapeUntypedLowererError
    derives CanEqual:
  def message: String

private[quasiquotes] object DefinitionShapeUntypedLowererError:
  case object MissingDefinitionShape
      extends DefinitionShapeUntypedLowererError:
    def message: String = "DefinitionShape exact lowering requires a non-null semantic shape."

  final case class OrdinaryDefinitionCompletionFailure(
      cause: DefinitionConstructionError
  ) extends DefinitionShapeUntypedLowererError:
    def message: String = s"Ordinary DefinitionShape completion failed: ${cause.message}"

  final case class OrdinaryDefinitionExactBackendFailure(
      cause: ConstructedDefinitionUntypedBackendError
  ) extends DefinitionShapeUntypedLowererError:
    def message: String = s"Ordinary DefinitionShape exact lowering failed: ${cause.message}"

  final case class SimpleTypeAliasCompletionFailure(
      cause: TypeQuasiquoteError
  ) extends DefinitionShapeUntypedLowererError:
    def message: String = s"Simple type-alias normal-form completion failed: ${cause.message}"

  final case class SimpleTypeAliasNameFailure(detail: String)
      extends DefinitionShapeUntypedLowererError:
    def message: String = s"Simple type-alias name lowering failed: $detail"

  final case class SimpleTypeAliasCompletedTypeFailure(
      cause: CompletedTypeUntypedLoweringError
  ) extends DefinitionShapeUntypedLowererError:
    def message: String = s"Simple type-alias completed-type lowering failed: ${cause.message}"

  final case class RawInvariantFailure(family: String, detail: String)
      extends DefinitionShapeUntypedLowererError:
    def message: String = s"$family raw exact invariant failed: $detail"
