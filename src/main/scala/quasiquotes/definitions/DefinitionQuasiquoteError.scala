package quasiquotes.definitions

private[quasiquotes] sealed trait DefinitionQuasiquoteError derives CanEqual:
  def message: String

private[quasiquotes] object DefinitionQuasiquoteError:
  final case class InvalidPartsArgumentArity(parts: Int, arguments: Int)
      extends DefinitionQuasiquoteError:
    def message: String =
      s"Definition quasiquote arity mismatch: received $parts literal parts and $arguments interpolation arguments."

  final case class NullDescriptor(argumentIndex: Int)
      extends DefinitionQuasiquoteError:
    def message: String =
      s"Definition interpolation argument $argumentIndex is null."

  final case class NullDescriptorPayload(argumentIndex: Int, role: String)
      extends DefinitionQuasiquoteError:
    def message: String =
      s"Definition interpolation argument $argumentIndex ($role) has a null payload."

  final case class InvalidAssemblySourceMetadata(detail: String)
      extends DefinitionQuasiquoteError:
    def message: String =
      s"Invalid definition quasiquote assembly metadata: $detail"

  final case class FrontendFailure(
      kind: String,
      detail: String,
      argumentIndex: Option[Int],
      role: Option[String]
  ) extends DefinitionQuasiquoteError:
    def message: String =
      argumentIndex match
        case Some(index) =>
          val roleText = role.fold("")(value => s" ($value)")
          s"Definition interpolation argument $index$roleText failed during $kind: $detail"
        case None =>
          s"Definition quasiquote $kind failure: $detail"

  final case class CompletionFailure(
      underlying: DefinitionConstructionError,
      detail: String,
      argumentIndex: Option[Int],
      role: Option[String]
  ) extends DefinitionQuasiquoteError:
    def message: String =
      argumentIndex match
        case Some(index) =>
          val roleText = role.fold("")(value => s" ($value)")
          s"Definition interpolation argument $index$roleText failed during completion: $detail"
        case None =>
          s"Definition quasiquote completion failed: $detail"

  final case class InvalidCompletedSourceEvidence(detail: String)
      extends DefinitionQuasiquoteError:
    def message: String =
      s"Invalid completed definition quasiquote source evidence: $detail"
