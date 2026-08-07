package quasiquotes.definitions.dotty

private[quasiquotes] sealed trait PublicContextualMethodGeneratedOriginError
    derives CanEqual:
  def message: String

private[quasiquotes] object PublicContextualMethodGeneratedOriginError:
  final case class InvalidVirtualSourceName(detail: String)
      extends PublicContextualMethodGeneratedOriginError:
    def message: String =
      s"Invalid contextual-method virtual source name: $detail."

  final case class ProjectionPlanningFailure(detail: String)
      extends PublicContextualMethodGeneratedOriginError:
    def message: String =
      s"Contextual-method generated-source planning failed: $detail"

  final case class RawLoweringFailure(detail: String)
      extends PublicContextualMethodGeneratedOriginError:
    def message: String =
      s"Contextual-method raw structural lowering failed: $detail"

  final case class RawTreePlanMismatch(detail: String)
      extends PublicContextualMethodGeneratedOriginError:
    def message: String =
      s"Contextual-method raw tree/position plan mismatch: $detail"

  final case class IncompletePositionMap(detail: String)
      extends PublicContextualMethodGeneratedOriginError:
    def message: String =
      s"Incomplete contextual-method generated-origin position map: $detail"
