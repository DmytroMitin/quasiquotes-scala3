package quasiquotes.definitions.dotty

private[quasiquotes] sealed trait ConstructedDefinitionGeneratedOriginError
    derives CanEqual:
  def message: String

private[quasiquotes] object ConstructedDefinitionGeneratedOriginError:
  final case class InvalidVirtualSourceName(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Invalid generated-definition virtual source name: $detail."

  final case class DefinitionNameRenderingFailure(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Generated-definition name rendering failed: $detail"

  final case class DefinitionTypePlanningFailure(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Generated-definition type planning failed: $detail"

  final case class DefinitionBodyPlanningFailure(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Generated-definition body planning failed: $detail"

  final case class RawDefinitionLoweringFailure(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Generated-definition raw structural lowering failed: $detail"

  final case class UnsupportedConstructedDefinitionVariant(variant: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Unsupported constructed-definition variant at the generated-origin backend boundary: $variant."

  final case class RawDefinitionPlanMismatch(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Generated-definition raw tree/plan mismatch: $detail"

  final case class InvalidDefinitionStructuralPlan(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Invalid generated-definition structural plan: $detail."

  final case class IncompleteDefinitionPositionMap(detail: String)
      extends ConstructedDefinitionGeneratedOriginError:
    def message: String =
      s"Incomplete generated-definition position map: $detail."
