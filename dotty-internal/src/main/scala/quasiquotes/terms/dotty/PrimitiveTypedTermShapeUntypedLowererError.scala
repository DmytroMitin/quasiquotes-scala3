package quasiquotes.terms.dotty

private[quasiquotes] sealed trait PrimitiveTypedTermShapeUntypedLowererError
    derives CanEqual:
  def message: String

private[quasiquotes] object PrimitiveTypedTermShapeUntypedLowererError:
  case object MissingTermShape
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      "Primitive Typed lowering requires a present TermShape."

  final case class WrongTermShapeFamily(actual: String)
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      s"Primitive Typed lowering does not accept $actual."

  case object MissingTypedExpression
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      "Primitive Typed lowering requires a present expression."

  case object MissingPrimitiveTypeName
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      "Primitive Typed lowering requires a present primitive type name."

  final case class UnsupportedPrimitiveTypeName(value: String)
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      s"Primitive Typed lowering does not admit `${String.valueOf(value)}`; expected Int, String, or Boolean."

  final case class ExpressionLoweringFailure(
      cause: CoreTermShapeUntypedLowererError
  ) extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      s"Primitive Typed expression lowering failed: ${cause.message}"

  final case class RawTopologyMismatch(detail: String)
      extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      s"Primitive Typed raw topology mismatch: $detail"

  final case class SourceFreeInvariantFailure(
      cause: CoreTermShapeUntypedLowererError
  ) extends PrimitiveTypedTermShapeUntypedLowererError:
    def message: String =
      s"Primitive Typed source-free invariant failed: ${cause.message}"
