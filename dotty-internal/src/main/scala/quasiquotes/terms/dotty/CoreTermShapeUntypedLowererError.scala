package quasiquotes.terms.dotty

private[quasiquotes] sealed trait CoreTermShapeUntypedLowererError derives CanEqual:
  def message: String

private[quasiquotes] object CoreTermShapeUntypedLowererError:
  case object MissingTermShape extends CoreTermShapeUntypedLowererError:
    def message: String =
      "Cannot lower a missing core TermShape at the bounded exact-backend boundary."

  final case class InvalidIntegerLiteral(value: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Invalid bounded exact-backend integer literal `${String.valueOf(value)}`: expected 0, a non-zero decimal digit followed by decimal digits, or the same non-zero form prefixed by minus."

  final case class InvalidInfixOperator(operator: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Invalid bounded exact-backend infix operator `${String.valueOf(operator)}`: expected one of +, -, *, /, %, ==, !=, <, <=, >, or >=."

  final case class UnsupportedTermShape(nodeKind: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Unsupported core TermShape at the bounded exact-backend boundary: $nodeKind."

  final case class SourceFreeInvariantViolation(nodeKind: String, detail: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Source-free raw-tree invariant failed for $nodeKind: $detail"
