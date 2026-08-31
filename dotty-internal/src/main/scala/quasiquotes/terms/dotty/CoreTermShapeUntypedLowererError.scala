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

  final case class InvalidIdentifierName(name: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Invalid bounded exact-backend Identifier name `${String.valueOf(name)}`: expected a non-keyword ASCII name matching [A-Za-z_][A-Za-z0-9_]*, excluding _."

  final case class PlaceholderIdentifier(name: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Placeholder Identifier `${String.valueOf(name)}` is not an admitted semantic source Identifier at the bounded exact-backend boundary."

  final case class InvalidSelectedName(name: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Invalid bounded exact-backend selected-member name `${String.valueOf(name)}`: expected a non-keyword ASCII name matching [A-Za-z_][A-Za-z0-9_]*, excluding _."

  case object MultipleApplicationLists extends CoreTermShapeUntypedLowererError:
    def message: String =
      "Unsupported multiple application lists at the bounded exact-backend boundary: an Apply node must not have a direct Apply in function position."

  case object MissingApplyArguments extends CoreTermShapeUntypedLowererError:
    def message: String =
      "Cannot lower a bounded exact-backend Apply with a missing ordinary argument list."

  final case class UnsupportedTermShape(nodeKind: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Unsupported core TermShape at the bounded exact-backend boundary: $nodeKind."

  final case class SourceFreeInvariantViolation(nodeKind: String, detail: String)
      extends CoreTermShapeUntypedLowererError:
    def message: String =
      s"Source-free raw-tree invariant failed for $nodeKind: $detail"
