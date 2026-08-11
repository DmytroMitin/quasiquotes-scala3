package quasiquotes.terms.dotty

private[quasiquotes] sealed trait ConstructedTermUntypedBackendError derives CanEqual:
  def message: String

private[quasiquotes] object ConstructedTermUntypedBackendError:
  final case class UnsupportedLiteral(value: String)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Unsupported constructed-term literal `$value`: expected a decimal integer, Boolean, or semantic String value enclosed by quotes."

  final case class UnsupportedTermNode(nodeKind: String)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Unsupported constructed-term node at the exact-version untyped backend boundary: $nodeKind."

  final case class InvalidConstructorName(name: String, detail: String)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Invalid constructed-term constructor name `$name`: $detail."

  final case class MalformedConstructorArguments(arguments: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Malformed constructed-term constructor arguments: arguments=$arguments; expected a non-null argument list."

  final case class NullConstructorArgument(index: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Malformed constructed-term constructor arguments: argument $index is null."

  final case class UnsupportedUnaryOperator(operator: String)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Unsupported constructed-term unary operator `$operator`: expected one of +, -, !, or ~."

  final case class UnsupportedInterpolationPrefix(prefix: String)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Unsupported constructed-term interpolation prefix `$prefix`: expected standard `s`."

  final case class MalformedInterpolation(
      parts: Int,
      arguments: Int
  ) extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Malformed constructed-term interpolation: parts=$parts, arguments=$arguments; expected parts == arguments + 1."

  final case class NullInterpolationPart(index: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Malformed constructed-term interpolation: semantic part $index is null."

  final case class NullInterpolationArgument(index: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Malformed constructed-term interpolation: argument $index is null."

  final case class UnsupportedTypeSidecar(
      typedOrdinal: Int,
      normalForm: String
  ) extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Unsupported completed type sidecar at typed ordinal $typedOrdinal: $normalForm."

  final case class MissingTypeSidecar(typedOrdinal: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Missing completed type sidecar at typed ordinal $typedOrdinal."

  final case class UnconsumedTypeSidecars(consumed: Int, total: Int)
      extends ConstructedTermUntypedBackendError:
    def message: String =
      s"Constructed-term lowering consumed $consumed of $total completed type sidecars."
