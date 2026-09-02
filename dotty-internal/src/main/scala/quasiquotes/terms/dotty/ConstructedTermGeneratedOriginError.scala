package quasiquotes.terms.dotty

private[quasiquotes] sealed trait ConstructedTermGeneratedOriginError
    derives CanEqual:
  def message: String

private[quasiquotes] object ConstructedTermGeneratedOriginError:
  case object MissingConstructedTerm
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      "Cannot render a missing ConstructedTerm at the generated-origin boundary."

  case object MissingTermShape extends ConstructedTermGeneratedOriginError:
    def message: String =
      "Cannot render a missing TermShape at the generated-origin boundary."

  final case class InvalidVirtualSourceName(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Invalid generated-origin virtual source name: $detail."

  final case class UnrenderableName(role: String, name: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin $role name `$name` in the bounded Scala source fragment."

  final case class InvalidConstructorName(name: String, detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin constructor name `$name`: $detail."

  final case class MalformedConstructorArguments(arguments: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin constructor arguments: arguments=$arguments; expected a non-null argument list."

  final case class NullConstructorArgument(index: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin constructor arguments: argument $index is null."

  final case class UnsupportedLiteral(value: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin literal `$value`: expected a decimal integer, Boolean, or semantic String marker."

  final case class UnsupportedUnaryOperator(operator: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin unary operator `$operator`: expected one of +, -, !, or ~."

  final case class MalformedBlock(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin bounded P1/P2/P3 Block: $detail"

  final case class MalformedLocalDef(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin bounded P3 LocalDef: $detail"

  final case class UnsupportedInterpolationPrefix(prefix: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin interpolation prefix `$prefix`: expected standard `s`."

  final case class MalformedInterpolation(
      parts: Int,
      arguments: Int
  ) extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin interpolation: parts=$parts, arguments=$arguments; expected parts == arguments + 1."

  final case class NullInterpolationPart(index: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin interpolation: semantic part $index is null."

  final case class NullInterpolationArgument(index: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render malformed generated-origin interpolation: argument $index is null."

  final case class UnsupportedTermNode(nodeKind: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render unsupported generated-origin term node `$nodeKind`."

  final case class OutOfScopeBoundReference(binderId: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render bound reference for inactive binder identity $binderId."

  final case class MalformedBinderScope(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Malformed generated-origin binder scope: $detail"

  case object NestedLambda1Unsupported
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      "Nested Lambda1 is outside the bounded generated-origin contract."

  final case class MissingTypeSidecar(typedOrdinal: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Missing generated-origin type sidecar at typed ordinal $typedOrdinal."

  final case class UnconsumedTypeSidecars(consumed: Int, total: Int)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Generated-origin rendering consumed $consumed of $total completed type sidecars."

  final case class UnsupportedTypeSidecar(
      typedOrdinal: Int,
      normalForm: String
  ) extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Cannot render generated-origin type sidecar at typed ordinal $typedOrdinal: $normalForm."

  final case class RawLoweringFailure(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Generated-origin raw structural lowering failed: $detail"

  final case class InvalidStructuralPlan(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Invalid generated-origin structural plan: $detail."

  final case class RawTreePlanMismatch(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Generated-origin raw tree/plan mismatch: $detail."

  final case class IncompletePositionMap(detail: String)
      extends ConstructedTermGeneratedOriginError:
    def message: String =
      s"Incomplete generated-origin position map: $detail."
