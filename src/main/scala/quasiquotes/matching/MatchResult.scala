package quasiquotes.matching

final case class MatchResult[T](bindings: Map[String, T]):
  def binding(name: String): Option[T] =
    bindings.get(name.stripPrefix("$"))

sealed trait MatchFailure derives CanEqual:
  def message: String

object MatchFailure:
  final case class ShapeMismatch(expected: String, actual: String) extends MatchFailure:
    def message: String = s"Pattern shape mismatch: expected $expected, got $actual"

  final case class RepeatedHoleMismatch(name: String, previous: String, current: String) extends MatchFailure:
    def message: String =
      s"Repeated hole $$${name} matched different subtrees: previous=$previous current=$current"

  final case class UnsupportedTargetShape(detail: String) extends MatchFailure:
    def message: String = s"Unsupported target tree shape: $detail"
