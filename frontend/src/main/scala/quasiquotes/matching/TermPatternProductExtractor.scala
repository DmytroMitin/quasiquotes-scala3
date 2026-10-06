package quasiquotes.matching

/** A statically typed heterogeneous product of captures from one qq pattern. */
final class TermPatternProductExtractor[T, Captures <: Tuple] private[matching] (
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)
