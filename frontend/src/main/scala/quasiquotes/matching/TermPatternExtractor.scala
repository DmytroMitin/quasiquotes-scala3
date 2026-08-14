package quasiquotes.matching

/** A bounded extractor for ordered term captures in a `qq` pattern. */
final class TermPatternExtractor[T] private[matching] (
    extract: T => Option[Seq[T]]
):
  def unapplySeq(value: T): Option[Seq[T]] = extract(value)
