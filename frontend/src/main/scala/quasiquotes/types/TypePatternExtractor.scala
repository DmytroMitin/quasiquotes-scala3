package quasiquotes.types

/** A bounded extractor for ordered, original `TypeRepr` captures in a `tqq` pattern. */
final class TypePatternExtractor[T] private[types] (
    extract: T => Option[Seq[T]]
):
  def unapplySeq(value: T): Option[Seq[T]] = extract(value)
