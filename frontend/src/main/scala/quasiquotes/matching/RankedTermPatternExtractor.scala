package quasiquotes.matching

import scala.quoted.Quotes

/** A typed product extractor for one bounded sequence-Term capture. */
final class RankedTermPatternExtractor[T, Captures <: Tuple](
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)

private[quasiquotes] final class RankedSingleSequenceTermPatternExtractor[T](
    extract: T => Option[Seq[T]]
):
  def unapply(value: T): Option[Seq[T]] = extract(value)

private[quasiquotes] sealed trait RankedCaptureKind
private[quasiquotes] sealed trait ScalarTermCapture extends RankedCaptureKind
private[quasiquotes] sealed trait SequenceTermCapture extends RankedCaptureKind

private[quasiquotes] type RankedCaptureTypes[T, Kinds <: Tuple] <: Tuple = Kinds match
  case EmptyTuple => EmptyTuple
  case ScalarTermCapture *: tail => T *: RankedCaptureTypes[T, tail]
  case SequenceTermCapture *: tail => Seq[T] *: RankedCaptureTypes[T, tail]

private[matching] object RankedTermPatternExtractorFactory:
  transparent inline def singleSequenceExtractor(
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedSingleSequenceTermPatternExtractor[q.reflect.Term] =
    val compiled = RankedPatternSource.compileOrAbort(context.parts.toList, sequenceIndex)
    RankedTermPatternBridge.singleSequenceExtractor(
      compiled.pattern,
      compiled.holeNames,
      compiled.sequenceHoleName
    )

  transparent inline def extractor[Kinds <: Tuple](
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedTermPatternExtractor[
    q.reflect.Term,
    RankedCaptureTypes[q.reflect.Term, Kinds]
  ] =
    val compiled = RankedPatternSource.compileOrAbort(context.parts.toList, sequenceIndex)
    RankedTermPatternBridge.extractor[Kinds](
      compiled.pattern,
      compiled.holeNames,
      compiled.sequenceHoleName
    )
