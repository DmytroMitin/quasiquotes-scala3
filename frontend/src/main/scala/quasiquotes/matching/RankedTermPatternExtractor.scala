package quasiquotes.matching

import scala.compiletime.erasedValue
import scala.quoted.Quotes

/** A typed product extractor for one bounded sequence-Term capture. */
final class RankedTermPatternExtractor[T, Captures <: Tuple](
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)

private[matching] final class RankedSingleSequenceTermPatternExtractor[T](
    extract: T => Option[Seq[T]]
):
  def unapply(value: T): Option[Seq[T]] = extract(value)

private[matching] sealed trait RankedCaptureKind
private[matching] sealed trait ScalarTermCapture extends RankedCaptureKind
private[matching] sealed trait SequenceTermCapture extends RankedCaptureKind

private[matching] type RankedCaptureTypes[T, Kinds <: Tuple] <: Tuple = Kinds match
  case EmptyTuple => EmptyTuple
  case ScalarTermCapture *: tail => T *: RankedCaptureTypes[T, tail]
  case SequenceTermCapture *: tail => Seq[T] *: RankedCaptureTypes[T, tail]

private[matching] final case class RankedTermMatch[T](
    scalarBindings: Map[String, T],
    sequenceBindings: Map[String, Seq[T]],
    holeNames: Vector[String]
):
  def scalar(index: Int): T = scalarBindings(holeNames(index))
  def sequence(index: Int): Seq[T] = sequenceBindings(holeNames(index))

private[matching] object RankedTermPatternExtractorFactory:
  transparent inline def singleSequenceExtractor(
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedSingleSequenceTermPatternExtractor[q.reflect.Term] =
    val ranked = extractor[SequenceTermCapture *: EmptyTuple](context, sequenceIndex)
    new RankedSingleSequenceTermPatternExtractor(term => ranked.unapply(term).map(_.head))

  transparent inline def extractor[Kinds <: Tuple](
      context: StringContext,
      sequenceIndex: Int
  )(using q: Quotes): RankedTermPatternExtractor[
    q.reflect.Term,
    RankedCaptureTypes[q.reflect.Term, Kinds]
  ] =
    val compiled = RankedPatternSource.compileOrAbort(context.parts.toList, sequenceIndex)
    new RankedTermPatternExtractor(term =>
      TermMatcher
        .matchTermRanked(compiled.pattern, compiled.sequenceHoleName, term)
        .toOption
        .map(result =>
          captureTuple[q.reflect.Term, Kinds](
            RankedTermMatch(result.scalarBindings, result.sequenceBindings, compiled.holeNames),
            0
          )
        )
    )

  private inline def captureTuple[T, Kinds <: Tuple](
      result: RankedTermMatch[T],
      index: Int
  ): RankedCaptureTypes[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ScalarTermCapture *: tail) =>
        result.scalar(index) *: captureTuple[T, tail](result, index + 1)
      case _: (SequenceTermCapture *: tail) =>
        result.sequence(index) *: captureTuple[T, tail](result, index + 1)
