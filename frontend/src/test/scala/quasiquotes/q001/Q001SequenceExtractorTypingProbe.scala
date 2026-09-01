package quasiquotes.q001

import scala.quoted.*

import quasiquotes.matching.TargetTermView

/** Q001-only extractor protocol probe. This is deliberately test-only and is
  * not the production matcher selected by the feasibility result.
  */
final class Q001ProductTermExtractor[T, Captures <: Tuple](
    extract: T => Option[Captures]
):
  def unapply(value: T): Option[Captures] = extract(value)

object Q001RankAwareProbe:
  extension (inline context: StringContext)
    transparent inline def qq(using q: Quotes) =
      ${ sequenceExtractor('context, 'q) }

  private def sequenceExtractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q001 probe requires a statically known StringContext.",
          context
        )

    if parts == List("", "(..", ")") then
      '{
        Q001SequenceExtractorFactory.sequenceExtractor(using $callerQuotes)
      }
    else if !parts.exists(_.endsWith("..")) then
      '{
        Q001SequenceExtractorFactory.scalarExtractor($context)(using $callerQuotes)
      }
    else
      quotes.reflect.report.errorAndAbort(
        s"Q001 probe does not implement this ranked extractor layout: $parts.",
        context
      )

object Q001SequenceExtractorFactory:
  def scalarExtractor(context: StringContext)(using q: Quotes):
      quasiquotes.matching.TermPatternExtractor[q.reflect.Term] =
    quasiquotes.matching.QuasiPattern.qq(context)(using q)

  def sequenceExtractor(using q: Quotes):
      Q001ProductTermExtractor[
        q.reflect.Term,
        (q.reflect.Term, Seq[q.reflect.Term])
  ] =
    new Q001ProductTermExtractor(term =>
      TargetTermView.fromTerm(term) match
        case Right(TargetTermView.Apply(function, arguments, _)) =>
          Some((function.original, arguments.map(_.original)))
        case _ => None
    )
