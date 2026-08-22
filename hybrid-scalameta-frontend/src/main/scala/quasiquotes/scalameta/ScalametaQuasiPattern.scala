package quasiquotes.scalameta

import scala.quoted.Quotes

import quasiquotes.matching.TermMatcher

/** Bounded extractor for ordered captures from the opt-in pattern frontend. */
final class ScalametaTermPatternExtractor[T] private[scalameta] (
    extract: T => Option[Seq[T]]
):
  def unapplySeq(value: T): Option[Seq[T]] = extract(value)

/** Explicit opt-in matching syntax backed by the Scalameta-primary compiler. */
object ScalametaQuasiPattern:
  extension (context: StringContext)
    def qq(using q: Quotes): ScalametaTermPatternExtractor[q.reflect.Term] =
      import q.reflect.*

      val captureCount = context.parts.size - 1
      if captureCount <= 0 then
        report.errorAndAbort(
          "Invalid Scalameta qq term-pattern template: at least one term capture slot is required."
        )
      val captureNames = Vector.tabulate(captureCount)(index => s"qqCapture$index")
      val source = context.parts.zipWithIndex.foldLeft(new StringBuilder) {
        case (builder, (part, index)) =>
          builder.append(part)
          captureNames.lift(index).foreach(name => builder.append('$').append(name))
          builder
      }.toString

      TermFrontend.compile(source) match
        case Left(failure) =>
          report.errorAndAbort(
            s"Invalid Scalameta qq term-pattern template: ${failure.message}"
          )
        case Right(compiled) =>
          new ScalametaTermPatternExtractor[q.reflect.Term](term =>
            TermMatcher.matchTerm(compiled.pattern, term) match
              case Left(_) => None
              case Right(result) =>
                captureNames.foldLeft(Option(Vector.empty[q.reflect.Term])) {
                  case (captures, name) =>
                    captures.flatMap(current =>
                      result.bindings.get(name).map(current :+ _)
                    )
                }
          )
