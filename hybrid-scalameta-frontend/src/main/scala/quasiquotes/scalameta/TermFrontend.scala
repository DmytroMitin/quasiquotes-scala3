package quasiquotes.scalameta

import scala.meta.Dialect
import scala.quoted.Quotes

import quasiquotes.construct.QuasiTypeSplice
import quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import quasiquotes.hybrid.TermQ3DialectPolicy
import quasiquotes.matching.TermPattern
import quasiquotes.matching.hybrid.HybridPatternFrontend

/** Explicit programmatic entry point for the Scalameta-primary Term frontend.
  * Importing or calling this object never changes the current public frontend.
  */
object TermFrontend:
  enum Engine derives CanEqual:
    case Scalameta
    case CurrentDottyFallback

  final case class Failure(
      category: String,
      start: Int,
      end: Int,
      detail: String
  ) derives CanEqual:
    def message: String = s"$category[$start..$end]: $detail"

  final case class BuildResult[T](
      term: T,
      engine: Engine,
      primaryFailure: Option[Failure]
  )

  final case class CompileResult(
      pattern: TermPattern,
      engine: Engine,
      primaryFailure: Option[Failure]
  )

  val defaultDialectName: String = TermQ3DialectPolicy.selectedName

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice],
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, BuildResult[q.reflect.Term]] =
    HybridTermFrontend
      .build(parts, arguments, dialect)
      .left
      .map(fromInternalFailure)
      .map(result =>
        BuildResult(
          result.term,
          result.engine match
            case HybridTermFrontend.Engine.Scalameta => Engine.Scalameta
            case HybridTermFrontend.Engine.CurrentDottyFallback =>
              Engine.CurrentDottyFallback,
          result.primaryFailure.map(fromInternalFailure)
        )
      )

  def compile(
      source: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, CompileResult] =
    HybridPatternFrontend
      .compile(source, dialect)
      .left
      .map(fromInternalFailure)
      .map(result =>
        CompileResult(
          result.pattern,
          result.engine match
            case HybridPatternFrontend.Engine.Scalameta => Engine.Scalameta
            case HybridPatternFrontend.Engine.CurrentDottyFallback =>
              Engine.CurrentDottyFallback,
          result.primaryFailure.map(fromInternalFailure)
        )
      )

  private def fromInternalFailure(
      failure: ScalametaTermFrontend.Failure
  ): Failure =
    Failure(failure.category, failure.start, failure.end, failure.detail)
