package quasiquotes.hybrid

import scala.meta.Dialect
import scala.quoted.Quotes

import quasiquotes.construct.{QuasiTypeSplice, QuasiquoteBuilder}
import quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import quasiquotes.matching.{QuasiPattern, TermPattern}
import quasiquotes.matching.hybrid.HybridPatternFrontend

/** Explicit, immutable selector for the unpublished typed-term harness. */
object TermQ3FrontendSelector:
  enum Selection derives CanEqual:
    case CurrentDotty
    case ScalametaPrimary

  enum Engine derives CanEqual:
    case CurrentDotty
    case Scalameta
    case CurrentDottyFallback

  final case class BuildResult[T](
      term: T,
      engine: Engine,
      primaryFailure: Option[ScalametaTermFrontend.Failure]
  )

  final case class CompileResult(
      pattern: TermPattern,
      engine: Engine,
      primaryFailure: Option[ScalametaTermFrontend.Failure]
  )

  def build(using q: Quotes)(
      selection: Selection,
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice],
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[ScalametaTermFrontend.Failure, BuildResult[q.reflect.Term]] =
    selection match
      case Selection.CurrentDotty =>
        QuasiquoteBuilder.build(parts, arguments)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
          .map(BuildResult(_, Engine.CurrentDotty, None))
      case Selection.ScalametaPrimary =>
        HybridTermFrontend.build(parts, arguments, dialect).map(result =>
          BuildResult(
            result.term,
            result.engine match
              case HybridTermFrontend.Engine.Scalameta => Engine.Scalameta
              case HybridTermFrontend.Engine.CurrentDottyFallback => Engine.CurrentDottyFallback,
            result.primaryFailure
          )
        )

  def compile(
      selection: Selection,
      source: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[ScalametaTermFrontend.Failure, CompileResult] =
    selection match
      case Selection.CurrentDotty =>
        QuasiPattern.term(source)
          .left.map(error => ScalametaTermFrontend.Failure.template(error.message))
          .map(result => CompileResult(result.pattern, Engine.CurrentDotty, None))
      case Selection.ScalametaPrimary =>
        HybridPatternFrontend.compile(source, dialect).map(result =>
          CompileResult(
            result.pattern,
            result.engine match
              case HybridPatternFrontend.Engine.Scalameta => Engine.Scalameta
              case HybridPatternFrontend.Engine.CurrentDottyFallback => Engine.CurrentDottyFallback,
            result.primaryFailure
          )
        )
