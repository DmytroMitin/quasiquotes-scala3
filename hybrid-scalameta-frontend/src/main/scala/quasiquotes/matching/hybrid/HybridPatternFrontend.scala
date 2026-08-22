package quasiquotes.matching.hybrid

import scala.meta.Dialect

import quasiquotes.construct.hybrid.ScalametaTermFrontend
import quasiquotes.matching.{QuasiPattern, TermPattern}
import quasiquotes.hybrid.TermQ3DialectPolicy

private[quasiquotes] object HybridPatternFrontend:
  enum Engine derives CanEqual:
    case Scalameta
    case CurrentDottyFallback

  final case class CompileResult(
      pattern: TermPattern,
      engine: Engine,
      primaryFailure: Option[ScalametaTermFrontend.Failure]
  )

  def compile(
      source: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[ScalametaTermFrontend.Failure, CompileResult] =
    ScalametaPatternFrontend.compile(source, dialect) match
      case Right(pattern) => Right(CompileResult(pattern, Engine.Scalameta, None))
      case Left(primary) if primary.category == "SCALAMETA_PARSE_FAILURE" =>
        QuasiPattern.term(source)
          .left.map(error => ScalametaTermFrontend.Failure.template(error.message))
          .map(pattern => CompileResult(pattern.pattern, Engine.CurrentDottyFallback, Some(primary)))
      case Left(failure) => Left(failure)
