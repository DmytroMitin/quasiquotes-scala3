package quasiquotes.construct.hybrid

import scala.quoted.Quotes
import scala.meta.Dialect

import quasiquotes.construct.{QuasiTypeSplice, QuasiquoteBuilder}
import quasiquotes.hybrid.TermQ3DialectPolicy

private[quasiquotes] object HybridTermFrontend:
  enum Engine derives CanEqual:
    case Scalameta
    case CurrentDottyFallback

  final case class BuildResult[T](
      term: T,
      engine: Engine,
      primaryFailure: Option[ScalametaTermFrontend.Failure]
  )

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice],
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[ScalametaTermFrontend.Failure, BuildResult[q.reflect.Term]] =
    ScalametaTermFrontend.lower(parts, arguments, dialect) match
      case Right(term) => Right(BuildResult(term, Engine.Scalameta, None))
      case Left(primary) if primary.category == "SCALAMETA_PARSE_FAILURE" =>
        QuasiquoteBuilder.build(parts, arguments)
          .left.map(error => ScalametaTermFrontend.Failure.lowering(error.message))
          .map(term => BuildResult(term, Engine.CurrentDottyFallback, Some(primary)))
      case Left(failure) => Left(failure)
