package quasiquotes.matching

import scala.quoted.*
import quasiquotes.scalameta.TermFrontend

object Q078ScalametaInterpolator:
  extension (inline sc: StringContext)
    transparent inline def q078(using q: Quotes) =
      ${ Q078ScalametaMacro.extractor('sc, 'q) }

private[matching] object Q078ScalametaMacro:
  def extractor(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    Q078GenericMacro.extractor(context, callerQuotes, compilePrimary)

  private def compilePrimary(source: String): Either[String, TermPattern] =
    TermFrontend.compile(source).left.map(_.message).flatMap { compiled =>
      if compiled.engine != TermFrontend.Engine.Scalameta || compiled.primaryFailure.nonEmpty then
        Left(s"Q078 requires Scalameta primary: ${compiled.engine} / ${compiled.primaryFailure}")
      else Right(compiled.pattern)
    }
