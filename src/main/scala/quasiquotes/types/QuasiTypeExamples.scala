package quasiquotes.types

import quasiquotes.parser.TinyTypeParser
import scala.quoted.*

object QuasiTypeExamples:
  inline def supportedConstructionSummary: List[String] =
    ${ supportedConstructionSummaryImpl }

  inline def matches(patternSource: String, targetSource: String): Boolean =
    ${ matchesImpl('patternSource, 'targetSource) }

  inline def unsupportedMessage(source: String): String =
    ${ unsupportedMessageImpl('source) }

  private def supportedConstructionSummaryImpl(using Quotes): Expr[List[String]] =
    import quotes.reflect.*
    val sources = List("Int", "String", "Boolean", "List[Int]", "Option[String]", "(Int, String)", "Int => String")
    val rendered = sources.map { source =>
      QuasiTypeRepr.fromSource(source) match
        case Right(quasiType) => s"$source -> ${quasiType.renderedTypeRepr}"
        case Left(error) => s"$source -> ERROR: ${error.message}"
    }
    Expr.ofList(rendered.map(Expr(_)))

  private def matchesImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[Boolean] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val result =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        targetShape <- TinyTypeParser.parse(targetText).left.map(error => TypeQuasiquoteError(error.summary)).map(_.shape)
        targetRepr <- TypeReprLowerer.lower(targetShape)
      yield pattern.matchTypeRepr(targetRepr)
    Expr(result.getOrElse(false))

  private def unsupportedMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    QuasiTypeRepr.fromSource(sourceText) match
      case Left(error) => Expr(error.message)
      case Right(quasiType) => Expr(s"Unexpectedly supported ${quasiType.renderedTypeRepr}")
