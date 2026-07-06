package quasiquotes.types

import scala.quoted.*

object QuasiTypeExamples:
  inline def supportedConstructionSummary: List[String] =
    ${ supportedConstructionSummaryImpl }

  inline def matches(patternSource: String, targetSource: String): Boolean =
    ${ matchesImpl('patternSource, 'targetSource) }

  inline def matchSummary(patternSource: String, targetSource: String): String =
    ${ matchSummaryImpl('patternSource, 'targetSource) }

  inline def structuralNormalFormSummary(source: String): String =
    ${ structuralNormalFormSummaryImpl('source) }

  inline def structuralMatches(patternSource: String, targetSource: String): Boolean =
    ${ structuralMatchesImpl('patternSource, 'targetSource) }

  inline def equalityComparisonSummary(patternSource: String, targetSource: String): String =
    ${ equalityComparisonSummaryImpl('patternSource, 'targetSource) }

  inline def matchingSubstrateSummary(patternSource: String): String =
    ${ matchingSubstrateSummaryImpl('patternSource) }

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
    val result = QuasiTypePattern.matchesSource(patternText, targetText)
    Expr(result.getOrElse(false))

  private def matchSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val operator = if QuasiTypePattern.matchesSource(patternText, targetText).getOrElse(false) then "==" else "!="
    Expr(s"$patternText $operator $targetText")

  private def structuralNormalFormSummaryImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    Expr(TypeNormalForm.fromSource(sourceText).fold(_.message, _.render))

  private def structuralMatchesImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[Boolean] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    Expr(TypeNormalForm.equalSources(patternText, targetText).getOrElse(false))

  private def equalityComparisonSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val exact = QuasiTypePattern.matchesSourceByRenderedTypeRepr(patternText, targetText).getOrElse(false)
    val structural = TypeNormalForm.equalSources(patternText, targetText).getOrElse(false)
    Expr(s"exact=$exact structural=$structural")

  private def matchingSubstrateSummaryImpl(patternSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val pattern = QuasiTypePattern.reprOrThrow(patternText)
    Expr(pattern.matchingSubstrateSummary)

  private def unsupportedMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    QuasiTypeRepr.fromSource(sourceText) match
      case Left(error) => Expr(error.message)
      case Right(quasiType) => Expr(s"Unexpectedly supported ${quasiType.renderedTypeRepr}")
