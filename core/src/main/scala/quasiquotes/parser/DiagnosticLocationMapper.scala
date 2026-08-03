package quasiquotes.parser

import quasiquotes.source.*

/** Maps structured parser spans and conservative whole-input spans through a generated-source map. */
object DiagnosticLocationMapper:
  def fromParseError(error: ParseError, sourceMap: GeneratedSourceMap): Option[DiagnosticLocation] =
    error.diagnostics.iterator
      .flatMap(_.generatedSpan)
      .flatMap(
        DiagnosticLocation.fromGeneratedMap(
          sourceMap,
          _,
          DiagnosticPrecision.ExactOccurrence
        )
      )
      .nextOption()

  def wholeSource(
      sourceMap: GeneratedSourceMap,
      preferredSpan: Option[SourceSpan] = None
  ): Option[DiagnosticLocation] =
    val fullSpan = Option.when(sourceMap.generatedSource.nonEmpty)(SourceSpan(0, sourceMap.generatedSource.length))
    (preferredSpan.toList ++ fullSpan.toList).iterator
      .flatMap(
        DiagnosticLocation.fromGeneratedMap(
          sourceMap,
          _,
          DiagnosticPrecision.WholeSource
        )
      )
      .nextOption()
