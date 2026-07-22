package quasiquotes.parser

import quasiquotes.source.*

/** Maps structured parser spans and conservative whole-input spans through a generated-source map. */
object DiagnosticLocationMapper:
  def fromParseError(error: ParseError, sourceMap: GeneratedSourceMap): Option[DiagnosticLocation] =
    error.diagnostics.iterator
      .flatMap(_.generatedSpan)
      .find(span => !span.isEmpty && span.end <= sourceMap.generatedSource.length)
      .flatMap(DiagnosticLocation.from(sourceMap, _))

  def wholeGeneratedSource(
      sourceMap: GeneratedSourceMap,
      preferredSpan: Option[SourceSpan] = None
  ): Option[DiagnosticLocation] =
    preferredSpan
      .filter(span => !span.isEmpty && span.end <= sourceMap.generatedSource.length)
      .orElse(Option.when(sourceMap.generatedSource.nonEmpty)(SourceSpan(0, sourceMap.generatedSource.length)))
      .flatMap(DiagnosticLocation.from(sourceMap, _))
