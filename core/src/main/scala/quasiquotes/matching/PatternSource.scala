package quasiquotes.matching

import quasiquotes.source.*

final case class PatternSource(source: String, holes: Vector[String])

final case class MappedPatternSource(patternSource: PatternSource, originMap: GeneratedSourceMap, occurrences: Vector[HoleOccurrence]):
  lazy val generatedHoleIndex: GeneratedHoleIndex = GeneratedHoleIndex.fromOccurrences(occurrences)

object PatternSource:
  private val HolePrefix = "__qqhole_"

  def synthesize(pattern: String): Either[PatternError, PatternSource] =
    synthesizeMapped(pattern).map(_.patternSource)

  def synthesizeMapped(pattern: String): Either[PatternError, MappedPatternSource] =
    synthesizeMappedLocated(pattern).left.map(_.diagnostic)

  def synthesizeMappedLocated(pattern: String): Either[LocatedDiagnostic[PatternError], MappedPatternSource] =
    val scan = HoleSourceRewriter.scan(pattern, allowUnicodeIdentifiers = true)
    scan.invalidDollarSpans.headOption match
      case Some(span) =>
        Left(
          LocatedDiagnostic(
            PatternError.InvalidHoleName(pattern.slice(span.start, span.end)),
            DiagnosticLocation.direct(
              SourceId.TermPattern,
              span,
              DiagnosticPrecision.ExactOccurrence
            )
          )
        )
      case None =>
        val mapped = HoleSourceRewriter.rewriteScanned(
          pattern,
          scan,
          HolePrefix,
          HoleRole.TermPattern,
          SourceId.TermPattern,
          SourceId.VirtualTermPatternParserInput
        )
        Right(
          MappedPatternSource(
            PatternSource(mapped.generatedSource, mapped.occurrences.map(_.name)),
            mapped.originMap,
            mapped.occurrences
          )
        )

  def extractHoleName(identifier: String): Option[String] =
    Option.when(identifier.startsWith(HolePrefix))(identifier.stripPrefix(HolePrefix))
