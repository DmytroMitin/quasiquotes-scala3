package quasiquotes.matching

import quasiquotes.source.*

final case class PatternSource(source: String, holes: Vector[String])

final case class MappedPatternSource(patternSource: PatternSource, originMap: GeneratedSourceMap, occurrences: Vector[HoleOccurrence])

object PatternSource:
  private val HolePrefix = "__qqhole_"

  def synthesize(pattern: String): Either[PatternError, PatternSource] =
    synthesizeMapped(pattern).map(_.patternSource)

  def synthesizeMapped(pattern: String): Either[PatternError, MappedPatternSource] =
    synthesizeMappedLocated(pattern).left.map(_.diagnostic)

  def synthesizeMappedLocated(pattern: String): Either[LocatedDiagnostic[PatternError], MappedPatternSource] =
    var index = 0

    while index < pattern.length do
      val current = pattern.charAt(index)
      if current == '$' then
        // Task 3 keeps pattern syntax tiny: a hole is just `$` plus an identifier.
        val start = index + 1
        if start >= pattern.length || !isIdentifierStart(pattern.charAt(start)) then
          val span = SourceSpan(index, math.min(index + 2, pattern.length))
          val location = DiagnosticLocation(
            SourceId.TermPattern,
            span,
            Vector(SourceOrigin.OriginalText(SourceId.TermPattern, span))
          )
          return Left(LocatedDiagnostic(PatternError.InvalidHoleName(pattern.drop(index).take(2)), Some(location)))
        var end = start + 1
        while end < pattern.length && isIdentifierPart(pattern.charAt(end)) do end += 1
        index = end
      else
        index += 1

    val mapped = HoleSourceRewriter.rewrite(
      pattern,
      HolePrefix,
      HoleRole.TermPattern,
      SourceId.TermPattern,
      SourceId.VirtualTermPatternParserInput,
      allowUnicodeIdentifiers = true
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

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' || char.isLetter

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || char.isDigit
