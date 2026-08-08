package quasiquotes.matching

import scala.collection.mutable

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
    if containsSingleQuotedSInterpolation(pattern) then
      synthesizeInterpolationAware(pattern)
    else synthesizeOrdinary(pattern)

  private def synthesizeOrdinary(pattern: String): Either[LocatedDiagnostic[PatternError], MappedPatternSource] =
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

  private def synthesizeInterpolationAware(pattern: String): Either[LocatedDiagnostic[PatternError], MappedPatternSource] =
    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val occurrences = mutable.ArrayBuffer.empty[HoleOccurrence]
    val generatedBySemanticName = mutable.LinkedHashMap.empty[String, String]
    var index = 0
    var inSInterpolation = false
    var escaped = false

    def appendOriginal(start: Int, end: Int, text: String = ""): Unit =
      val generatedStart = builder.length
      if text.nonEmpty then builder.append(text)
      else builder.append(pattern.substring(start, end))
      if builder.length > generatedStart then
        segments += GeneratedSegment(
          SourceSpan(generatedStart, builder.length),
          SourceOrigin.OriginalText(SourceId.TermPattern, SourceSpan(start, end))
        )

    def generatedName(name: String): String =
      generatedBySemanticName.getOrElseUpdate(
        name,
        Iterator.from(0)
          .map(attempt => if attempt == 0 then s"$HolePrefix$name" else s"$HolePrefix${name}_$attempt")
          .find(candidate => !pattern.contains(candidate) && !generatedBySemanticName.values.exists(_ == candidate))
          .get
      )

    def appendHole(start: Int, end: Int, name: String, guestDollar: Boolean): Unit =
      if guestDollar then appendOriginal(start, start + 1)
      val generated = generatedName(name)
      val generatedStart = builder.length
      builder.append(generated)
      val generatedSpan = SourceSpan(generatedStart, builder.length)
      val originalSpan = SourceSpan(start, end)
      segments += GeneratedSegment(
        generatedSpan,
        SourceOrigin.RewrittenHole(SourceId.TermPattern, originalSpan, name, HoleRole.TermPattern)
      )
      occurrences += HoleOccurrence(name, generated, originalSpan, generatedSpan, HoleRole.TermPattern)

    while index < pattern.length do
      if inSInterpolation then
        val current = pattern.charAt(index)
        if escaped then
          appendOriginal(index, index + 1)
          escaped = false
          index += 1
        else if current == '\\' then
          appendOriginal(index, index + 1)
          escaped = true
          index += 1
        else if current == '"' then
          appendOriginal(index, index + 1)
          inSInterpolation = false
          index += 1
        else if current == '$' && index + 2 < pattern.length && pattern.charAt(index + 1) == '$' && isIdentifierStart(pattern.charAt(index + 2)) then
          appendOriginal(index, index + 2, "$")
          index += 2
        else if current == '$' && index + 1 < pattern.length && isIdentifierStart(pattern.charAt(index + 1)) then
          val nameStart = index + 1
          var end = nameStart + 1
          while end < pattern.length && isIdentifierPart(pattern.charAt(end)) do end += 1
          appendHole(index, end, pattern.substring(nameStart, end), guestDollar = true)
          index = end
        else
          appendOriginal(index, index + 1)
          index += 1
      else
        val current = pattern.charAt(index)
        if isSInterpolationStart(pattern, index) then
          appendOriginal(index, index + 2)
          inSInterpolation = true
          index += 2
        else if current == '"' || current == '\'' then
          val end = skipQuoted(pattern, index, current)
          appendOriginal(index, end)
          index = end
        else if current == '$' && index + 1 < pattern.length && isIdentifierStart(pattern.charAt(index + 1)) then
          val nameStart = index + 1
          var end = nameStart + 1
          while end < pattern.length && isIdentifierPart(pattern.charAt(end)) do end += 1
          appendHole(index, end, pattern.substring(nameStart, end), guestDollar = false)
          index = end
        else if current == '$' then
          val span = SourceSpan(index, math.min(index + 2, pattern.length))
          return Left(
            LocatedDiagnostic(
              PatternError.InvalidHoleName(pattern.slice(span.start, span.end)),
              DiagnosticLocation.direct(SourceId.TermPattern, span, DiagnosticPrecision.ExactOccurrence)
            )
          )
        else
          appendOriginal(index, index + 1)
          index += 1

    val generatedSource = builder.toString
    Right(
      MappedPatternSource(
        PatternSource(generatedSource, occurrences.map(_.name).toVector),
        GeneratedSourceMap(generatedSource, SourceId.VirtualTermPatternParserInput, segments.toVector),
        occurrences.toVector
      )
    )

  private def containsSingleQuotedSInterpolation(source: String): Boolean =
    source.indices.exists(isSInterpolationStart(source, _))

  private def isSInterpolationStart(source: String, index: Int): Boolean =
    index + 1 < source.length && source.charAt(index) == 's' && source.charAt(index + 1) == '"' &&
      (index == 0 || !isIdentifierPart(source.charAt(index - 1))) &&
      !(index + 3 < source.length && source.substring(index + 1, index + 4) == "\"\"\"")

  private def skipQuoted(source: String, start: Int, delimiter: Char): Int =
    var index = start + 1
    var escaped = false
    while index < source.length do
      val current = source.charAt(index)
      if escaped then escaped = false
      else if current == '\\' then escaped = true
      else if current == delimiter then return index + 1
      index += 1
    source.length

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' || char.isLetter

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || char.isDigit

  def extractHoleName(identifier: String): Option[String] =
    Option.when(identifier.startsWith(HolePrefix))(identifier.stripPrefix(HolePrefix))
