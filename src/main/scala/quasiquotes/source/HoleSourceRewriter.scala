package quasiquotes.source

import scala.collection.mutable

final case class HoleOccurrence(
    name: String,
    generatedName: String,
    originalSpan: SourceSpan,
    generatedSpan: SourceSpan,
    role: HoleRole
) derives CanEqual

final case class GeneratedHoleIndex private (private val semanticNamesByGeneratedName: Map[String, String])
    derives CanEqual:
  def semanticNameFor(generatedIdentifier: String): Option[String] =
    semanticNamesByGeneratedName.get(generatedIdentifier)

  def generatedNameFor(semanticName: String): Option[String] =
    semanticNamesByGeneratedName.collectFirst { case (generatedName, `semanticName`) => generatedName }

object GeneratedHoleIndex:
  val empty: GeneratedHoleIndex = GeneratedHoleIndex(Map.empty)

  def fromOccurrences(occurrences: Iterable[HoleOccurrence]): GeneratedHoleIndex =
    val bindings = occurrences.iterator.map(occurrence => occurrence.generatedName -> occurrence.name).toVector
    require(
      bindings.groupMap(_._1)(_._2).values.forall(_.distinct.size == 1),
      "A generated hole identifier must map to exactly one semantic hole name"
    )
    require(
      bindings.groupMap(_._2)(_._1).values.forall(_.distinct.size == 1),
      "A semantic hole name must reuse exactly one generated identifier"
    )
    GeneratedHoleIndex(bindings.toMap)

final case class MappedHoleSource(
    generatedSource: String,
    occurrences: Vector[HoleOccurrence],
    originMap: GeneratedSourceMap
) derives CanEqual:
  lazy val generatedHoleIndex: GeneratedHoleIndex = GeneratedHoleIndex.fromOccurrences(occurrences)

object HoleSourceRewriter:
  private[quasiquotes] final case class ScannedHole(name: String, start: Int, end: Int)

  private[quasiquotes] final case class SourceScan(
      literalIdentifiers: Set[String],
      holes: Vector[ScannedHole],
      invalidDollarSpans: Vector[SourceSpan]
  )

  def rewrite(
      source: String,
      generatedPrefix: String,
      role: HoleRole,
      originalSourceId: SourceId,
      generatedSourceId: SourceId,
      allowUnicodeIdentifiers: Boolean = false
  ): MappedHoleSource =
    val sourceScan = scan(source, allowUnicodeIdentifiers)
    rewriteScanned(
      source,
      sourceScan,
      generatedPrefix,
      role,
      originalSourceId,
      generatedSourceId
    )

  /** Restores semantic `$hole` spellings in user-facing text without changing
    * prefix-, suffix-, or substring-sharing literal identifiers.
    *
    * This deliberately uses the same identifier policy as source scanning.
    */
  private[quasiquotes] def restoreSemanticHoleIdentifiers(
      text: String,
      mapped: MappedHoleSource,
      allowUnicodeIdentifiers: Boolean
  ): String =
    val builder = new StringBuilder
    var index = 0

    while index < text.length do
      val current = text.charAt(index)
      if isIdentifierStart(current, allowUnicodeIdentifiers) then
        val start = index
        index += 1
        while index < text.length && isIdentifierPart(text.charAt(index), allowUnicodeIdentifiers) do
          index += 1
        val identifier = text.substring(start, index)
        mapped.generatedHoleIndex.semanticNameFor(identifier) match
          case Some(semanticName) => builder.append('$').append(semanticName)
          case None => builder.append(identifier)
      else
        builder.append(current)
        index += 1

    builder.toString

  private[quasiquotes] def rewriteScanned(
      source: String,
      scan: SourceScan,
      generatedPrefix: String,
      role: HoleRole,
      originalSourceId: SourceId,
      generatedSourceId: SourceId
  ): MappedHoleSource =
    val generatedNames = assignGeneratedNames(scan, generatedPrefix)
    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val occurrences = mutable.ArrayBuffer.empty[HoleOccurrence]
    var literalStart = 0

    def appendOriginal(start: Int, end: Int): Unit =
      if start < end then
        val generatedStart = builder.length
        builder.append(source.substring(start, end))
        segments += GeneratedSegment(
          SourceSpan(generatedStart, builder.length),
          SourceOrigin.OriginalText(originalSourceId, SourceSpan(start, end))
        )

    scan.holes.foreach { hole =>
      appendOriginal(literalStart, hole.start)
      val generatedName = generatedNames(hole.name)
      val generatedStart = builder.length
      builder.append(generatedName)
      val originalSpan = SourceSpan(hole.start, hole.end)
      val generatedSpan = SourceSpan(generatedStart, builder.length)
      segments += GeneratedSegment(
        generatedSpan,
        SourceOrigin.RewrittenHole(originalSourceId, originalSpan, hole.name, role)
      )
      occurrences += HoleOccurrence(hole.name, generatedName, originalSpan, generatedSpan, role)
      literalStart = hole.end
    }

    appendOriginal(literalStart, source.length)
    val generatedSource = builder.toString
    MappedHoleSource(
      generatedSource,
      occurrences.toVector,
      GeneratedSourceMap(generatedSource, generatedSourceId, segments.toVector)
    )

  private def assignGeneratedNames(scan: SourceScan, generatedPrefix: String): Map[String, String] =
    val usedNames = mutable.Set.from(scan.literalIdentifiers)
    val generatedNames = mutable.LinkedHashMap.empty[String, String]

    scan.holes.foreach { hole =>
      generatedNames.getOrElseUpdate(
        hole.name,
        freshName(s"$generatedPrefix${hole.name}", usedNames)
      )
    }
    generatedNames.toMap

  private def freshName(baseName: String, usedNames: mutable.Set[String]): String =
    var suffix = 0
    var candidate = baseName
    while usedNames(candidate) do
      suffix += 1
      candidate = s"${baseName}_$suffix"
    usedNames += candidate
    candidate

  private[quasiquotes] def scan(source: String, allowUnicodeIdentifiers: Boolean): SourceScan =
    val literalIdentifiers = mutable.Set.empty[String]
    val holes = mutable.ArrayBuffer.empty[ScannedHole]
    val invalidDollarSpans = mutable.ArrayBuffer.empty[SourceSpan]
    var index = 0

    while index < source.length do
      val current = source.charAt(index)
      if current == '"' then
        index = skipQuoted(source, index, '"')
      else if current == '\'' then
        index = skipQuoted(source, index, '\'')
      else if current == '`' then
        val end = source.indexOf('`', index + 1)
        if end < 0 then index = source.length
        else
          literalIdentifiers += source.substring(index + 1, end)
          index = end + 1
      else if current == '/' && index + 1 < source.length && source.charAt(index + 1) == '/' then
        val end = source.indexOf('\n', index + 2)
        index = if end < 0 then source.length else end + 1
      else if current == '/' && index + 1 < source.length && source.charAt(index + 1) == '*' then
        index = skipBlockComment(source, index)
      else if current == '$' &&
          index + 1 < source.length &&
          isIdentifierStart(source.charAt(index + 1), allowUnicodeIdentifiers)
      then
        val nameStart = index + 1
        var end = nameStart + 1
        while end < source.length && isIdentifierPart(source.charAt(end), allowUnicodeIdentifiers) do end += 1
        holes += ScannedHole(source.substring(nameStart, end), index, end)
        index = end
      else if current == '$' then
        invalidDollarSpans += SourceSpan(index, math.min(index + 2, source.length))
        index += 1
      else if isIdentifierStart(current, allowUnicodeIdentifiers) then
        val start = index
        index += 1
        while index < source.length && isIdentifierPart(source.charAt(index), allowUnicodeIdentifiers) do index += 1
        literalIdentifiers += source.substring(start, index)
      else
        index += 1

    SourceScan(literalIdentifiers.toSet, holes.toVector, invalidDollarSpans.toVector)

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

  private def skipBlockComment(source: String, start: Int): Int =
    var index = start + 2
    var depth = 1
    while index < source.length && depth > 0 do
      if index + 1 < source.length && source.charAt(index) == '/' && source.charAt(index + 1) == '*' then
        depth += 1
        index += 2
      else if index + 1 < source.length && source.charAt(index) == '*' && source.charAt(index + 1) == '/' then
        depth -= 1
        index += 2
      else
        index += 1
    index

  private def isIdentifierStart(char: Char, allowUnicodeIdentifiers: Boolean): Boolean =
    char == '_' ||
      (if allowUnicodeIdentifiers then char.isLetter
       else ('A' <= char && char <= 'Z') || ('a' <= char && char <= 'z'))

  private def isIdentifierPart(char: Char, allowUnicodeIdentifiers: Boolean): Boolean =
    isIdentifierStart(char, allowUnicodeIdentifiers) ||
      (if allowUnicodeIdentifiers then char.isDigit else '0' <= char && char <= '9')
