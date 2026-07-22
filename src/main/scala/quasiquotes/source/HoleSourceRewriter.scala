package quasiquotes.source

import scala.collection.mutable

final case class HoleOccurrence(
    name: String,
    generatedName: String,
    originalSpan: SourceSpan,
    generatedSpan: SourceSpan,
    role: HoleRole
) derives CanEqual

final case class MappedHoleSource(
    generatedSource: String,
    occurrences: Vector[HoleOccurrence],
    originMap: GeneratedSourceMap
) derives CanEqual

object HoleSourceRewriter:
  def rewrite(
      source: String,
      generatedPrefix: String,
      role: HoleRole,
      originalSourceId: SourceId,
      generatedSourceId: SourceId,
      allowUnicodeIdentifiers: Boolean = false
  ): MappedHoleSource =
    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val occurrences = mutable.ArrayBuffer.empty[HoleOccurrence]
    var index = 0
    var literalStart = 0

    def appendOriginal(start: Int, end: Int): Unit =
      if start < end then
        val generatedStart = builder.length
        builder.append(source.substring(start, end))
        segments += GeneratedSegment(
          SourceSpan(generatedStart, builder.length),
          SourceOrigin.OriginalText(originalSourceId, SourceSpan(start, end))
        )

    while index < source.length do
      if source.charAt(index) == '$' &&
          index + 1 < source.length &&
          isIdentifierStart(source.charAt(index + 1), allowUnicodeIdentifiers)
      then
        appendOriginal(literalStart, index)
        val nameStart = index + 1
        var end = nameStart + 1
        while end < source.length && isIdentifierPart(source.charAt(end), allowUnicodeIdentifiers) do end += 1
        val name = source.substring(nameStart, end)
        val generatedName = s"$generatedPrefix$name"
        val generatedStart = builder.length
        builder.append(generatedName)
        val originalSpan = SourceSpan(index, end)
        val generatedSpan = SourceSpan(generatedStart, builder.length)
        segments += GeneratedSegment(
          generatedSpan,
          SourceOrigin.RewrittenHole(originalSourceId, originalSpan, name, role)
        )
        occurrences += HoleOccurrence(name, generatedName, originalSpan, generatedSpan, role)
        index = end
        literalStart = end
      else
        index += 1

    appendOriginal(literalStart, source.length)
    val generatedSource = builder.toString
    MappedHoleSource(
      generatedSource,
      occurrences.toVector,
      GeneratedSourceMap(generatedSource, generatedSourceId, segments.toVector)
    )

  private def isIdentifierStart(char: Char, allowUnicodeIdentifiers: Boolean): Boolean =
    char == '_' ||
      (if allowUnicodeIdentifiers then char.isLetter
       else ('A' <= char && char <= 'Z') || ('a' <= char && char <= 'z'))

  private def isIdentifierPart(char: Char, allowUnicodeIdentifiers: Boolean): Boolean =
    isIdentifierStart(char, allowUnicodeIdentifiers) ||
      (if allowUnicodeIdentifiers then char.isDigit else '0' <= char && char <= '9')
