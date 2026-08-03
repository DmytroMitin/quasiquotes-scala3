package quasiquotes.definitions.dotty

import quasiquotes.source.SourceSpan

private[dotty] object RawDefinitionSpanAdapter:
  final case class NameEvidence(sourceSpelling: String, span: SourceSpan)

  def nameEvidence(
      source: String,
      definitionSpan: SourceSpan,
      typeSpan: SourceSpan,
      keyword: String,
      decodedName: String
  ): Either[RawDefinitionAdapterError, NameEvidence] =
    val limit = math.min(typeSpan.start, definitionSpan.end)
    if definitionSpan.start < 0 || limit > source.length || definitionSpan.start >= limit then
      indefensible
    else
      for
        keywordStart <- skipTrivia(source, definitionSpan.start, limit)
        keywordEnd = keywordStart + keyword.length
        _ <-
          Either.cond(
            keywordEnd <= limit &&
              source.regionMatches(keywordStart, keyword, 0, keyword.length) &&
              tokenBoundary(source, keywordEnd, limit),
            (),
            RawDefinitionAdapterError.IndefensibleComponentSpan("name")
          )
        nameStart <- skipTrivia(source, keywordEnd, limit)
        nameEnd <- scanNameEnd(source, nameStart, limit)
        spelling = source.substring(nameStart, nameEnd)
        decoded =
          if spelling.startsWith("`") && spelling.endsWith("`") then
            spelling.substring(1, spelling.length - 1)
          else spelling
        _ <-
          Either.cond(
            decoded == decodedName,
            (),
            RawDefinitionAdapterError.IndefensibleComponentSpan("name")
          )
      yield NameEvidence(spelling, SourceSpan(nameStart, nameEnd))

  private def skipTrivia(
      source: String,
      from: Int,
      limit: Int
  ): Either[RawDefinitionAdapterError, Int] =
    var cursor = from
    var done = false
    while !done && cursor < limit do
      if source.charAt(cursor).isWhitespace then cursor += 1
      else if cursor + 1 < limit && source.startsWith("//", cursor) then
        cursor += 2
        while cursor < limit && source.charAt(cursor) != '\n' && source.charAt(cursor) != '\r' do
          cursor += 1
      else if cursor + 1 < limit && source.startsWith("/*", cursor) then
        cursor += 2
        var depth = 1
        while cursor < limit && depth > 0 do
          if cursor + 1 < limit && source.startsWith("/*", cursor) then
            depth += 1
            cursor += 2
          else if cursor + 1 < limit && source.startsWith("*/", cursor) then
            depth -= 1
            cursor += 2
          else cursor += 1
        if depth != 0 then return indefensible
      else done = true
    Either.cond(cursor < limit, cursor, RawDefinitionAdapterError.IndefensibleComponentSpan("name"))

  private def scanNameEnd(
      source: String,
      start: Int,
      limit: Int
  ): Either[RawDefinitionAdapterError, Int] =
    if start >= limit then indefensible
    else if source.charAt(start) == '`' then
      val closing = source.indexOf('`', start + 1)
      Either.cond(
        closing > start + 1 && closing < limit,
        closing + 1,
        RawDefinitionAdapterError.IndefensibleComponentSpan("name")
      )
    else if isIdentifierStart(source.charAt(start)) then
      var cursor = start + 1
      while cursor < limit && isIdentifierPart(source.charAt(cursor)) do cursor += 1
      Right(cursor)
    else
      var cursor = start
      while cursor < limit && isOperatorPart(source.charAt(cursor)) do cursor += 1
      Either.cond(cursor > start, cursor, RawDefinitionAdapterError.IndefensibleComponentSpan("name"))

  private def tokenBoundary(source: String, offset: Int, limit: Int): Boolean =
    offset >= limit || !isIdentifierPart(source.charAt(offset))

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' || char == '$' || Character.isUnicodeIdentifierStart(char)

  private def isIdentifierPart(char: Char): Boolean =
    char == '$' || Character.isUnicodeIdentifierPart(char)

  private def isOperatorPart(char: Char): Boolean =
    !char.isWhitespace && !char.isLetterOrDigit && !"()[]{}=,:;.`'\"".contains(char)

  private def indefensible[A]: Left[RawDefinitionAdapterError, A] =
    Left(RawDefinitionAdapterError.IndefensibleComponentSpan("name"))
