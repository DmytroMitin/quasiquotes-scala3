package quasiquotes.matching

import scala.collection.mutable

final case class PatternSource(source: String, holes: Vector[String])

object PatternSource:
  private val HolePrefix = "__qqhole_"

  def synthesize(pattern: String): Either[PatternError, PatternSource] =
    val builder = new StringBuilder
    val holes = mutable.ArrayBuffer.empty[String]
    var index = 0

    while index < pattern.length do
      val current = pattern.charAt(index)
      if current == '$' then
        // Task 3 keeps pattern syntax tiny: a hole is just `$` plus an identifier.
        val start = index + 1
        if start >= pattern.length || !isIdentifierStart(pattern.charAt(start)) then
          return Left(PatternError.InvalidHoleName(pattern.drop(index).take(2)))
        var end = start + 1
        while end < pattern.length && isIdentifierPart(pattern.charAt(end)) do end += 1
        val holeName = pattern.substring(start, end)
        holes += holeName
        builder.append(HolePrefix).append(holeName)
        index = end
      else
        builder.append(current)
        index += 1

    Right(PatternSource(builder.result(), holes.toVector))

  def extractHoleName(identifier: String): Option[String] =
    Option.when(identifier.startsWith(HolePrefix))(identifier.stripPrefix(HolePrefix))

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' || char.isLetter

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || char.isDigit
