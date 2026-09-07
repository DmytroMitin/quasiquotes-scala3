package quasiquotes.syntax

import scala.collection.mutable

/** Lexical dot runs only; each construction or pattern family owns placement and diagnostics. */
private[quasiquotes] object RankMarkerScanner:
  private sealed trait ScanState
  private case object CodeState extends ScanState
  private final case class StringState(
      tripleQuoted: Boolean,
      interpolated: Boolean,
      returnTo: ScanState
  ) extends ScanState
  private final case class InterpolationState(
      parent: StringState,
      braceDepth: Int
  ) extends ScanState
  private final case class CharacterState(returnTo: ScanState) extends ScanState
  private final case class BacktickState(returnTo: ScanState) extends ScanState

  def unquotedDotRuns(
      parts: Vector[String],
      includeInterpolationBodies: Boolean = true
  ): Vector[(Int, Int, Int)] =
    val runs = mutable.ArrayBuffer.empty[(Int, Int, Int)]
    var state: ScanState = CodeState
    var escaped = false
    var lineComment = false
    var blockCommentDepth = 0

    def interpolatedPrefix(part: String, quoteIndex: Int): Boolean =
      quoteIndex > 0 && {
        val previous = part.charAt(quoteIndex - 1)
        previous.isLetterOrDigit || previous == '_' || previous == '$'
      }

    parts.zipWithIndex.foreach { (part, partIndex) =>
      var index = 0
      while index < part.length do
        val current = part.charAt(index)
        val next = Option.when(index + 1 < part.length)(part.charAt(index + 1))
        state match
          case string: StringState =>
            if string.tripleQuoted && part.startsWith("\"\"\"", index) then
              state = string.returnTo
              index += 3
            else if !string.tripleQuoted && escaped then
              escaped = false
              index += 1
            else if !string.tripleQuoted && current == '\\' then
              escaped = true
              index += 1
            else if !string.tripleQuoted && current == '"' then
              state = string.returnTo
              index += 1
            else if string.interpolated && current == '$' && next.contains('{') then
              state = InterpolationState(string, 1)
              index += 2
            else index += 1
          case CharacterState(returnTo) =>
            if escaped then escaped = false
            else if current == '\\' then escaped = true
            else if current == '\'' then state = returnTo
            index += 1
          case BacktickState(returnTo) =>
            if current == '`' then state = returnTo
            index += 1
          case codeState @ (CodeState | InterpolationState(_, _)) =>
            if lineComment then
              if current == '\n' || current == '\r' then lineComment = false
              index += 1
            else if blockCommentDepth > 0 then
              if current == '/' && next.contains('*') then
                blockCommentDepth += 1
                index += 2
              else if current == '*' && next.contains('/') then
                blockCommentDepth -= 1
                index += 2
              else index += 1
            else
              if current == '/' && next.contains('/') then
                lineComment = true
                index += 2
              else if current == '/' && next.contains('*') then
                blockCommentDepth = 1
                index += 2
              else if current == '"' then
                val tripleQuoted = part.startsWith("\"\"\"", index)
                state = StringState(tripleQuoted, interpolatedPrefix(part, index), codeState)
                index += (if tripleQuoted then 3 else 1)
              else if current == '\'' then
                state = CharacterState(codeState)
                escaped = false
                index += 1
              else if current == '`' then
                state = BacktickState(codeState)
                index += 1
              else if current == '{' then
                state = codeState match
                  case InterpolationState(parent, depth) =>
                    InterpolationState(parent, depth + 1)
                  case CodeState => CodeState
                index += 1
              else if current == '}' then
                state = codeState match
                  case InterpolationState(parent, 1) => parent
                  case InterpolationState(parent, depth) =>
                    InterpolationState(parent, depth - 1)
                  case CodeState => CodeState
                index += 1
              else if current == '.' then
                val start = index
                while index < part.length && part.charAt(index) == '.' do index += 1
                if includeInterpolationBodies || codeState == CodeState then
                  runs += ((partIndex, start, index - start))
              else index += 1
    }
    runs.toVector

  def hasSplitDoubleDot(
      parts: Vector[String],
      runs: Vector[(Int, Int, Int)]
  ): Boolean =
    runs
      .groupBy(_._1)
      .exists { (partIndex, partRuns) =>
        partRuns.sortBy(_._2).sliding(2).exists {
          case Seq((_, firstOffset, 1), (_, secondOffset, 1)) =>
            parts(partIndex).substring(firstOffset + 1, secondOffset).forall(_.isWhitespace)
          case _ => false
        }
      }
