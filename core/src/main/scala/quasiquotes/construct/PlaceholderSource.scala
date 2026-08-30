package quasiquotes.construct

import scala.collection.mutable

import quasiquotes.source.*
import quasiquotes.types.ConstructedType

final case class PlaceholderSource[+T](source: String, holes: Vector[T])

private[construct] sealed trait QuasiquoteHole[+T, +ReflectedType]

private[construct] object QuasiquoteHole:
  final case class Term[+T](term: T) extends QuasiquoteHole[T, Nothing]
  final case class TermSequence[+T](terms: Vector[T]) extends QuasiquoteHole[T, Nothing]
  final case class ReflectedTypeSplice[+ReflectedType](reflectedType: ReflectedType)
      extends QuasiquoteHole[Nothing, ReflectedType]
  final case class ConstructedTypeSplice(constructedType: ConstructedType)
      extends QuasiquoteHole[Nothing, Nothing]
  final case class SelectedMemberNameSplice(selectedMemberName: SelectedMemberName)
      extends QuasiquoteHole[Nothing, Nothing]

private[construct] final case class PlaceholderBinding[+T, +ReflectedType](
    name: String,
    hole: QuasiquoteHole[T, ReflectedType]
)

private[construct] final case class CategorizedPlaceholderSource[+T, +ReflectedType](
    source: String,
    bindings: Vector[PlaceholderBinding[T, ReflectedType]],
    literalCategorizedNames: Set[String],
    originMap: GeneratedSourceMap
)

object PlaceholderSource:
  private val CategorizedNamePattern =
    "__qq_(?:term|terms|reflected_type|type|name)_hole_[0-9]+(?:_[0-9]+)*".r

  def synthesize[T](parts: Seq[String], holes: Seq[T]): Either[QuasiquoteError, PlaceholderSource[T]] =
    if parts.length != holes.length + 1 then
      Left(
        QuasiquoteError.HoleCountMismatch(
          expected = parts.length - 1,
          actual = holes.length
        )
      )
    else
      val builder = new StringBuilder(parts.head)
      holes.zipWithIndex.foreach { (hole, index) =>
        builder.append(s"__hole$index")
        builder.append(parts(index + 1))
      }
      Right(PlaceholderSource[T](builder.result(), holes.toVector))

  private[construct] def synthesizeCategorized[T, ReflectedType](
      parts: Seq[String],
      holes: Seq[QuasiquoteHole[T, ReflectedType]]
  ): Either[QuasiquoteError, CategorizedPlaceholderSource[T, ReflectedType]] =
    if parts.length != holes.length + 1 then
      Left(
        QuasiquoteError.HoleCountMismatch(
          expected = parts.length - 1,
          actual = holes.length
        )
      )
    else
      val adjustedParts = parts.toVector
      val rankMarkerOffsets = unquotedDoubleDotOffsets(adjustedParts).toSet
      val consumedParts = holes.zipWithIndex.foldLeft[Either[QuasiquoteError, Vector[String]]](Right(adjustedParts)) {
        case (result, (hole, index)) =>
          result.flatMap { currentParts =>
            val preceding = currentParts(index)
            val hasRankMarker =
              preceding.length >= 2 &&
                rankMarkerOffsets.contains(index -> (preceding.length - 2))
            hole match
              case _: QuasiquoteHole.TermSequence[?] if hasRankMarker =>
                Right(currentParts.updated(index, preceding.dropRight(2)))
              case _: QuasiquoteHole.TermSequence[?] =>
                Left(QuasiquoteError.MissingSequenceTermRankMarker(index))
              case _ if hasRankMarker =>
                Left(
                  QuasiquoteError.SequenceTermRankMarkerCategoryMismatch(
                    index,
                    categoryOf(hole)
                  )
                )
              case _ => Right(currentParts)
          }
      }

      consumedParts.flatMap { effectiveParts =>
        val consumedMarkers = holes.zipWithIndex.collect {
          case (_: QuasiquoteHole.TermSequence[?], index) =>
            index -> (parts(index).length - 2)
        }.toSet
        rankMarkerOffsets.diff(consumedMarkers).toVector.sortBy(identity).headOption match
          case Some((partIndex, _)) =>
            Left(QuasiquoteError.OrphanSequenceTermRankMarker(partIndex))
          case None => synthesizeValidatedCategorized(effectiveParts, parts, holes)
      }

  private def synthesizeValidatedCategorized[T, ReflectedType](
      effectiveParts: Vector[String],
      originalParts: Seq[String],
      holes: Seq[QuasiquoteHole[T, ReflectedType]]
  ): Either[QuasiquoteError, CategorizedPlaceholderSource[T, ReflectedType]] =
      val builder = new StringBuilder
      val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
      val literalSource = originalParts.mkString
      val literalCategorizedNames = CategorizedNamePattern.findAllIn(literalSource).toSet
      var generatedNames = Set.empty[String]

      def appendLiteral(part: String, partIndex: Int): Unit =
        if part.nonEmpty then
          val generatedStart = builder.length
          builder.append(part)
          segments += GeneratedSegment(
            SourceSpan(generatedStart, builder.length),
            SourceOrigin.LiteralPart(
              SourceId.TermConstructionTemplate,
              partIndex,
              SourceSpan(0, part.length)
            )
          )

      appendLiteral(effectiveParts.head, 0)
      val bindings = holes.zipWithIndex.map { (hole, index) =>
        val baseName = hole match
          case _: QuasiquoteHole.Term[?] => s"__qq_term_hole_$index"
          case _: QuasiquoteHole.TermSequence[?] => s"__qq_terms_hole_$index"
          case _: QuasiquoteHole.ReflectedTypeSplice[?] =>
            s"__qq_reflected_type_hole_$index"
          case _: QuasiquoteHole.ConstructedTypeSplice => s"__qq_type_hole_$index"
          case _: QuasiquoteHole.SelectedMemberNameSplice => s"__qq_name_hole_$index"
        val name = freshCategorizedName(baseName, literalSource, generatedNames)
        generatedNames += name
        val generatedStart = builder.length
        val isWholeGuestInterpolationArgument =
          hole.isInstanceOf[QuasiquoteHole.Term[?]] &&
            insideSInterpolationLiteral(builder.result())
        if isWholeGuestInterpolationArgument then builder.append('$')
        builder.append(name)
        val category = hole match
          case _: QuasiquoteHole.Term[?] => InterpolationCategory.TermSplice
          case _: QuasiquoteHole.TermSequence[?] => InterpolationCategory.TermSplice
          case _: QuasiquoteHole.ReflectedTypeSplice[?] =>
            InterpolationCategory.ReflectedTypeSplice
          case _: QuasiquoteHole.ConstructedTypeSplice => InterpolationCategory.ConstructedTypeSplice
          case _: QuasiquoteHole.SelectedMemberNameSplice =>
            InterpolationCategory.SelectedMemberNameSplice
        segments += GeneratedSegment(
          SourceSpan(generatedStart, builder.length),
          SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, index, category)
        )
        appendLiteral(effectiveParts(index + 1), index + 1)
        PlaceholderBinding(name, hole)
      }
      val generatedSource = builder.result()
      Right(
        CategorizedPlaceholderSource(
          generatedSource,
          bindings.toVector,
          literalCategorizedNames,
          GeneratedSourceMap(generatedSource, SourceId.VirtualExpressionParserInput, segments.toVector)
        )
      )

  private def categoryOf[T, ReflectedType](
      hole: QuasiquoteHole[T, ReflectedType]
  ): PlaceholderCategory =
    hole match
      case _: QuasiquoteHole.Term[?] => PlaceholderCategory.TermSplice
      case _: QuasiquoteHole.TermSequence[?] => PlaceholderCategory.TermSequenceSplice
      case _: QuasiquoteHole.ReflectedTypeSplice[?] => PlaceholderCategory.ReflectedTypeSplice
      case _: QuasiquoteHole.ConstructedTypeSplice => PlaceholderCategory.ConstructedTypeSplice
      case _: QuasiquoteHole.SelectedMemberNameSplice => PlaceholderCategory.SelectedMemberNameSplice

  private def unquotedDoubleDotOffsets(parts: Vector[String]): Vector[(Int, Int)] =
    val offsets = mutable.ArrayBuffer.empty[(Int, Int)]
    var quote: Option[Char] = None
    var escaped = false
    var lineComment = false
    var blockCommentDepth = 0

    parts.zipWithIndex.foreach { (part, partIndex) =>
      var index = 0
      while index < part.length do
        val current = part.charAt(index)
        val next = Option.when(index + 1 < part.length)(part.charAt(index + 1))

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
          quote match
            case Some(delimiter) =>
              if escaped then escaped = false
              else if current == '\\' && delimiter != '`' then escaped = true
              else if current == delimiter then quote = None
              index += 1
            case None =>
              if current == '/' && next.contains('/') then
                lineComment = true
                index += 2
              else if current == '/' && next.contains('*') then
                blockCommentDepth = 1
                index += 2
              else if current == '"' || current == '\'' || current == '`' then
                quote = Some(current)
                index += 1
              else if current == '.' && next.contains('.') then
                offsets += partIndex -> index
                index += 2
              else index += 1
    }

    offsets.toVector

  private[construct] def isCategorizedName(name: String): Boolean =
    CategorizedNamePattern.matches(name)

  private def freshCategorizedName(baseName: String, literalSource: String, generatedNames: Set[String]): String =
    Iterator.from(0)
      .map(attempt => if attempt == 0 then baseName else s"${baseName}_$attempt")
      .find(candidate => !literalSource.contains(candidate) && !generatedNames.contains(candidate))
      .get

  /** True only in the literal-text region of a single-quoted standard `s`
    * interpolation. A hole inside an existing `${...}` guest expression remains
    * an ordinary identifier splice and therefore does not receive another `$`.
    */
  private def insideSInterpolationLiteral(source: String): Boolean =
    var index = 0
    var inInterpolation = false
    var guestBraceDepth = 0
    var escaped = false

    while index < source.length do
      val current = source.charAt(index)
      if !inInterpolation then
        if current == 's' && index + 1 < source.length && source.charAt(index + 1) == '"' &&
            (index == 0 || !isIdentifierPart(source.charAt(index - 1))) &&
            !(index + 3 < source.length && source.substring(index + 1, index + 4) == "\"\"\"")
        then
          inInterpolation = true
          escaped = false
          index += 2
        else index += 1
      else if guestBraceDepth == 0 then
        if escaped then
          escaped = false
          index += 1
        else if current == '\\' then
          escaped = true
          index += 1
        else if current == '"' then
          inInterpolation = false
          index += 1
        else if current == '$' && index + 1 < source.length && source.charAt(index + 1) == '{' then
          guestBraceDepth = 1
          index += 2
        else index += 1
      else
        if current == '{' then guestBraceDepth += 1
        else if current == '}' then guestBraceDepth -= 1
        index += 1

    inInterpolation && guestBraceDepth == 0

  private def isIdentifierPart(char: Char): Boolean =
    char == '_' || char.isLetterOrDigit
