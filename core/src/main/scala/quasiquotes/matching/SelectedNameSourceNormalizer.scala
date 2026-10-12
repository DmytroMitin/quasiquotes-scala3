package quasiquotes.matching

import scala.collection.mutable

import quasiquotes.definitions.DefinitionName
import quasiquotes.source.{
  GeneratedHoleIndex,
  GeneratedSegment,
  GeneratedSourceMap,
  HoleOccurrence,
  HoleSourceRewriter,
  SourceOrigin,
  SourceSpan
}

private[quasiquotes] final case class SelectedNameSourcePosition(
    sourceOccurrenceIndex: Int,
    selectOrdinal: Int
) derives CanEqual

private[quasiquotes] sealed trait SelectedNameSourceNormalizationError
    derives CanEqual:
  def message: String

private[quasiquotes] object SelectedNameSourceNormalizationError:
  case object MissingOriginalSource
      extends SelectedNameSourceNormalizationError:
    def message: String = "Original selected-name pattern source must be present."

  case object MissingSelectedPositions
      extends SelectedNameSourceNormalizationError:
    def message: String = "Selected-name source positions must be present."

  final case class SelectedPositionCount(actual: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected-name source normalization requires exactly two positions, got $actual."

  final case class MissingSelectedPosition(index: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected-name source position $index must be present."

  final case class InvalidSourceOccurrenceIndex(index: Int, occurrenceCount: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected source occurrence index $index is outside 0 until $occurrenceCount."

  final case class InvalidSelectOrdinal(ordinal: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected source Select ordinal must be non-negative, got $ordinal."

  final case class DuplicateSourceOccurrenceIndex(index: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected source occurrence index $index is duplicated."

  final case class DuplicateSelectOrdinal(ordinal: Int)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected source Select ordinal $ordinal is duplicated."

  final case class UnsupportedSelectedSemanticName(name: String)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Selected source name $name is outside the current neutral plain-name intersection."

  final case class CaptureCategoryConflict(name: String)
      extends SelectedNameSourceNormalizationError:
    def message: String =
      s"Source name $name is used by both an ordinary Term hole and a selected-name occurrence."

  final case class PublicPatternSourceError(error: PatternError)
      extends SelectedNameSourceNormalizationError:
    def message: String = error.message

private[quasiquotes] final case class NormalizedSelectedNameSource(
    originalSource: String,
    publicMapped: MappedPatternSource,
    generatedSource: String,
    sourceMap: GeneratedSourceMap,
    occurrences: Vector[HoleOccurrence],
    ordinaryHoleIndex: GeneratedHoleIndex,
    selectedOccurrences: Vector[SelectedNamePatternOccurrence]
) derives CanEqual

private[quasiquotes] object SelectedNameSourceNormalizer:
  import SelectedNameSourceNormalizationError.*

  private val SelectedMarker = "selected"

  def normalize(
      originalSource: String,
      selected: Vector[SelectedNameSourcePosition]
  ): Either[SelectedNameSourceNormalizationError, NormalizedSelectedNameSource] =
    for
      source <- Option(originalSource).toRight(MissingOriginalSource)
      positions <- Option(selected).toRight(MissingSelectedPositions)
      _ <- Either.cond(
        positions.size == 2,
        (),
        SelectedPositionCount(positions.size)
      )
      _ <- positions.zipWithIndex
        .collectFirst { case (null, index) => MissingSelectedPosition(index) }
        .toLeft(())
      publicMapped <- PatternSource
        .synthesizeMapped(source)
        .left
        .map(PublicPatternSourceError.apply)
      _ <- validatePositions(positions, publicMapped.occurrences.size)
      selectedNames =
        positions.map(position =>
          publicMapped.occurrences(position.sourceOccurrenceIndex).name
        )
      _ <- selectedNames
        .find(name => DefinitionName.plain(name).isLeft)
        .map(UnsupportedSelectedSemanticName.apply)
        .toLeft(())
      selectedIndexes = positions.map(_.sourceOccurrenceIndex).toSet
      ordinaryNames = publicMapped.occurrences.zipWithIndex.collect {
        case (occurrence, index) if !selectedIndexes(index) => occurrence.name
      }.toSet
      _ <- selectedNames
        .find(ordinaryNames)
        .map(CaptureCategoryConflict.apply)
        .toLeft(())
      normalized <- build(source, publicMapped, positions)
    yield normalized

  private def validatePositions(
      positions: Vector[SelectedNameSourcePosition],
      occurrenceCount: Int
  ): Either[SelectedNameSourceNormalizationError, Unit] =
    positions
      .find(position =>
        position.sourceOccurrenceIndex < 0 ||
          position.sourceOccurrenceIndex >= occurrenceCount
      )
      .map(position =>
        InvalidSourceOccurrenceIndex(
          position.sourceOccurrenceIndex,
          occurrenceCount
        )
      )
      .toLeft(())
      .flatMap { _ =>
        positions
          .find(_.selectOrdinal < 0)
          .map(position => InvalidSelectOrdinal(position.selectOrdinal))
          .toLeft(())
      }
      .flatMap { _ =>
        duplicate(positions.map(_.sourceOccurrenceIndex))
          .map(DuplicateSourceOccurrenceIndex.apply)
          .toLeft(())
      }
      .flatMap { _ =>
        duplicate(positions.map(_.selectOrdinal))
          .map(DuplicateSelectOrdinal.apply)
          .toLeft(())
      }

  private def duplicate(values: Vector[Int]): Option[Int] =
    values.groupBy(identity).collectFirst {
      case (value, occurrences) if occurrences.size > 1 => value
    }

  private def build(
      source: String,
      publicMapped: MappedPatternSource,
      positions: Vector[SelectedNameSourcePosition]
  ): Either[SelectedNameSourceNormalizationError, NormalizedSelectedNameSource] =
    val positionBySourceIndex =
      positions.map(position => position.sourceOccurrenceIndex -> position).toMap
    val scan = HoleSourceRewriter.scan(source, allowUnicodeIdentifiers = true)
    val usedNames = mutable.Set.from(
      scan.literalIdentifiers ++
        publicMapped.occurrences.map(_.generatedName)
    )
    val normalizedNames =
      publicMapped.occurrences.zipWithIndex.map { case (occurrence, index) =>
        positionBySourceIndex.get(index) match
          case None => occurrence.generatedName
          case Some(position) =>
            freshName(
              s"${occurrence.generatedName}__${SelectedMarker}_${position.selectOrdinal}",
              usedNames
            )
      }

    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val occurrences = mutable.ArrayBuffer.empty[HoleOccurrence]
    var occurrenceIndex = 0

    publicMapped.originMap.segments.foreach { segment =>
      val generatedStart = builder.length
      segment.origin match
        case rewritten: SourceOrigin.RewrittenHole =>
          val occurrence = publicMapped.occurrences(occurrenceIndex)
          val generatedName = normalizedNames(occurrenceIndex)
          builder.append(generatedName)
          val generatedSpan = SourceSpan(generatedStart, builder.length)
          segments += GeneratedSegment(generatedSpan, rewritten)
          occurrences += occurrence.copy(
            generatedName = generatedName,
            generatedSpan = generatedSpan
          )
          occurrenceIndex += 1
        case original =>
          builder.append(
            publicMapped.patternSource.source.substring(
              segment.generatedSpan.start,
              segment.generatedSpan.end
            )
          )
          segments += GeneratedSegment(
            SourceSpan(generatedStart, builder.length),
            original
          )
    }

    val generatedSource = builder.toString
    val normalizedOccurrences = occurrences.toVector
    val selectedIndexes = positionBySourceIndex.keySet
    val ordinaryOccurrences =
      normalizedOccurrences.zipWithIndex.collect {
        case (occurrence, index) if !selectedIndexes(index) => occurrence
      }
    val selectedOccurrences = positions.map { position =>
      val occurrence =
        normalizedOccurrences(position.sourceOccurrenceIndex)
      SelectedNamePatternOccurrence(
        occurrence.name,
        position.selectOrdinal,
        occurrence.generatedName
      )
    }

    Right(
      NormalizedSelectedNameSource(
        source,
        publicMapped,
        generatedSource,
        GeneratedSourceMap(
          generatedSource,
          publicMapped.originMap.generatedSourceId,
          segments.toVector
        ),
        normalizedOccurrences,
        GeneratedHoleIndex.fromOccurrences(ordinaryOccurrences),
        selectedOccurrences
      )
    )

  private def freshName(
      base: String,
      usedNames: mutable.Set[String]
  ): String =
    var candidate = base
    var suffix = 0
    while usedNames(candidate) do
      suffix += 1
      candidate = s"${base}_$suffix"
    usedNames += candidate
    candidate
