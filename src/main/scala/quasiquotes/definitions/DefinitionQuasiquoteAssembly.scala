package quasiquotes.definitions

import scala.collection.mutable

import quasiquotes.source.*
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

private[quasiquotes] final class DefinitionQuasiquoteAssemblyOccurrence private[
  definitions
] (
    val argumentIndex: Int,
    val semanticIdentity: String,
    val category: InterpolationCategory,
    val role: HoleRole,
    val assembledMarkerSpan: SourceSpan,
    val origin: SourceOrigin.InterpolationArgument
)

private[quasiquotes] final class DefinitionQuasiquoteAssembly private (
    val source: String,
    val sourceId: SourceId,
    val sourceMap: GeneratedSourceMap,
    val occurrences: Vector[DefinitionQuasiquoteAssemblyOccurrence],
    val termBindings: Map[String, ConstructedTerm],
    val typeBindings: Map[String, TypeNormalForm]
)

private[quasiquotes] object DefinitionQuasiquoteAssembly:
  import DefinitionQuasiquoteError.*

  def create(
      parts: Seq[String],
      arguments: Seq[DefinitionQuasiquoteArgument]
  ): Either[
    LocatedDiagnostic[DefinitionQuasiquoteError],
    DefinitionQuasiquoteAssembly
  ] =
    if parts == null || arguments == null then
      failure(
        InvalidAssemblySourceMetadata(
          "literal parts and interpolation arguments must be non-null collections"
        ),
        None
      )
    else if parts.size != arguments.size + 1 then
      failure(
        InvalidPartsArgumentArity(parts.size, arguments.size),
        wholeLiteralLocation(parts)
      )
    else if parts.exists(_ == null) then
      failure(
        InvalidAssemblySourceMetadata(
          s"literal part ${parts.indexWhere(_ == null)} is null"
        ),
        wholeLiteralLocation(parts)
      )
    else
      arguments.zipWithIndex.collectFirst {
        case (null, index) => index
      } match
        case Some(index) =>
          failure(NullDescriptor(index), None)
        case None =>
          build(parts.toVector, arguments.toVector)

  private def build(
      parts: Vector[String],
      arguments: Vector[DefinitionQuasiquoteArgument]
  ): Either[
    LocatedDiagnostic[DefinitionQuasiquoteError],
    DefinitionQuasiquoteAssembly
  ] =
    val sourceId = SourceId.DefinitionConstructionTemplate
    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val occurrences = mutable.ArrayBuffer.empty[DefinitionQuasiquoteAssemblyOccurrence]

    def appendPart(part: String, partIndex: Int): Unit =
      if part.nonEmpty then
        val start = builder.length
        builder.append(part)
        segments += GeneratedSegment(
          SourceSpan(start, builder.length),
          SourceOrigin.LiteralPart(
            sourceId,
            partIndex,
            SourceSpan(0, part.length)
          )
        )

    arguments.zipWithIndex.foreach { case (argument, index) =>
      appendPart(parts(index), index)
      val identity = s"definitionArgument$index"
      val marker = s"$$$identity"
      val category = categoryOf(argument)
      val role = roleFor(category)
      val start = builder.length
      builder.append(marker)
      val span = SourceSpan(start, builder.length)
      val origin = SourceOrigin.InterpolationArgument(sourceId, index, category)
      segments += GeneratedSegment(span, origin)
      occurrences += new DefinitionQuasiquoteAssemblyOccurrence(
        index,
        identity,
        category,
        role,
        span,
        origin
      )
    }
    appendPart(parts.last, parts.size - 1)

    val source = builder.toString
    val sourceMap = GeneratedSourceMap(source, sourceId, segments.toVector)
    validateCoverage(sourceMap).flatMap { _ =>
      validatePayloads(arguments, sourceMap, occurrences.toVector).map {
        case (termBindings, typeBindings) =>
          new DefinitionQuasiquoteAssembly(
            source,
            sourceId,
            sourceMap,
            occurrences.toVector,
            termBindings,
            typeBindings
          )
      }
    }

  private def validatePayloads(
      arguments: Vector[DefinitionQuasiquoteArgument],
      sourceMap: GeneratedSourceMap,
      occurrences: Vector[DefinitionQuasiquoteAssemblyOccurrence]
  ): Either[
    LocatedDiagnostic[DefinitionQuasiquoteError],
    (Map[String, ConstructedTerm], Map[String, TypeNormalForm])
  ] =
    val termBindings = mutable.LinkedHashMap.empty[String, ConstructedTerm]
    val typeBindings = mutable.LinkedHashMap.empty[String, TypeNormalForm]
    arguments.zip(occurrences).collectFirst {
      case (argument: DefinitionTypeArgument, occurrence)
          if argument.value == null =>
        NullDescriptorPayload(occurrence.argumentIndex, roleLabel(occurrence.category)) -> occurrence
      case (argument: BodyTermArgument, occurrence)
          if argument.value == null =>
        NullDescriptorPayload(occurrence.argumentIndex, roleLabel(occurrence.category)) -> occurrence
      case (argument: BodyTypeArgument, occurrence)
          if argument.value == null =>
        NullDescriptorPayload(occurrence.argumentIndex, roleLabel(occurrence.category)) -> occurrence
    } match
      case Some((error, occurrence)) =>
        failure(error, exactLocation(sourceMap, occurrence.assembledMarkerSpan))
      case None =>
        arguments.zip(occurrences).foreach {
          case (argument: DefinitionTypeArgument, occurrence) =>
            typeBindings += occurrence.semanticIdentity -> argument.value
          case (argument: BodyTermArgument, occurrence) =>
            termBindings += occurrence.semanticIdentity -> argument.value
          case (argument: BodyTypeArgument, occurrence) =>
            typeBindings += occurrence.semanticIdentity -> argument.value
        }
        Right(termBindings.toMap -> typeBindings.toMap)

  private def validateCoverage(
      sourceMap: GeneratedSourceMap
  ): Either[LocatedDiagnostic[DefinitionQuasiquoteError], Unit] =
    val spans = sourceMap.segments.map(_.generatedSpan)
    val complete =
      if sourceMap.generatedSource.isEmpty then spans.isEmpty
      else
        spans.nonEmpty &&
          spans.head.start == 0 &&
          spans.last.end == sourceMap.generatedSource.length &&
          spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          }
    Either.cond(
      complete,
      (),
      LocatedDiagnostic(
        InvalidAssemblySourceMetadata(
          "the initial literal/interpolation map must cover the complete assembled source"
        ),
        wholeLocation(sourceMap)
      )
    )

  private[definitions] def categoryOf(
      argument: DefinitionQuasiquoteArgument
  ): InterpolationCategory =
    argument match
      case _: DefinitionTypeArgument => InterpolationCategory.DefinitionTypeSplice
      case _: BodyTermArgument => InterpolationCategory.DefinitionBodyTermSplice
      case _: BodyTypeArgument => InterpolationCategory.DefinitionBodyTypeSplice

  private[definitions] def roleFor(
      category: InterpolationCategory
  ): HoleRole =
    category match
      case InterpolationCategory.DefinitionTypeSplice =>
        HoleRole.DefinitionTypeTemplate
      case InterpolationCategory.DefinitionBodyTermSplice =>
        HoleRole.DefinitionBodyTermTemplate
      case InterpolationCategory.DefinitionBodyTypeSplice =>
        HoleRole.DefinitionBodyTypeTemplate
      case other =>
        throw new IllegalArgumentException(
          s"Unsupported definition interpolation category: $other"
        )

  private[definitions] def roleLabel(
      category: InterpolationCategory
  ): String =
    category match
      case InterpolationCategory.DefinitionTypeSplice => "definition type"
      case InterpolationCategory.DefinitionBodyTermSplice => "body term"
      case InterpolationCategory.DefinitionBodyTypeSplice => "body type"
      case _ => "unsupported definition role"

  private def exactLocation(
      sourceMap: GeneratedSourceMap,
      span: SourceSpan
  ): Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      span,
      DiagnosticPrecision.ExactOccurrence
    )

  private def wholeLocation(
      sourceMap: GeneratedSourceMap
  ): Option[DiagnosticLocation] =
    Option
      .when(sourceMap.generatedSource.nonEmpty)(
        SourceSpan(0, sourceMap.generatedSource.length)
      )
      .flatMap(
        DiagnosticLocation.fromGeneratedMap(
          sourceMap,
          _,
          DiagnosticPrecision.WholeSource
        )
      )

  private def wholeLiteralLocation(
      parts: Seq[String]
  ): Option[DiagnosticLocation] =
    val source = parts.iterator.filter(_ != null).mkString
    DiagnosticLocation.direct(
      SourceId.DefinitionConstructionTemplate,
      SourceSpan(0, source.length),
      DiagnosticPrecision.WholeSource
    )

  private def failure(
      error: DefinitionQuasiquoteError,
      location: Option[DiagnosticLocation]
  ): Left[LocatedDiagnostic[DefinitionQuasiquoteError], Nothing] =
    Left(LocatedDiagnostic(error, location))
