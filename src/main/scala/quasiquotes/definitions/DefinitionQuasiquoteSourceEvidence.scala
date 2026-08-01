package quasiquotes.definitions

import quasiquotes.source.*

private[quasiquotes] final class DefinitionQuasiquoteInterpolationOccurrence private[
  definitions
] (
    val argumentIndex: Int,
    val semanticIdentity: String,
    val category: InterpolationCategory,
    val role: HoleRole,
    val assembledMarkerSpan: SourceSpan,
    val finalGeneratedMarkerSpan: SourceSpan,
    val origin: SourceOrigin.InterpolationArgument
)

private[quasiquotes] final class DefinitionQuasiquoteSourceEvidence private (
    val sourceId: SourceId,
    val sourceMap: GeneratedSourceMap,
    val components: DefinitionComponentSpans,
    val interpolationOccurrences: Vector[
      DefinitionQuasiquoteInterpolationOccurrence
    ]
)

private[quasiquotes] object DefinitionQuasiquoteSourceEvidence:
  import DefinitionQuasiquoteError.InvalidCompletedSourceEvidence

  def create(
      sourceId: SourceId,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans,
      assemblyOccurrences: Vector[DefinitionQuasiquoteAssemblyOccurrence],
      locatedOccurrences: Vector[HoleOccurrence]
  ): Either[DefinitionQuasiquoteError, DefinitionQuasiquoteSourceEvidence] =
    for
      _ <- validateIdentityAndCoverage(sourceId, sourceMap, components)
      _ <- validateCounts(assemblyOccurrences, locatedOccurrences)
      occurrences <- assemblyOccurrences
        .zip(locatedOccurrences)
        .foldLeft[
          Either[
            DefinitionQuasiquoteError,
            Vector[DefinitionQuasiquoteInterpolationOccurrence]
          ]
        ](Right(Vector.empty)) { case (result, (assembly, located)) =>
          result.flatMap(values =>
            validateOccurrence(sourceMap, assembly, located)
              .map(values :+ _)
          )
        }
      _ <- validateInterpolationPartition(sourceMap, occurrences)
    yield
      new DefinitionQuasiquoteSourceEvidence(
        sourceId,
        sourceMap,
        components,
        occurrences
      )

  private def validateIdentityAndCoverage(
      sourceId: SourceId,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans
  ): Either[DefinitionQuasiquoteError, Unit] =
    val spans = sourceMap.segments.map(_.generatedSpan)
    if sourceMap.generatedSourceId != sourceId then
      invalid("the source map identity must equal the result source identity")
    else if components.definition.end > sourceMap.generatedSource.length then
      invalid("definition component spans must fit inside the final generated source")
    else if
      sourceMap.generatedSource.nonEmpty &&
        (spans.isEmpty || spans.head.start != 0 ||
          spans.last.end != sourceMap.generatedSource.length ||
          !spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          })
    then
      invalid("the final generated map must be complete and contiguous")
    else Right(())

  private def validateCounts(
      assembly: Vector[DefinitionQuasiquoteAssemblyOccurrence],
      located: Vector[HoleOccurrence]
  ): Either[DefinitionQuasiquoteError, Unit] =
    if assembly.map(_.argumentIndex) != assembly.indices.toVector then
      invalid("argument indices must be complete, ordered, and zero-based")
    else if assembly.size != located.size then
      invalid("assembled and located interpolation occurrence counts must agree")
    else Right(())

  private def validateOccurrence(
      sourceMap: GeneratedSourceMap,
      assembly: DefinitionQuasiquoteAssemblyOccurrence,
      located: HoleOccurrence
  ): Either[
    DefinitionQuasiquoteError,
    DefinitionQuasiquoteInterpolationOccurrence
  ] =
    val expectedIdentity = s"definitionArgument${assembly.argumentIndex}"
    val expectedRole = DefinitionQuasiquoteAssembly.roleFor(assembly.category)
    val matchingSegments = sourceMap.segments.filter { segment =>
      segment.generatedSpan == located.generatedSpan &&
      segment.origin == assembly.origin
    }
    if assembly.semanticIdentity != expectedIdentity then
      invalid("semantic interpolation identity must be derived only from argument order")
    else if assembly.origin.argumentIndex != assembly.argumentIndex then
      invalid("interpolation origin argument index must match occurrence order")
    else if assembly.origin.category != assembly.category then
      invalid("interpolation origin category must match the descriptor category")
    else if assembly.role != expectedRole || located.role != expectedRole then
      invalid("interpolation category and located transport role must correspond exactly")
    else if located.name != expectedIdentity then
      invalid("located semantic identity must match the interpolation occurrence")
    else if located.originalSpan != assembly.assembledMarkerSpan then
      invalid("located original span must equal the assembled semantic marker span")
    else if located.generatedSpan.isEmpty then
      invalid("final generated marker spans must be nonempty")
    else if matchingSegments.size != 1 then
      invalid("every interpolation occurrence must have one exact final mapped origin")
    else
      Right(
        new DefinitionQuasiquoteInterpolationOccurrence(
          assembly.argumentIndex,
          assembly.semanticIdentity,
          assembly.category,
          assembly.role,
          assembly.assembledMarkerSpan,
          located.generatedSpan,
          assembly.origin
        )
      )

  private def validateInterpolationPartition(
      sourceMap: GeneratedSourceMap,
      occurrences: Vector[DefinitionQuasiquoteInterpolationOccurrence]
  ): Either[DefinitionQuasiquoteError, Unit] =
    val mapped = sourceMap.segments.collect {
      case segment @ GeneratedSegment(
            _,
            _: SourceOrigin.InterpolationArgument
          ) =>
        segment
    }
    val projected = occurrences.map(occurrence =>
      GeneratedSegment(occurrence.finalGeneratedMarkerSpan, occurrence.origin)
    )
    Either.cond(
      mapped == projected,
      (),
      InvalidCompletedSourceEvidence(
        "interpolation occurrences must form one exact ordered partition of final interpolation origins"
      )
    )

  private def invalid(
      detail: String
  ): Left[DefinitionQuasiquoteError, Nothing] =
    Left(InvalidCompletedSourceEvidence(detail))
