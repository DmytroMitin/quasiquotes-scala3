package quasiquotes.definitions

import quasiquotes.source.*
import quasiquotes.terms.{ConstructedTerm, LocatedTermTemplate}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] final class LocatedDefinitionTemplate private (
    val template: DefinitionTemplate,
    val sourceId: SourceId,
    val sourceMap: GeneratedSourceMap,
    val components: DefinitionComponentSpans,
    val definitionTypeOccurrences: Vector[HoleOccurrence],
    val body: LocatedTermTemplate
) derives CanEqual:
  def complete(
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[
    LocatedDiagnostic[DefinitionConstructionError],
    ConstructedDefinition
  ] =
    template
      .complete(termBindings, typeBindings)
      .left
      .map(error => LocatedDiagnostic(error, locationFor(error)))

  def render: String =
    s"LocatedDefinitionTemplate(template=${template.render}, sourceId=${sourceId.value}, sourceMetadata=present)"

  override def equals(other: Any): Boolean =
    other match
      case that: LocatedDefinitionTemplate =>
        template == that.template &&
          sourceId == that.sourceId &&
          sourceMap == that.sourceMap &&
          components == that.components &&
          definitionTypeOccurrences == that.definitionTypeOccurrences &&
          body == that.body
      case _ =>
        false

  override def hashCode: Int =
    (
      template,
      sourceId,
      sourceMap,
      components,
      definitionTypeOccurrences,
      body
    ).hashCode

  override def toString: String =
    render

  private def locationFor(
      error: DefinitionConstructionError
  ): Option[DiagnosticLocation] =
    bindingLocationRequest(error)
      .flatMap(uniqueRelevantOccurrenceSpan)
      .flatMap(
        DiagnosticLocation.fromGeneratedMap(
          sourceMap,
          _,
          DiagnosticPrecision.ExactOccurrence
        )
      )
      .orElse(wholeDefinitionLocation)

  private enum BindingLocationRequest:
    case Term(name: String)
    case Type(name: String)

  private def bindingLocationRequest(
      error: DefinitionConstructionError
  ): Option[BindingLocationRequest] =
    error match
      case DefinitionConstructionError.MissingTermBinding(name) =>
        Some(BindingLocationRequest.Term(name))
      case DefinitionConstructionError.MissingTypeBinding(name) =>
        Some(BindingLocationRequest.Type(name))
      case DefinitionConstructionError.InvalidTypeBinding(name, _) =>
        Some(BindingLocationRequest.Type(name))
      case _ =>
        None

  private def uniqueRelevantOccurrenceSpan(
      request: BindingLocationRequest
  ): Option[SourceSpan] =
    val spans =
      request match
        case BindingLocationRequest.Term(name) =>
          body.termOccurrences.collect {
            case occurrence if occurrence.semantic.name == name =>
              occurrence.source.generatedSpan
          }
        case BindingLocationRequest.Type(name) =>
          definitionTypeOccurrences.collect {
            case occurrence if occurrence.name == name =>
              occurrence.generatedSpan
          } ++
            body.typeOccurrences.collect {
              case occurrence if occurrence.name == name =>
                occurrence.generatedSpan
            }
    Option.when(spans.size == 1)(spans.head)

  private def wholeDefinitionLocation: Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      components.definition,
      DiagnosticPrecision.WholeSource
    )

private[quasiquotes] object LocatedDefinitionTemplate:
  def create(
      template: DefinitionTemplate,
      sourceId: SourceId,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans,
      definitionTypeOccurrences: Vector[HoleOccurrence],
      body: LocatedTermTemplate
  ): Either[DefinitionError, LocatedDefinitionTemplate] =
    for
      _ <- validateSupportedTemplate(template)
      _ <- validateIdentity(sourceId, sourceMap, body)
      _ <- validateCoverage(
        sourceMap.generatedSource.length,
        sourceMap.segments.map(_.generatedSpan)
      )
      _ <- validateComponents(sourceMap, components)
      _ <- validateName(template, sourceMap, components)
      _ <- validateDefinitionTypeOccurrences(
        template,
        sourceMap,
        components,
        definitionTypeOccurrences
      )
      _ <- validateBody(template, sourceMap, components, body)
      _ <- validateCompleteOccurrencePartition(
        sourceMap,
        definitionTypeOccurrences,
        body
      )
    yield
      new LocatedDefinitionTemplate(
        template,
        sourceId,
        sourceMap,
        components,
        definitionTypeOccurrences,
        body
      )

  private def validateSupportedTemplate(
      template: DefinitionTemplate
  ): Either[DefinitionError, Unit] =
    template match
      case _: DefinitionTemplate.SingleParameterDef =>
        invalid(
          "single-parameter definition templates require separate parameter-name and parameter-type evidence"
        )
      case _: DefinitionTemplate.TwoParameterDef =>
        invalid(
          "two-parameter definition templates require separate name and type evidence for both parameter declarations"
        )
      case _ => Right(())

  private[quasiquotes] def validateCoverageForTest(
      generatedSourceLength: Int,
      spans: Vector[SourceSpan]
  ): Either[DefinitionError, Unit] =
    validateCoverage(generatedSourceLength, spans)

  private def validateIdentity(
      sourceId: SourceId,
      sourceMap: GeneratedSourceMap,
      body: LocatedTermTemplate
  ): Either[DefinitionError, Unit] =
    if sourceMap.generatedSourceId != sourceId then
      invalid(
        "the generated source map identity must equal the located definition source identity"
      )
    else if body.sourceMap != sourceMap then
      invalid(
        "the located body must retain the complete definition source map"
      )
    else Right(())

  private def validateCoverage(
      generatedSourceLength: Int,
      spans: Vector[SourceSpan]
  ): Either[DefinitionError, Unit] =
    val complete =
      if generatedSourceLength == 0 then spans.isEmpty
      else
        spans.nonEmpty &&
          spans.head.start == 0 &&
          spans.last.end == generatedSourceLength &&
          spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          }
    Either.cond(
      complete,
      (),
      DefinitionError.InvalidSourceMetadata(
        "the generated source map must cover the complete generated definition without gaps or overlaps"
      )
    )

  private def validateComponents(
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans
  ): Either[DefinitionError, Unit] =
    Either.cond(
      components.definition.end <= sourceMap.generatedSource.length,
      (),
      DefinitionError.InvalidSourceMetadata(
        "the definition component spans must fit inside the generated source"
      )
    )

  private def validateName(
      template: DefinitionTemplate,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans
  ): Either[DefinitionError, Unit] =
    val spelling =
      sourceMap.generatedSource.slice(
        components.name.start,
        components.name.end
      )
    Either.cond(
      spelling == template.name.source,
      (),
      DefinitionError.InvalidSourceMetadata(
        "the name component must equal DefinitionName.source exactly"
      )
    )

  private def validateDefinitionTypeOccurrences(
      template: DefinitionTemplate,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans,
      occurrences: Vector[HoleOccurrence]
  ): Either[DefinitionError, Unit] =
    val expected =
      TypeTemplate.holeOccurrences(definitionType(template))
    if occurrences.map(_.name) != expected then
      invalid(
        "definition-type occurrences must match the semantic type-template occurrence order"
      )
    else
      occurrences.foldLeft[Either[DefinitionError, Unit]](Right(())) {
        (result, occurrence) =>
          result.flatMap { _ =>
            if occurrence.role != HoleRole.DefinitionTypeTemplate then
              invalid(
                "definition-type occurrences must use the definition-type role"
              )
            else if !contains(
                components.declaredType,
                occurrence.generatedSpan
              )
            then
              invalid(
                "every definition-type occurrence must lie inside the explicit definition type"
              )
            else validateMappedOrigin(sourceMap, occurrence)
          }
      }

  private def validateBody(
      template: DefinitionTemplate,
      sourceMap: GeneratedSourceMap,
      components: DefinitionComponentSpans,
      body: LocatedTermTemplate
  ): Either[DefinitionError, Unit] =
    val expectedBody =
      template match
        case method: DefinitionTemplate.ParameterlessDef => method.body
        case method: DefinitionTemplate.SingleParameterDef => method.body
        case method: DefinitionTemplate.TwoParameterDef => method.body
        case value: DefinitionTemplate.ImmutableVal => value.rhs
    val occurrences =
      body.termOccurrences.map(_.source) ++ body.typeOccurrences
    if body.template != expectedBody then
      invalid(
        "the located body template must equal the definition template body"
      )
    else if
      occurrences.exists(occurrence =>
        !contains(components.body, occurrence.generatedSpan)
      )
    then
      invalid(
        "every located body occurrence must lie inside the body component"
      )
    else if
      body.termOccurrences.exists(
        _.source.role != HoleRole.DefinitionBodyTermTemplate
      )
    then
      invalid(
        "located body term occurrences must use the definition-body-term role"
      )
    else if
      body.typeOccurrences.exists(
        _.role != HoleRole.DefinitionBodyTypeTemplate
      )
    then
      invalid(
        "located body type occurrences must use the definition-body-type role"
      )
    else Right(())

  private def validateCompleteOccurrencePartition(
      sourceMap: GeneratedSourceMap,
      definitionTypeOccurrences: Vector[HoleOccurrence],
      body: LocatedTermTemplate
  ): Either[DefinitionError, Unit] =
    val actual =
      (
        definitionTypeOccurrences ++
          body.termOccurrences.map(_.source) ++
          body.typeOccurrences
      ).sortBy(_.generatedSpan.start)
    val mapped = sourceMap.segments.filter(isDefinitionHoleOrigin)
    Either.cond(
      actual.size == mapped.size &&
        actual.zip(mapped).forall { case (occurrence, segment) =>
          occurrenceMatchesOrigin(occurrence, segment)
        },
      (),
      DefinitionError.InvalidSourceMetadata(
        "categorized definition occurrences must form one exact nonduplicated partition of raw or surface origins"
      )
    )

  private def isDefinitionHoleOrigin(segment: GeneratedSegment): Boolean =
    segment.origin match
      case _: SourceOrigin.RewrittenHole => true
      case SourceOrigin.InterpolationArgument(_, _, category) =>
        definitionRole(category).nonEmpty
      case _ => false

  private def occurrenceMatchesOrigin(
      occurrence: HoleOccurrence,
      segment: GeneratedSegment
  ): Boolean =
    occurrence.generatedSpan == segment.generatedSpan &&
      (segment.origin match
        case SourceOrigin.RewrittenHole(
              _,
              originalSpan,
              name,
              role
            ) =>
          originalSpan == occurrence.originalSpan &&
            name == occurrence.name &&
            role == occurrence.role
        case SourceOrigin.InterpolationArgument(_, argumentIndex, category) =>
          occurrence.name == s"definitionArgument$argumentIndex" &&
            definitionRole(category).contains(occurrence.role)
        case _ => false)

  private def validateMappedOrigin(
      sourceMap: GeneratedSourceMap,
      occurrence: HoleOccurrence
  ): Either[DefinitionError, Unit] =
    val exact =
      sourceMap.segments.exists { segment =>
        segment.generatedSpan == occurrence.generatedSpan &&
        (segment.origin match
        case SourceOrigin.RewrittenHole(
              _,
              originalSpan,
              name,
              role
            ) =>
          originalSpan == occurrence.originalSpan &&
          name == occurrence.name &&
          role == occurrence.role
        case SourceOrigin.InterpolationArgument(
              _,
              argumentIndex,
              category
            ) =>
          occurrence.name == s"definitionArgument$argumentIndex" &&
            definitionRole(category).contains(occurrence.role)
        case _ =>
          false)
      }
    Either.cond(
      exact,
      (),
      DefinitionError.InvalidSourceMetadata(
        s"the `${occurrence.name}` occurrence must have exact mapped origin evidence"
      )
    )

  private def definitionRole(
      category: InterpolationCategory
  ): Option[HoleRole] =
    category match
      case InterpolationCategory.DefinitionTypeSplice =>
        Some(HoleRole.DefinitionTypeTemplate)
      case InterpolationCategory.DefinitionBodyTermSplice =>
        Some(HoleRole.DefinitionBodyTermTemplate)
      case InterpolationCategory.DefinitionBodyTypeSplice =>
        Some(HoleRole.DefinitionBodyTypeTemplate)
      case _ => None

  private def definitionType(
      template: DefinitionTemplate
  ): TypeTemplate =
    template match
      case method: DefinitionTemplate.ParameterlessDef =>
        method.resultType
      case method: DefinitionTemplate.SingleParameterDef =>
        method.resultType
      case method: DefinitionTemplate.TwoParameterDef =>
        method.resultType
      case value: DefinitionTemplate.ImmutableVal =>
        value.declaredType

  private def contains(
      outer: SourceSpan,
      inner: SourceSpan
  ): Boolean =
    outer.start <= inner.start && inner.end <= outer.end

  private def invalid(
      detail: String
  ): Left[DefinitionError, Nothing] =
    Left(DefinitionError.InvalidSourceMetadata(detail))
