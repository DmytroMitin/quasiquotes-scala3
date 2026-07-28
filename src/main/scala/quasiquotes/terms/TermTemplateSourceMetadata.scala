package quasiquotes.terms

import quasiquotes.source.*
import quasiquotes.types.TypeNormalForm

private[quasiquotes] final case class LocatedTermHoleOccurrence(
    semantic: TermHoleOccurrence,
    source: HoleOccurrence
) derives CanEqual

private[quasiquotes] final class LocatedTermTemplate private (
    val template: TermTemplate,
    val sourceMap: GeneratedSourceMap,
    val termOccurrences: Vector[LocatedTermHoleOccurrence],
    val typeOccurrences: Vector[HoleOccurrence]
) derives CanEqual:
  def complete(
      termBindings: Map[String, ConstructedTerm],
      typeBindings: Map[String, TypeNormalForm]
  ): Either[LocatedDiagnostic[TermConstructionError], ConstructedTerm] =
    template
      .complete(termBindings, typeBindings)
      .left
      .map(error => LocatedDiagnostic(error, locationFor(error)))

  def render: String =
    s"LocatedTermTemplate(template=${template.render}, sourceMetadata=present)"

  override def equals(other: Any): Boolean =
    other match
      case that: LocatedTermTemplate =>
        template == that.template &&
          sourceMap == that.sourceMap &&
          termOccurrences == that.termOccurrences &&
          typeOccurrences == that.typeOccurrences
      case _ =>
        false

  override def hashCode: Int =
    (template, sourceMap, termOccurrences, typeOccurrences).hashCode

  override def toString: String =
    render

  private def locationFor(
      error: TermConstructionError
  ): Option[DiagnosticLocation] =
    semanticName(error)
      .flatMap(uniqueOccurrenceSpan)
      .flatMap(
        DiagnosticLocation.fromGeneratedMap(
          sourceMap,
          _,
          DiagnosticPrecision.ExactOccurrence
        )
      )
      .orElse(wholeSourceLocation)

  private def semanticName(error: TermConstructionError): Option[String] =
    error match
      case TermConstructionError.UnknownTermOccurrence(name, _) => Some(name)
      case TermConstructionError.MissingTermOccurrence(name) => Some(name)
      case TermConstructionError.TermOccurrenceCategoryMismatch(name, _) =>
        Some(name)
      case TermConstructionError.InvalidTermHolePosition(name) => Some(name)
      case TermConstructionError.MissingTermBinding(name) => Some(name)
      case TermConstructionError.MissingTypeBinding(name) => Some(name)
      case TermConstructionError.TypeBindingConstructionFailure(name, _) =>
        Some(name)
      case TermConstructionError.IncompleteBoundTerm(name) => Some(name)
      case _ => None

  private def uniqueOccurrenceSpan(name: String): Option[SourceSpan] =
    val spans =
      termOccurrences.collect {
        case occurrence if occurrence.semantic.name == name =>
          occurrence.source.generatedSpan
      } ++
        typeOccurrences.collect {
          case occurrence if occurrence.name == name =>
            occurrence.generatedSpan
        }
    Option.when(spans.size == 1)(spans.head)

  private def wholeSourceLocation: Option[DiagnosticLocation] =
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

private[quasiquotes] object LocatedTermTemplate:
  def create(
      template: TermTemplate,
      sourceMap: GeneratedSourceMap,
      termOccurrences: Vector[LocatedTermHoleOccurrence],
      typeOccurrences: Vector[HoleOccurrence]
  ): Either[TermConstructionError, LocatedTermTemplate] =
    for
      _ <- validateCoverage(sourceMap)
      _ <- validateTermOccurrences(template, sourceMap, termOccurrences)
      _ <- validateTypeOccurrences(template, sourceMap, typeOccurrences)
    yield new LocatedTermTemplate(
      template,
      sourceMap,
      termOccurrences,
      typeOccurrences
    )

  private def validateCoverage(
      sourceMap: GeneratedSourceMap
  ): Either[TermConstructionError, Unit] =
    val spans = sourceMap.segments.map(_.generatedSpan)
    val covered =
      if sourceMap.generatedSource.isEmpty then spans.isEmpty
      else
        spans.nonEmpty &&
          spans.head.start == 0 &&
          spans.last.end == sourceMap.generatedSource.length &&
          spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          }
    Either.cond(
      covered,
      (),
      TermConstructionError.InvalidLocatedTemplateMetadata(
        "the generated source map must cover the complete generated source"
      )
    )

  private def validateTermOccurrences(
      template: TermTemplate,
      sourceMap: GeneratedSourceMap,
      occurrences: Vector[LocatedTermHoleOccurrence]
  ): Either[TermConstructionError, Unit] =
    if occurrences.map(_.semantic) != template.termHoleOccurrences then
      invalid(
        "located term occurrences must match semantic occurrence order and addresses"
      )
    else
      occurrences.foldLeft[Either[TermConstructionError, Unit]](Right(())) {
        (result, occurrence) =>
          result.flatMap { _ =>
            val expectedGenerated =
              template.termHoleIndex.generatedNameFor(occurrence.semantic.name)
            if occurrence.source.name != occurrence.semantic.name then
              invalid("located term occurrence semantic names must agree")
            else if !expectedGenerated.contains(occurrence.source.generatedName)
            then
              invalid(
                "located term occurrence generated names must agree with the term-hole index"
              )
            else if occurrence.source.role != HoleRole.TermTemplate then
              invalid("located term occurrences must use the term-template role")
            else
              validateMappedOrigin(
                sourceMap,
                occurrence.source,
                HoleRole.TermTemplate
              )
          }
      }

  private def validateTypeOccurrences(
      template: TermTemplate,
      sourceMap: GeneratedSourceMap,
      occurrences: Vector[HoleOccurrence]
  ): Either[TermConstructionError, Unit] =
    val expectedNames =
      template.ascriptionTypes.flatMap(
        TermShapeTraversal.typeHoleOccurrences
      )
    if occurrences.map(_.name) != expectedNames then
      invalid(
        "located type occurrences must match typed-sidecar preorder and occurrence counts"
      )
    else
      occurrences.foldLeft[Either[TermConstructionError, Unit]](Right(())) {
        (result, occurrence) =>
          result.flatMap { _ =>
            val expectedGenerated =
              template.typeHoleIndex.generatedNameFor(occurrence.name)
            if !expectedGenerated.contains(occurrence.generatedName) then
              invalid(
                "located type occurrence generated names must agree with the type-hole index"
              )
            else if occurrence.role != HoleRole.TypeTemplate then
              invalid("located type occurrences must use the type-template role")
            else
              validateMappedOrigin(
                sourceMap,
                occurrence,
                HoleRole.TypeTemplate
              )
          }
      }

  private def validateMappedOrigin(
      sourceMap: GeneratedSourceMap,
      occurrence: HoleOccurrence,
      expectedRole: HoleRole
  ): Either[TermConstructionError, Unit] =
    val origins =
      sourceMap.originsFor(occurrence.generatedSpan).map(_.origin)
    val exact =
      origins.exists {
        case SourceOrigin.RewrittenHole(
              _,
              originalSpan,
              name,
              role
            ) =>
          originalSpan == occurrence.originalSpan &&
          name == occurrence.name &&
          role == expectedRole
        case _ =>
          false
      }
    Either.cond(
      !occurrence.generatedSpan.isEmpty &&
        occurrence.generatedSpan.end <= sourceMap.generatedSource.length &&
        exact,
      (),
      TermConstructionError.InvalidLocatedTemplateMetadata(
        s"the `${occurrence.name}` occurrence must have exact mapped origin evidence"
      )
    )

  private def invalid(
      detail: String
  ): Left[TermConstructionError, Nothing] =
    Left(TermConstructionError.InvalidLocatedTemplateMetadata(detail))
