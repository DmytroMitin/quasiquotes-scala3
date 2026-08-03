package quasiquotes.definitions.parser

import quasiquotes.definitions.*
import quasiquotes.source.*

private[quasiquotes] object DefinitionInterpolationSourceAssembler:
  import DefinitionTemplateHoleCategory.*
  import DefinitionTemplateSourceAdapterError.*

  def construct(
      assembly: DefinitionQuasiquoteAssembly
  ): Either[
    LocatedDiagnostic[DefinitionQuasiquoteError],
    DefinitionQuasiquoteResult
  ] =
    constructUsing(assembly)(
      (source, occurrences, initialMap) =>
        DefinitionTemplateSourceAdapter.parseLocatedMapped(
          source,
          occurrences,
          initialMap
        ),
      (located, termBindings, typeBindings) =>
        located.complete(termBindings, typeBindings)
    )

  private[parser] def constructUsing(
      assembly: DefinitionQuasiquoteAssembly
  )(
      parseLocated: (
          String,
          Vector[CategorizedDefinitionHoleOccurrence],
          GeneratedSourceMap
      ) => Either[
        LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
        LocatedDefinitionTemplate
      ],
      completeLocated: (
          LocatedDefinitionTemplate,
          Map[String, quasiquotes.terms.ConstructedTerm],
          Map[String, quasiquotes.types.TypeNormalForm]
      ) => Either[
        LocatedDiagnostic[DefinitionConstructionError],
        ConstructedDefinition
      ]
  ): Either[
    LocatedDiagnostic[DefinitionQuasiquoteError],
    DefinitionQuasiquoteResult
  ] =
    val plan = assembly.occurrences.map(occurrence =>
      CategorizedDefinitionHoleOccurrence(
        occurrence.semanticIdentity,
        phase50Category(occurrence.category)
      )
    )
    parseLocated(assembly.source, plan, assembly.sourceMap)
      .left
      .map(mapFrontendFailure(_, assembly))
      .flatMap { located =>
        completeLocated(
          located,
          assembly.termBindings,
          assembly.typeBindings
        ).left
          .map(mapCompletionFailure(_, assembly))
          .flatMap { constructed =>
            val locatedOccurrences =
              (
                located.definitionTypeOccurrences ++
                  located.body.termOccurrences.map(_.source) ++
                  located.body.typeOccurrences
              ).sortBy(_.generatedSpan.start)
            DefinitionQuasiquoteSourceEvidence
              .create(
                located.sourceId,
                located.sourceMap,
                located.components,
                assembly.occurrences,
                locatedOccurrences
              )
              .left
              .map(error =>
                LocatedDiagnostic(
                  error,
                  wholeLocation(located.sourceMap, located.components.definition)
                )
              )
              .map(evidence =>
                new DefinitionQuasiquoteResult(constructed, evidence)
              )
          }
      }

  private def phase50Category(
      category: InterpolationCategory
  ): DefinitionTemplateHoleCategory =
    category match
      case InterpolationCategory.DefinitionTypeSplice => DefinitionType
      case InterpolationCategory.DefinitionBodyTermSplice => BodyTerm
      case InterpolationCategory.DefinitionBodyTypeSplice => BodyType
      case other =>
        throw new IllegalArgumentException(
          s"Unsupported definition interpolation category: $other"
        )

  private def mapFrontendFailure(
      failure: LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
      assembly: DefinitionQuasiquoteAssembly
  ): LocatedDiagnostic[DefinitionQuasiquoteError] =
    val occurrence = occurrenceFrom(failure.location, assembly)
    val kind =
      failure.diagnostic match
        case _: DefinitionParserFailure => "parsing"
        case _: InvalidDefinitionName => "fixed-name validation"
        case _: InvalidDefinitionTypeTemplate => "definition-type adaptation"
        case _: InvalidDefinitionBodyTemplate => "body adaptation"
        case _: CategoryComponentMismatch => "descriptor-role validation"
        case _: InvalidLocatedDefinitionTemplate => "source-evidence validation"
        case _ => "frontend adaptation"
    val detail =
      failure.diagnostic match
        case _: UnknownGeneratedMarker =>
          "an unowned generated marker was rejected"
        case other => sanitize(other.message, assembly)
    LocatedDiagnostic(
      DefinitionQuasiquoteError.FrontendFailure(
        kind,
        detail,
        occurrence.map(_.argumentIndex),
        occurrence.map(item => DefinitionQuasiquoteAssembly.roleLabel(item.category))
      ),
      failure.location
    )

  private def mapCompletionFailure(
      failure: LocatedDiagnostic[DefinitionConstructionError],
      assembly: DefinitionQuasiquoteAssembly
  ): LocatedDiagnostic[DefinitionQuasiquoteError] =
    val identity = bindingIdentity(failure.diagnostic)
    val occurrence = identity.flatMap(name =>
      val matches = assembly.occurrences.filter(_.semanticIdentity == name)
      Option.when(matches.size == 1)(matches.head)
    )
    LocatedDiagnostic(
      DefinitionQuasiquoteError.CompletionFailure(
        failure.diagnostic,
        sanitizeCompletion(failure.diagnostic.message, occurrence),
        occurrence.map(_.argumentIndex),
        occurrence.map(item => DefinitionQuasiquoteAssembly.roleLabel(item.category))
      ),
      failure.location
    )

  private def bindingIdentity(
      error: DefinitionConstructionError
  ): Option[String] =
    error match
      case DefinitionConstructionError.MissingTermBinding(name) => Some(name)
      case DefinitionConstructionError.MissingTypeBinding(name) => Some(name)
      case DefinitionConstructionError.InvalidTypeBinding(name, _) => Some(name)
      case _ => None

  private def occurrenceFrom(
      location: Option[DiagnosticLocation],
      assembly: DefinitionQuasiquoteAssembly
  ): Option[DefinitionQuasiquoteAssemblyOccurrence] =
    val indices = location.toVector.flatMap(_.origins).collect {
      case SourceOrigin.InterpolationArgument(_, index, _) => index
    }.distinct
    Option
      .when(indices.size == 1)(indices.head)
      .flatMap(index => assembly.occurrences.lift(index))

  private def sanitize(
      message: String,
      assembly: DefinitionQuasiquoteAssembly
  ): String =
    val identitiesRestored = assembly.occurrences.foldLeft(message) {
      (current, occurrence) =>
        current
          .replace(
            s"$$${occurrence.semanticIdentity}",
            s"definition interpolation argument ${occurrence.argumentIndex}"
          )
          .replace(
            occurrence.semanticIdentity,
            s"definition interpolation argument ${occurrence.argumentIndex}"
          )
    }
    "__qq_dt_[A-Za-z0-9_]+".r.replaceAllIn(
      identitiesRestored,
      "generated definition marker"
    )

  /**
   * Completion attribution is resolved before presentation from structured
   * error fields only. A non-name-bearing detail is preserved as literal
   * presentation text while known generated transport prefixes are masked.
   */
  private def sanitizeCompletion(
      message: String,
      occurrence: Option[DefinitionQuasiquoteAssemblyOccurrence]
  ): String =
    val identitiesPresented = occurrence.fold(message) { resolved =>
      val replacement =
        s"definition interpolation argument ${resolved.argumentIndex}"
      message
        .replace(s"$$${resolved.semanticIdentity}", replacement)
        .replace(resolved.semanticIdentity, replacement)
    }
    "__qq_dt_[A-Za-z0-9_]+".r.replaceAllIn(
      identitiesPresented,
      "generated definition marker"
    )

  private def wholeLocation(
      sourceMap: GeneratedSourceMap,
      span: SourceSpan
  ): Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      span,
      DiagnosticPrecision.WholeSource
    )
