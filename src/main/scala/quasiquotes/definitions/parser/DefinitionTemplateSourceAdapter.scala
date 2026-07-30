package quasiquotes.definitions.parser

import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.{
  RawDefinitionAdapterError,
  RawDefinitionEnvelope,
  RawDefinitionParser,
  RawDefinitionVariant
}
import quasiquotes.parser.{
  TermShapeInspector,
  TypeShape,
  TypeShapeInspector
}
import quasiquotes.source.*
import quasiquotes.terms.parser.{
  RawTermTemplateAdapter,
  TermTemplateSourceAdapterError
}
import quasiquotes.types.TypeTemplate

private[quasiquotes] object DefinitionTemplateSourceAdapter:
  import DefinitionTemplateHoleCategory.*
  import DefinitionTemplateSourceAdapterError.*

  private val DefinitionTypePrefix = "__qq_dt_type_"
  private val BodyTermPrefix = "__qq_dt_body_term_"
  private val BodyTypePrefix = "__qq_dt_body_type_"

  def parse(
      source: String,
      occurrences: Vector[CategorizedDefinitionHoleOccurrence]
  ): Either[DefinitionTemplateSourceAdapterError, DefinitionTemplate] =
    parseLocated(source, occurrences).left.map(_.diagnostic).map(_.template)

  def parseLocated(
      source: String,
      occurrences: Vector[CategorizedDefinitionHoleOccurrence]
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    LocatedDefinitionTemplate
  ] =
    parseLocatedUsing(source, occurrences)(
      RawDefinitionParser.parseEnvelopeStandalone
    )

  private[parser] def parseLocatedUsing(
      source: String,
      occurrences: Vector[CategorizedDefinitionHoleOccurrence]
  )(
      parseGenerated: (
          String,
          SourceId
      ) => Either[
        LocatedDiagnostic[RawDefinitionAdapterError],
        RawDefinitionEnvelope
      ]
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    LocatedDefinitionTemplate
  ] =
    val scan =
      HoleSourceRewriter.scan(source, allowUnicodeIdentifiers = false)

    validatePlan(source, scan, occurrences).flatMap { roles =>
      val mapped =
        HoleSourceRewriter.rewriteScannedCategorized(
          source,
          scan,
          roles,
          {
            case HoleRole.DefinitionTypeTemplate =>
              DefinitionTypePrefix
            case HoleRole.DefinitionBodyTermTemplate =>
              BodyTermPrefix
            case HoleRole.DefinitionBodyTypeTemplate =>
              BodyTypePrefix
            case other =>
              s"__qq_dt_${other.toString.toLowerCase}_"
          },
          SourceId.DefinitionConstructionTemplate,
          SourceId.VirtualDefinitionTemplateParserInput
        )

      validateSourceMap(mapped).flatMap { _ =>
        parseGenerated(
          mapped.generatedSource,
          mapped.originMap.generatedSourceId
        ).left
          .map(mapParserFailure(_, mapped))
          .flatMap(adaptEnvelope(scan, mapped, _))
      }
    }

  private def adaptEnvelope(
      scan: HoleSourceRewriter.SourceScan,
      mapped: CategorizedMappedHoleSource,
      envelope: RawDefinitionEnvelope
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    LocatedDefinitionTemplate
  ] =
    given Context = envelope.context
    for
      _ <- validateCategories(mapped, envelope.components)
      definitionType <- adaptDefinitionType(scan, mapped, envelope)
      body <- RawTermTemplateAdapter
        .adapt(
          scan,
          mapped,
          envelope.body,
          TermShapeInspector.inspect(envelope.body),
          HoleRole.DefinitionBodyTermTemplate,
          HoleRole.DefinitionBodyTypeTemplate,
          envelope.components.body,
          Vector(
            DefinitionTypePrefix,
            BodyTermPrefix,
            BodyTypePrefix
          )
        )
        .left
        .map(mapBodyFailure(_, mapped))
      template <- createTemplate(
        envelope,
        definitionType,
        body.template,
        mapped.originMap
      )
      located <- LocatedDefinitionTemplate
        .create(
          template,
          mapped.originMap.generatedSourceId,
          mapped.originMap,
          envelope.components,
          mapped.occurrences.filter(
            _.role == HoleRole.DefinitionTypeTemplate
          ),
          body
        )
        .left
        .map(error =>
          LocatedDiagnostic(
            InvalidLocatedDefinitionTemplate(error.message),
            wholeLocation(mapped.originMap)
          )
        )
    yield located

  private def adaptDefinitionType(
      scan: HoleSourceRewriter.SourceScan,
      mapped: CategorizedMappedHoleSource,
      envelope: RawDefinitionEnvelope
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    TypeTemplate
  ] =
    val shape = TypeShapeInspector.inspect(envelope.definitionType)
    val occurrences =
      mapped.occurrences.filter(
        _.role == HoleRole.DefinitionTypeTemplate
      )
    val ownedGeneratedNames =
      mapped.occurrences.map(_.generatedName).toSet
    firstUnknownGeneratedMarker(
      shape,
      scan.literalIdentifiers,
      ownedGeneratedNames
    ) match
      case Some(name) =>
        Left(
          LocatedDiagnostic(
            UnknownGeneratedMarker(name),
            componentLocation(
              mapped.originMap,
              envelope.components.declaredType
            )
          )
        )
      case None =>
        adaptKnownDefinitionType(
          shape,
          occurrences,
          mapped,
          envelope
        )

  private def adaptKnownDefinitionType(
      shape: TypeShape,
      occurrences: Vector[HoleOccurrence],
      mapped: CategorizedMappedHoleSource,
      envelope: RawDefinitionEnvelope
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    TypeTemplate
  ] =
    TypeTemplate
      .fromShapeWithHoles(
        shape,
        mapped.generatedHoleIndex(HoleRole.DefinitionTypeTemplate)
      )
      .left
      .map(error =>
        LocatedDiagnostic(
          InvalidDefinitionTypeTemplate(
            restore(error.message, mapped)
          ),
          componentLocation(
            mapped.originMap,
            envelope.components.declaredType
          )
        )
      )
      .flatMap { template =>
        val expected = TypeTemplate.holeOccurrences(template)
        Either.cond(
          occurrences.map(_.name) == expected,
          template,
          LocatedDiagnostic(
            InvalidDefinitionTypeTemplate(
              s"expected ${expected.mkString("[", ", ", "]")} but extracted ${occurrences.map(_.name).mkString("[", ", ", "]")}"
            ),
            componentLocation(
              mapped.originMap,
              envelope.components.declaredType
            )
          )
        )
      }

  private def firstUnknownGeneratedMarker(
      shape: TypeShape,
      literalIdentifiers: Set[String],
      ownedGeneratedNames: Set[String]
  ): Option[String] =
    typeIdentifiers(shape).find(name =>
      Vector(
        DefinitionTypePrefix,
        BodyTermPrefix,
        BodyTypePrefix
      ).exists(name.startsWith) &&
        !literalIdentifiers(name) &&
        !ownedGeneratedNames(name)
    )

  private def typeIdentifiers(shape: TypeShape): Vector[String] =
    shape match
      case TypeShape.Identifier(name) =>
        Vector(name)
      case TypeShape.Select(qualifier, name) =>
        typeIdentifiers(qualifier) :+ name
      case TypeShape.Apply(constructor, arguments) =>
        typeIdentifiers(constructor) ++
          arguments.toVector.flatMap(typeIdentifiers)
      case TypeShape.Tuple(elements) =>
        elements.toVector.flatMap(typeIdentifiers)
      case TypeShape.Function(arguments, result) =>
        arguments.toVector.flatMap(typeIdentifiers) ++
          typeIdentifiers(result)
      case TypeShape.Parenthesized(inner) =>
        typeIdentifiers(inner)
      case TypeShape.Unsupported(_, _) =>
        Vector.empty

  private def createTemplate(
      envelope: RawDefinitionEnvelope,
      definitionType: TypeTemplate,
      body: quasiquotes.terms.TermTemplate,
      sourceMap: GeneratedSourceMap
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    DefinitionTemplate
  ] =
    val result =
      envelope.variant match
        case RawDefinitionVariant.ParameterlessDef =>
          DefinitionTemplate.parameterlessDef(
            envelope.name,
            definitionType,
            body
          )
        case RawDefinitionVariant.ImmutableVal =>
          DefinitionTemplate.immutableVal(
            envelope.name,
            definitionType,
            body
          )
    result.left.map(error =>
      LocatedDiagnostic(
        DefinitionTemplateFactoryFailure(error.message),
        wholeLocation(sourceMap)
      )
    )

  private def validateCategories(
      mapped: CategorizedMappedHoleSource,
      components: DefinitionComponentSpans
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    Unit
  ] =
    mapped.occurrences.zipWithIndex
      .collectFirst {
        case (occurrence, index)
            if !categoryContains(occurrence, components) =>
          val detail =
            occurrence.role match
              case HoleRole.DefinitionTypeTemplate =>
                "DefinitionType occurrences must lie inside the explicit definition type"
              case HoleRole.DefinitionBodyTermTemplate =>
                "BodyTerm occurrences must lie inside the definition body"
              case HoleRole.DefinitionBodyTypeTemplate =>
                "BodyType occurrences must lie inside the definition body"
              case _ =>
                "the occurrence has an unsupported definition-template role"
          Left(
            LocatedDiagnostic(
              CategoryComponentMismatch(
                index,
                occurrence.name,
                categoryFor(occurrence.role),
                detail
              ),
              exactLocation(mapped.originMap, occurrence.generatedSpan)
            )
          )
      }
      .getOrElse(Right(()))

  private def categoryContains(
      occurrence: HoleOccurrence,
      components: DefinitionComponentSpans
  ): Boolean =
    occurrence.role match
      case HoleRole.DefinitionTypeTemplate =>
        contains(components.declaredType, occurrence.generatedSpan)
      case HoleRole.DefinitionBodyTermTemplate |
          HoleRole.DefinitionBodyTypeTemplate =>
        contains(components.body, occurrence.generatedSpan)
      case _ =>
        false

  private def categoryFor(
      role: HoleRole
  ): DefinitionTemplateHoleCategory =
    role match
      case HoleRole.DefinitionTypeTemplate => DefinitionType
      case HoleRole.DefinitionBodyTermTemplate => BodyTerm
      case HoleRole.DefinitionBodyTypeTemplate => BodyType
      case _ => BodyTerm

  private def validatePlan(
      source: String,
      scan: HoleSourceRewriter.SourceScan,
      occurrences: Vector[CategorizedDefinitionHoleOccurrence]
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    Vector[HoleRole]
  ] =
    occurrences.zipWithIndex
      .find { case (occurrence, _) =>
        !isValidHoleName(occurrence.name)
      }
      .map { case (occurrence, index) =>
        Left(
          LocatedDiagnostic(
            InvalidHoleName(index, occurrence.name),
            wholeOriginalLocation(source)
          )
        )
      }
      .orElse(
        scan.invalidDollarSpans.headOption.map { span =>
          Left(
            LocatedDiagnostic(
              InvalidDollarSyntax(source.slice(span.start, span.end)),
              DiagnosticLocation.direct(
                SourceId.DefinitionConstructionTemplate,
                span,
                DiagnosticPrecision.ExactOccurrence
              )
            )
          )
        }
      )
      .orElse(
        Option.when(scan.holes.size != occurrences.size) {
          val location =
            scan.holes
              .lift(occurrences.size)
              .flatMap(hole =>
                DiagnosticLocation.direct(
                  SourceId.DefinitionConstructionTemplate,
                  SourceSpan(hole.start, hole.end),
                  DiagnosticPrecision.ExactOccurrence
                )
              )
              .orElse(wholeOriginalLocation(source))
          Left(
            LocatedDiagnostic(
              OccurrenceCountMismatch(
                expected = scan.holes.size,
                actual = occurrences.size
              ),
              location
            )
          )
        }
      )
      .orElse(
        scan.holes
          .zip(occurrences)
          .zipWithIndex
          .collectFirst {
            case ((hole, occurrence), index)
                if hole.name != occurrence.name =>
              Left(
                LocatedDiagnostic(
                  OccurrenceNameMismatch(
                    index,
                    hole.name,
                    occurrence.name
                  ),
                  DiagnosticLocation.direct(
                    SourceId.DefinitionConstructionTemplate,
                    SourceSpan(hole.start, hole.end),
                    DiagnosticPrecision.ExactOccurrence
                  )
                )
              )
          }
      )
      .getOrElse {
        Right(
          occurrences.map {
            case CategorizedDefinitionHoleOccurrence(
                  _,
                  DefinitionType
                ) =>
              HoleRole.DefinitionTypeTemplate
            case CategorizedDefinitionHoleOccurrence(_, BodyTerm) =>
              HoleRole.DefinitionBodyTermTemplate
            case CategorizedDefinitionHoleOccurrence(_, BodyType) =>
              HoleRole.DefinitionBodyTypeTemplate
          }
        )
      }

  private def validateSourceMap(
      mapped: CategorizedMappedHoleSource
  ): Either[
    LocatedDiagnostic[DefinitionTemplateSourceAdapterError],
    Unit
  ] =
    val spans = mapped.originMap.segments.map(_.generatedSpan)
    val complete =
      if mapped.generatedSource.isEmpty then spans.isEmpty
      else
        spans.nonEmpty &&
          spans.head.start == 0 &&
          spans.last.end == mapped.generatedSource.length &&
          spans.zip(spans.drop(1)).forall { case (left, right) =>
            left.end == right.start
          }
    Either.cond(
      complete,
      (),
      LocatedDiagnostic(
        IncompleteDefinitionSourceMap(
          "the generated source map does not cover the complete source"
        ),
        wholeLocation(mapped.originMap)
      )
    )

  private def mapParserFailure(
      failure: LocatedDiagnostic[RawDefinitionAdapterError],
      mapped: CategorizedMappedHoleSource
  ): LocatedDiagnostic[DefinitionTemplateSourceAdapterError] =
    val error =
      failure.diagnostic match
        case RawDefinitionAdapterError.DefinitionParseFailure =>
          DefinitionParserFailure(failure.diagnostic.message)
        case RawDefinitionAdapterError.InvalidDefinitionName(_) =>
          InvalidDefinitionName(failure.diagnostic.message)
        case other =>
          UnsupportedDefinitionVariant(other.message)
    LocatedDiagnostic(
      error,
      failure.location
        .flatMap(location =>
          DiagnosticLocation.fromGeneratedMap(
            mapped.originMap,
            location.span,
            location.precision
          )
        )
        .orElse(wholeLocation(mapped.originMap))
    )

  private def mapBodyFailure(
      failure: LocatedDiagnostic[TermTemplateSourceAdapterError],
      mapped: CategorizedMappedHoleSource
  ): LocatedDiagnostic[DefinitionTemplateSourceAdapterError] =
    val error =
      failure.diagnostic match
        case TermTemplateSourceAdapterError.UnknownGeneratedMarker(name) =>
          UnknownGeneratedMarker(name)
        case other =>
          InvalidDefinitionBodyTemplate(
            restore(other.message, mapped)
          )
    LocatedDiagnostic(error, failure.location)

  private def restore(
      detail: String,
      mapped: CategorizedMappedHoleSource
  ): String =
    HoleSourceRewriter.restoreSemanticHoleIdentifiers(
      detail,
      mapped,
      allowUnicodeIdentifiers = false
    )

  private def exactLocation(
      sourceMap: GeneratedSourceMap,
      span: SourceSpan
  ): Option[DiagnosticLocation] =
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      span,
      DiagnosticPrecision.ExactOccurrence
    )

  private def componentLocation(
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
    DiagnosticLocation.fromGeneratedMap(
      sourceMap,
      SourceSpan(0, sourceMap.generatedSource.length),
      DiagnosticPrecision.WholeSource
    )

  private def wholeOriginalLocation(
      source: String
  ): Option[DiagnosticLocation] =
    Option
      .when(source.nonEmpty)(SourceSpan(0, source.length))
      .flatMap(
        DiagnosticLocation.direct(
          SourceId.DefinitionConstructionTemplate,
          _,
          DiagnosticPrecision.WholeSource
        )
      )

  private def contains(
      outer: SourceSpan,
      inner: SourceSpan
  ): Boolean =
    outer.start <= inner.start && inner.end <= outer.end

  private def isValidHoleName(name: String): Boolean =
    name.nonEmpty &&
      isIdentifierStart(name.head) &&
      name.tail.forall(isIdentifierPart)

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' ||
      ('A' <= char && char <= 'Z') ||
      ('a' <= char && char <= 'z')

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || ('0' <= char && char <= '9')
