package quasiquotes.types

import scala.quoted.*
import quasiquotes.parser.DiagnosticLocationMapper
import quasiquotes.source.{DiagnosticLocation, DiagnosticPrecision, LocatedDiagnostic, MappedHoleSource}

object QuasiTypeConstruct:
  def fromTemplate(
      templateSource: String,
      bindings: Map[String, TypeNormalForm]
  ): Either[TypeQuasiquoteError, ConstructedType] =
    fromTemplateLocated(templateSource, bindings).left.map(_.diagnostic)

  def fromTemplateLocated(
      templateSource: String,
      bindings: Map[String, TypeNormalForm]
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], ConstructedType] =
    TypeTemplate.fromSourceWithMappingLocated(templateSource).flatMap { parsed =>
      val template = parsed.template
      val mapped = parsed.mappedSource
      rejectExtraBindings(template, bindings)
        .left.map(LocatedDiagnostic(_, None))
        .flatMap { _ =>
          TypeTemplate.construct(template, bindings)
            .left.map { error =>
              LocatedDiagnostic(error, missingBindingLocation(template, bindings, mapped))
            }
            .flatMap { normalForm =>
              TypeTemplate.validateConstructed(normalForm)
                .left.map(error => LocatedDiagnostic(error, DiagnosticLocationMapper.wholeSource(mapped.originMap)))
                .map(_ => ConstructedType(normalForm))
            }
        }
    }

  def fromTemplate(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, ConstructedType] =
    fromTemplate(templateSource, bindings.toMap)

  def fromTemplateLocated(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], ConstructedType] =
    fromTemplateLocated(templateSource, bindings.toMap)

  def toTypeRepr(constructed: ConstructedType)(using Quotes): Either[TypeQuasiquoteError, quotes.reflect.TypeRepr] =
    constructed.toTypeRepr

  private def rejectExtraBindings(template: TypeTemplate, bindings: Map[String, TypeNormalForm]): Either[TypeQuasiquoteError, Unit] =
    val expectedNames = TypeTemplate.holeNames(template)
    val extraNames = bindings.keySet.diff(expectedNames).toList.sorted
    if extraNames.isEmpty then Right(())
    else Left(TypeQuasiquoteError(s"Extra type-construction binding(s): ${extraNames.mkString(", ")}"))

  private def missingBindingLocation(
      template: TypeTemplate,
      bindings: Map[String, TypeNormalForm],
      mapped: MappedHoleSource
  ): Option[DiagnosticLocation] =
    TypeTemplate.firstMissingHole(template, bindings).flatMap { missingName =>
      mapped.occurrences.filter(_.name == missingName) match
        case Vector(occurrence) =>
          DiagnosticLocation.fromGeneratedMap(
            mapped.originMap,
            occurrence.generatedSpan,
            DiagnosticPrecision.ExactOccurrence
          )
        case occurrences if occurrences.nonEmpty =>
          DiagnosticLocationMapper.wholeSource(mapped.originMap)
        case _ =>
          None
    }
