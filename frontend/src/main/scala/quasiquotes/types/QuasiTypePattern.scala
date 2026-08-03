package quasiquotes.types

import scala.quoted.*
import quasiquotes.parser.{DiagnosticLocationMapper, TinyTypeParser}
import quasiquotes.source.{LocatedDiagnostic, MappedHoleSource}

final case class QuasiTypePattern(
    source: String,
    expected: Option[QuasiTypeRepr],
    expectedNormalForm: Option[TypeNormalForm],
    typePattern: TypePattern
):
  def matchingSubstrateSummary: String =
    "source=TypeNormalForm targetTypeRepr=TypeNormalForm exact-rendered-TypeRepr=debug"

  def matchTypeRepr(using q: Quotes)(target: q.reflect.TypeRepr): Boolean =
    matchTypeReprResult(target).isDefined

  def matchTypeReprResult(using q: Quotes)(target: q.reflect.TypeRepr): Option[TypeMatchResult] =
    TargetTypeReprInspector.inspect(target).toOption.flatMap(targetNormalForm => TypePattern.matchNormalForm(typePattern, targetNormalForm))

  def exactRenderedTypeReprMatches(using q: Quotes)(target: q.reflect.TypeRepr): Boolean =
    import q.reflect.*
    expected.exists(expectedType => target.show == expectedType.renderedTypeRepr)

  def matchShape(targetShape: quasiquotes.parser.TypeShape)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    matchShapeResult(targetShape).map(_.isDefined)

  def matchShapeResult(targetShape: quasiquotes.parser.TypeShape)(using Quotes): Either[TypeQuasiquoteError, Option[TypeMatchResult]] =
    for
      _ <- TypeReprLowerer.lower(targetShape)
      targetNormalForm <- TypeNormalForm.fromShape(targetShape)
    yield TypePattern.matchNormalForm(typePattern, targetNormalForm)

  def matchSource(targetSource: String)(using Quotes): Either[TypeQuasiquoteError, Option[TypeMatchResult]] =
    for
      targetShape <- TinyTypeParser.parse(targetSource).left.map(error => TypeQuasiquoteError(error.summary)).map(_.shape)
      result <- matchShapeResult(targetShape)
    yield result

object QuasiTypePattern:
  /** Canonical explicit constructor for a supported type pattern. */
  def pattern(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    patternLocated(source).left.map(_.diagnostic)

  def patternLocated(source: String)(using Quotes): Either[LocatedDiagnostic[TypeQuasiquoteError], QuasiTypePattern] =
    TypePatternSource.fromSourceWithMappingLocated(source).flatMap { parsed =>
      val typePattern = parsed.pattern
      val mapped = parsed.mappedSource
      expectedTypeRepr(source, parsed)
        .left.map(locatedWhole(_, mapped))
        .flatMap { expected =>
          val expectedNormalForm = expected match
            case Some(expectedType) => TypeNormalForm.fromShape(expectedType.shape).map(Some(_))
            case None => Right(None)
          expectedNormalForm
            .left.map(locatedWhole(_, mapped))
            .map(QuasiTypePattern(source, expected, _, typePattern))
        }
    }

  /** Compatibility alias retained for the original research API. */
  def repr(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    pattern(source)

  def reprOrThrow(source: String)(using Quotes): QuasiTypePattern =
    repr(source).fold(throw _, identity)

  def matchesSource(expectedSource: String, actualSource: String)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    for
      pattern <- repr(expectedSource)
      result <- pattern.matchSource(actualSource)
    yield result.isDefined

  def matchesSourceByRenderedTypeRepr(expectedSource: String, actualSource: String)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    for
      pattern <- repr(expectedSource)
      targetShape <- TinyTypeParser.parse(actualSource).left.map(error => TypeQuasiquoteError(error.summary)).map(_.shape)
      targetRepr <- TypeReprLowerer.lower(targetShape)
    yield pattern.exactRenderedTypeReprMatches(targetRepr)

  private def expectedTypeRepr(
      source: String,
      parsed: MappedTypePattern
  )(using Quotes): Either[TypeQuasiquoteError, Option[QuasiTypeRepr]] =
    if parsed.pattern.containsHole then Right(None)
    else QuasiTypeRepr.fromShape(source, parsed.parsedType.shape).map(Some(_))

  private def locatedWhole(
      error: TypeQuasiquoteError,
      mapped: MappedHoleSource
  ): LocatedDiagnostic[TypeQuasiquoteError] =
    LocatedDiagnostic(error, DiagnosticLocationMapper.wholeSource(mapped.originMap))
