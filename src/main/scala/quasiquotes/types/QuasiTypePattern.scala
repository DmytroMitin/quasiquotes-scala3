package quasiquotes.types

import scala.quoted.*
import quasiquotes.parser.TinyTypeParser

final case class QuasiTypePattern(
    source: String,
    expected: QuasiTypeRepr,
    expectedNormalForm: TypeNormalForm
):
  def matchingSubstrateSummary: String =
    "source=TypeNormalForm targetTypeRepr=exact-rendered-TypeRepr"

  def matchTypeRepr(using q: Quotes)(target: q.reflect.TypeRepr): Boolean =
    import q.reflect.*
    target.show == expected.renderedTypeRepr

  def matchShape(targetShape: quasiquotes.parser.TypeShape)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    for
      _ <- TypeReprLowerer.lower(targetShape)
      targetNormalForm <- TypeNormalForm.fromShape(targetShape)
    yield targetNormalForm == expectedNormalForm

object QuasiTypePattern:
  def repr(source: String)(using Quotes): Either[TypeQuasiquoteError, QuasiTypePattern] =
    for
      expected <- QuasiTypeRepr.fromSource(source)
      expectedNormalForm <- TypeNormalForm.fromShape(expected.shape)
    yield QuasiTypePattern(source, expected, expectedNormalForm)

  def reprOrThrow(source: String)(using Quotes): QuasiTypePattern =
    repr(source).fold(throw _, identity)

  def matchesSource(expectedSource: String, actualSource: String)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    for
      pattern <- repr(expectedSource)
      targetShape <- TinyTypeParser.parse(actualSource).left.map(error => TypeQuasiquoteError(error.summary)).map(_.shape)
      matched <- pattern.matchShape(targetShape)
    yield matched

  def matchesSourceByRenderedTypeRepr(expectedSource: String, actualSource: String)(using Quotes): Either[TypeQuasiquoteError, Boolean] =
    for
      pattern <- repr(expectedSource)
      targetShape <- TinyTypeParser.parse(actualSource).left.map(error => TypeQuasiquoteError(error.summary)).map(_.shape)
      targetRepr <- TypeReprLowerer.lower(targetShape)
    yield pattern.matchTypeRepr(targetRepr)
