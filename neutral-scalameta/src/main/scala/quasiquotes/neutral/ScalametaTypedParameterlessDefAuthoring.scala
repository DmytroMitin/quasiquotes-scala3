package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for one reusable explicitly typed parameterless method shape. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedParameterlessDefAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      shape: DefinitionShape.ParameterlessDef
  ): Either[Error, Defn.Def] =
    Option(shape)
      .toRight(
        error(
          "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_MISSING",
          "the parameterless def shape must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(
      shape: DefinitionShape.ParameterlessDef
  ): Either[Error, Defn.Def] =
    for
      validated <- DefinitionShape
        .parameterlessDef(shape.name, shape.resultType, shape.body)
        .left
        .map(_ => shapeUnsupported)
      normalForm <- TypeNormalForm
        .fromShape(validated.resultType)
        .left
        .map(_ => typeUnsupported)
      authoredType <- ScalametaTypeNormalFormAuthoring
        .author(normalForm)
        .left
        .map(_ => typeUnsupported)
      authoredBody <- ScalametaTermShapeAuthoring
        .author(validated.body)
        .left
        .map(_ => termUnsupported)
      authoredName <- ScalametaTermDefinitionNameAuthoring
        .author(validated.name)
        .toRight(nameUnsupported)
      authored <- construct(authoredName, authoredType, authoredBody)
      _ <- requireExactRoundTrip(authored, shape)
    yield authored

  private def construct(
      name: Term.Name,
      resultType: Type,
      body: Term
  ): Either[Error, Defn.Def] =
    try Right(Defn.Def(Nil, name, Nil, Some(resultType), body))
    catch case NonFatal(_) => Left(roundTripFailed)

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: DefinitionShape.ParameterlessDef
  ): Either[Error, Unit] =
    ScalametaTypedParameterlessDefProjection.project(authored) match
      case Right(ProjectedDefinitionShape(projected, None)) if projected == expected => Right(())
      case _ => Left(roundTripFailed)

  private def shapeUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_SHAPE_UNSUPPORTED",
      "Core DefinitionShape rejected the parameterless def input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_TYPE_UNSUPPORTED",
      "the result Type is outside the existing unresolved Type normal-form authoring family."
    )

  private def termUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_TERM_UNSUPPORTED",
      "the method body is outside the current generic TermShape authoring family."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_NAME_UNSUPPORTED",
      "the method name cannot be authored as a fresh Term.Name with exact Core spelling."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_ROUNDTRIP_FAILED",
      "the authored parameterless def did not round-trip through the accepted N021 projector exactly."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
