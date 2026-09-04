package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for one reusable explicitly typed immutable val shape. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedImmutableValAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      shape: DefinitionShape.ImmutableVal
  ): Either[Error, Defn.Val] =
    Option(shape)
      .toRight(
        error(
          "NEUTRAL_TYPED_VAL_AUTHORING_MISSING",
          "the immutable val shape must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(
      shape: DefinitionShape.ImmutableVal
  ): Either[Error, Defn.Val] =
    for
      validated <- DefinitionShape
        .immutableVal(shape.name, shape.declaredType, shape.rhs)
        .left
        .map(_ => shapeUnsupported)
      normalForm <- TypeNormalForm
        .fromShape(validated.declaredType)
        .left
        .map(_ => typeUnsupported)
      authoredType <- ScalametaTypeNormalFormAuthoring
        .author(normalForm)
        .left
        .map(_ => typeUnsupported)
      authoredRhs <- ScalametaTermShapeAuthoring
        .author(validated.rhs)
        .left
        .map(_ => termUnsupported)
      authoredName <- authorName(validated.name)
      authored <- construct(authoredName, authoredType, authoredRhs)
      _ <- requireExactRoundTrip(authored, shape)
    yield authored

  private def authorName(name: DefinitionName): Either[Error, Term.Name] =
    Option(name)
      .flatMap(value => Option(value.decoded).map(value -> _))
      .toRight(nameUnsupported)
      .flatMap { (expected, decoded) =>
        try
          val authored = Term.Name(decoded)
          ScalametaDefinitionNameProjection.project(authored) match
            case Right(projected) if projected == expected => Right(authored)
            case _ => Left(nameUnsupported)
        catch case NonFatal(_) => Left(nameUnsupported)
      }

  private def construct(
      name: Term.Name,
      declaredType: Type,
      rhs: Term
  ): Either[Error, Defn.Val] =
    try Right(Defn.Val(Nil, List(Pat.Var(name)), Some(declaredType), rhs))
    catch case NonFatal(_) => Left(roundTripFailed)

  private def requireExactRoundTrip(
      authored: Defn.Val,
      expected: DefinitionShape.ImmutableVal
  ): Either[Error, Unit] =
    ScalametaTypedImmutableValProjection.project(authored) match
      case Right(ProjectedDefinitionShape(projected, None)) if projected == expected => Right(())
      case _ => Left(roundTripFailed)

  private def shapeUnsupported: Error =
    error(
      "NEUTRAL_TYPED_VAL_AUTHORING_SHAPE_UNSUPPORTED",
      "Core DefinitionShape rejected the immutable val input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_TYPED_VAL_AUTHORING_TYPE_UNSUPPORTED",
      "the declared Type is outside the existing unresolved Type normal-form authoring family."
    )

  private def termUnsupported: Error =
    error(
      "NEUTRAL_TYPED_VAL_AUTHORING_TERM_UNSUPPORTED",
      "the right-hand side is outside the current generic TermShape authoring family."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_TYPED_VAL_AUTHORING_NAME_UNSUPPORTED",
      "the val name cannot be authored as a fresh Term.Name with exact Core spelling."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_TYPED_VAL_AUTHORING_ROUNDTRIP_FAILED",
      "the authored immutable val did not round-trip through the accepted N020 projector exactly."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
