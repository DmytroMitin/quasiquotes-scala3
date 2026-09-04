package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*

/** Direct structural authoring for one reusable simple type-alias shape. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaSimpleTypeAliasAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      shape: DefinitionShape.SimpleTypeAlias
  ): Either[Error, Defn.Type] =
    Option(shape)
      .toRight(
        error(
          "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_MISSING",
          "the simple type alias shape must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(
      shape: DefinitionShape.SimpleTypeAlias
  ): Either[Error, Defn.Type] =
    for
      validated <- DefinitionShape
        .simpleTypeAlias(shape.name, shape.rhs)
        .left
        .map(_ => shapeUnsupported)
      normalForm <- TypeNormalForm
        .fromShape(validated.rhs)
        .left
        .map(_ => typeUnsupported)
      authoredRhs <- ScalametaTypeNormalFormAuthoring
        .author(normalForm)
        .left
        .map(_ => typeUnsupported)
      authoredName <- authorName(validated.name)
      authored = Defn.Type(
        Nil,
        authoredName,
        Type.ParamClause(Nil),
        authoredRhs,
        Type.Bounds.empty
      )
      _ <- requireExactRoundTrip(authored, shape)
    yield authored

  private def authorName(name: DefinitionName): Either[Error, Type.Name] =
    Option(name)
      .flatMap(value => Option(value.decoded).map(value -> _))
      .toRight(nameUnsupported)
      .flatMap { (expected, decoded) =>
        val authored = Type.Name(decoded)
        ScalametaDefinitionNameProjection.project(authored) match
          case Right(projected) if projected == expected => Right(authored)
          case _ => Left(nameUnsupported)
      }

  private def requireExactRoundTrip(
      authored: Defn.Type,
      expected: DefinitionShape.SimpleTypeAlias
  ): Either[Error, Unit] =
    ScalametaSimpleTypeAliasProjection.project(authored) match
      case Right(ProjectedDefinitionShape(projected, None)) if projected == expected => Right(())
      case _ => Left(roundTripFailed)

  private def shapeUnsupported: Error =
    error(
      "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_SHAPE_UNSUPPORTED",
      "Core DefinitionShape rejected the simple type alias input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_TYPE_UNSUPPORTED",
      "the alias right-hand side is outside the existing unresolved Type normal-form authoring family."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_NAME_UNSUPPORTED",
      "the alias name cannot be authored as a fresh Type.Name with exact Core spelling."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_ROUNDTRIP_FAILED",
      "the authored alias did not round-trip through the accepted N024 projector exactly."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
