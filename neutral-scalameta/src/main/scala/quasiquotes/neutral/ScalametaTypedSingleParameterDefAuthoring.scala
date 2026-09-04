package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.parser.BinderId
import _root_.quasiquotes.terms.TermShapeTraversal
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for one reusable explicitly typed single-parameter method shape. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedSingleParameterDefAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      shape: DefinitionShape.SingleParameterDef
  ): Either[Error, Defn.Def] =
    Option(shape)
      .toRight(
        error(
          "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_MISSING",
          "the single-parameter def shape must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(
      shape: DefinitionShape.SingleParameterDef
  ): Either[Error, Defn.Def] =
    for
      validated <- DefinitionShape
        .singleParameterDef(
          shape.name,
          shape.parameterBinderId,
          shape.parameterName,
          shape.parameterType,
          shape.resultType,
          shape.body
        )
        .left
        .map(_ => shapeUnsupported)
      parameterNormalForm <- TypeNormalForm
        .fromShape(validated.parameterType)
        .left
        .map(_ => typeUnsupported)
      authoredParameterType <- ScalametaTypeNormalFormAuthoring
        .author(parameterNormalForm)
        .left
        .map(_ => typeUnsupported)
      resultNormalForm <- TypeNormalForm
        .fromShape(validated.resultType)
        .left
        .map(_ => typeUnsupported)
      authoredResultType <- ScalametaTypeNormalFormAuthoring
        .author(resultNormalForm)
        .left
        .map(_ => typeUnsupported)
      authoredMethodName <- ScalametaTermDefinitionNameAuthoring
        .author(validated.name)
        .toRight(nameUnsupported)
      authoredParameterName <- ScalametaTermDefinitionNameAuthoring
        .author(validated.parameterName)
        .toRight(nameUnsupported)
      authoredBody <- ScalametaTermShapeAuthoring
        .authorWithDefinitionBinders(
          validated.body,
          Vector(
            ScalametaTermShapeAuthoring.DefinitionBinder(
              validated.parameterBinderId,
              validated.parameterName
            )
          )
        )
        .left
        .map(_ => termUnsupported)
      authored <- construct(
        authoredMethodName,
        authoredParameterName,
        authoredParameterType,
        authoredResultType,
        authoredBody
      )
      _ <- requireExactRoundTrip(authored, validated)
    yield authored

  private def construct(
      methodName: Term.Name,
      parameterName: Term.Name,
      parameterType: Type,
      resultType: Type,
      body: Term
  ): Either[Error, Defn.Def] =
    try
      Right(
        Defn.Def(
          Nil,
          methodName,
          List(
            Member.ParamClauseGroup(
              Type.ParamClause(Nil),
              List(
                Term.ParamClause(
                  List(Term.Param(Nil, parameterName, Some(parameterType), None))
                )
              )
            )
          ),
          Some(resultType),
          body
        )
      )
    catch case NonFatal(_) => Left(roundTripFailed)

  private def requireExactRoundTrip(
      authored: Defn.Def,
      expected: DefinitionShape.SingleParameterDef
  ): Either[Error, Unit] =
    ScalametaTypedSingleParameterDefProjection.project(authored) match
      case Right(ProjectedDefinitionShape(projected: DefinitionShape.SingleParameterDef, None)) =>
        Either.cond(
          projected.parameterBinderId == BinderId(0) &&
            projected.name == expected.name &&
            projected.parameterName == expected.parameterName &&
            projected.parameterType == expected.parameterType &&
            projected.resultType == expected.resultType &&
            TermShapeTraversal.alphaNormalizeInScope(
              projected.body,
              projected.parameterBinderId
            ) == TermShapeTraversal.alphaNormalizeInScope(
              expected.body,
              expected.parameterBinderId
            ) &&
            projected == expected,
          (),
          roundTripFailed
        )
      case _ => Left(roundTripFailed)

  private def shapeUnsupported: Error =
    error(
      "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_SHAPE_UNSUPPORTED",
      "Core DefinitionShape rejected the single-parameter def input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TYPE_UNSUPPORTED",
      "the parameter or result Type is outside the existing unresolved Type normal-form authoring family."
    )

  private def termUnsupported: Error =
    error(
      "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED",
      "the method body is outside the current definition-binder-aware TermShape authoring family."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED",
      "the method or parameter name cannot be authored as a fresh Term.Name with exact Core spelling."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_ROUNDTRIP_FAILED",
      "the authored single-parameter def did not round-trip through the accepted N022 projector semantically."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
