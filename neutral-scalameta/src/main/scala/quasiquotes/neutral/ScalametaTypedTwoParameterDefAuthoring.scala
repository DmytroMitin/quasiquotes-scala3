package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionShape
import _root_.quasiquotes.parser.BinderId
import _root_.quasiquotes.terms.TermShapeTraversal
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.util.control.NonFatal

/** Direct structural authoring for one reusable explicitly typed two-parameter method shape. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypedTwoParameterDefAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(
      shape: DefinitionShape.TwoParameterDef
  ): Either[Error, Defn.Def] =
    Option(shape)
      .toRight(
        error(
          "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_MISSING",
          "the two-parameter def shape must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(
      shape: DefinitionShape.TwoParameterDef
  ): Either[Error, Defn.Def] =
    for
      validated <- DefinitionShape
        .twoParameterDef(
          shape.name,
          shape.firstParameterBinderId,
          shape.firstParameterName,
          shape.firstParameterType,
          shape.secondParameterBinderId,
          shape.secondParameterName,
          shape.secondParameterType,
          shape.resultType,
          shape.body
        )
        .left
        .map(_ => shapeUnsupported)
      firstParameterNormalForm <- TypeNormalForm
        .fromShape(validated.firstParameterType)
        .left
        .map(_ => typeUnsupported)
      authoredFirstParameterType <- ScalametaTypeNormalFormAuthoring
        .author(firstParameterNormalForm)
        .left
        .map(_ => typeUnsupported)
      secondParameterNormalForm <- TypeNormalForm
        .fromShape(validated.secondParameterType)
        .left
        .map(_ => typeUnsupported)
      authoredSecondParameterType <- ScalametaTypeNormalFormAuthoring
        .author(secondParameterNormalForm)
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
      authoredFirstParameterName <- ScalametaTermDefinitionNameAuthoring
        .author(validated.firstParameterName)
        .toRight(nameUnsupported)
      authoredSecondParameterName <- ScalametaTermDefinitionNameAuthoring
        .author(validated.secondParameterName)
        .toRight(nameUnsupported)
      authoredBody <- ScalametaTermShapeAuthoring
        .authorWithDefinitionBinders(
          validated.body,
          Vector(
            ScalametaTermShapeAuthoring.DefinitionBinder(
              validated.firstParameterBinderId,
              validated.firstParameterName
            ),
            ScalametaTermShapeAuthoring.DefinitionBinder(
              validated.secondParameterBinderId,
              validated.secondParameterName
            )
          )
        )
        .left
        .map(_ => termUnsupported)
      authored <- construct(
        authoredMethodName,
        authoredFirstParameterName,
        authoredFirstParameterType,
        authoredSecondParameterName,
        authoredSecondParameterType,
        authoredResultType,
        authoredBody
      )
      _ <- requireExactRoundTrip(authored, validated)
    yield authored

  private def construct(
      methodName: Term.Name,
      firstParameterName: Term.Name,
      firstParameterType: Type,
      secondParameterName: Term.Name,
      secondParameterType: Type,
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
                  List(
                    Term.Param(Nil, firstParameterName, Some(firstParameterType), None),
                    Term.Param(Nil, secondParameterName, Some(secondParameterType), None)
                  )
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
      expected: DefinitionShape.TwoParameterDef
  ): Either[Error, Unit] =
    ScalametaTypedTwoParameterDefProjection.project(authored) match
      case Right(ProjectedDefinitionShape(projected: DefinitionShape.TwoParameterDef, None)) =>
        Either.cond(
          projected.firstParameterBinderId == BinderId(0) &&
            projected.secondParameterBinderId == BinderId(1) &&
            projected.name == expected.name &&
            projected.firstParameterName == expected.firstParameterName &&
            projected.secondParameterName == expected.secondParameterName &&
            projected.firstParameterType == expected.firstParameterType &&
            projected.secondParameterType == expected.secondParameterType &&
            projected.resultType == expected.resultType &&
            TermShapeTraversal.alphaNormalizeInScope(
              projected.body,
              Vector(projected.firstParameterBinderId, projected.secondParameterBinderId)
            ) == TermShapeTraversal.alphaNormalizeInScope(
              expected.body,
              Vector(expected.firstParameterBinderId, expected.secondParameterBinderId)
            ) &&
            projected == expected,
          (),
          roundTripFailed
        )
      case _ => Left(roundTripFailed)

  private def shapeUnsupported: Error =
    error(
      "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_SHAPE_UNSUPPORTED",
      "Core DefinitionShape rejected the two-parameter def input."
    )

  private def typeUnsupported: Error =
    error(
      "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TYPE_UNSUPPORTED",
      "the first parameter, second parameter, or result Type is outside the existing unresolved Type normal-form authoring family."
    )

  private def termUnsupported: Error =
    error(
      "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_TERM_UNSUPPORTED",
      "the method body is outside the current definition-binder-aware TermShape authoring family."
    )

  private def nameUnsupported: Error =
    error(
      "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_NAME_UNSUPPORTED",
      "the method or parameter name cannot be authored as a fresh Term.Name with exact Core spelling."
    )

  private def roundTripFailed: Error =
    error(
      "NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_ROUNDTRIP_FAILED",
      "the authored two-parameter def did not round-trip through the accepted N023 projector semantically."
    )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
