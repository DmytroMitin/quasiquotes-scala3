package quasiquotes.neutral

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.*

import scala.annotation.nowarn
import scala.meta.*

/** Direct structural authoring for the bounded N001 Aux type-alias plan. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaAuxTypeAliasAuthoring:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def author(plan: AuxTypeAliasPlan): Either[Error, Defn.Type] =
    Option(plan)
      .toRight(
        error(
          "NEUTRAL_AUX_AUTHORING_MISSING",
          "the AuxTypeAliasPlan must be present."
        )
      )
      .flatMap(authorPresent)

  private def authorPresent(plan: AuxTypeAliasPlan): Either[Error, Defn.Type] =
    for
      expected <- deriveExpectation(plan)
      validated <- AuxTypeAliasPlan
        .create(
          plan.aliasDisplayName,
          plan.typeParameters,
          plan.rhs,
          expected
        )
        .left
        .map(problem =>
          error("NEUTRAL_AUX_AUTHORING_PLAN_REJECTED", problem.message)
        )
      authored <- authorValidated(validated)
    yield authored

  private def deriveExpectation(
      plan: AuxTypeAliasPlan
  ): Either[Error, AuxTypeAliasExpectation] =
    for
      parameters <- Option(plan.typeParameters)
        .filter(values => values.size == 3 && !values.exists(_ == null))
        .toRight(topologyUnsupported("exactly three Type-parameter declarations are required."))
      upperBounds <- parameters.foldRight(
        Right(Nil): Either[Error, List[String]]
      ) { (parameter, accumulated) =>
        for
          bound <- parameter.upperBound match
            case SourceName(value) => Right(value)
            case _ =>
              Left(
                topologyUnsupported(
                  "every Type-parameter upper bound must be one source name."
                )
              )
          tail <- accumulated
        yield bound :: tail
      }
      targetName <- Option(plan.appliedBase).flatMap(applied => Option(applied.constructor)) match
        case Some(SourceName(value)) => Right(value)
        case _ =>
          Left(
            topologyUnsupported(
              "the applied-base constructor must be one source name."
            )
          )
      member <- Option(plan.refinementMember).toRight(
        topologyUnsupported("exactly one refinement Type-alias member is required.")
      )
    yield AuxTypeAliasExpectation(
      aliasName = plan.aliasDisplayName,
      firstParameter = AuxTypeParameterExpectation(
        parameters(0).displayName,
        upperBounds(0)
      ),
      secondParameter = AuxTypeParameterExpectation(
        parameters(1).displayName,
        upperBounds(1)
      ),
      outputParameter = AuxTypeParameterExpectation(
        parameters(2).displayName,
        upperBounds(2)
      ),
      targetName = targetName,
      refinementMemberName = member.memberName
    )

  private def authorValidated(
      plan: AuxTypeAliasPlan
  ): Either[Error, Defn.Type] =
    val parameters = plan.typeParameters
    val authoredParameters = parameters.map { parameter =>
      val SourceName(upperBound) = parameter.upperBound: @unchecked
      Type.Param(
        Nil,
        Type.Name(parameter.displayName),
        Type.ParamClause(Nil),
        Type.Bounds(None, Some(Type.Name(upperBound)), Nil, Nil)
      )
    }.toList
    val SourceName(targetName) = plan.appliedBase.constructor: @unchecked
    val applied = Type.Apply(
      Type.Name(targetName),
      Type.ArgClause(
        List(
          Type.Name(parameters(0).displayName),
          Type.Name(parameters(1).displayName)
        )
      )
    )
    val member = Defn.Type(
      Nil,
      Type.Name(plan.refinementMember.memberName),
      Type.ParamClause(Nil),
      Type.Name(parameters(2).displayName),
      Type.Bounds.empty
    )
    val refinement = Type.Refine(Some(applied), Stat.Block(List(member)))
    Right(
      Defn.Type(
        Nil,
        Type.Name(plan.aliasDisplayName),
        Type.ParamClause(authoredParameters),
        refinement,
        Type.Bounds.empty
      )
    )

  private def topologyUnsupported(detail: String): Error =
    error("NEUTRAL_AUX_AUTHORING_TOPOLOGY_UNSUPPORTED", detail)

  private def error(code: String, detail: String): Error =
    Error(code, detail)
