package quasiquotes.definitions.dotty

import quasiquotes.definitions.AuxTypeAliasPlan
import quasiquotes.definitions.ScopedType.*
import AuxTypeAliasUntypedLoweringInput.*

private[quasiquotes] final case class AuxTypeAliasPlanUntypedInputAdapterError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Mechanical, identity-preserving seam from the N001 plan to U001 input. */
private[quasiquotes] object AuxTypeAliasPlanUntypedInputAdapter:
  def adapt(
      plan: AuxTypeAliasPlan
  ): Either[AuxTypeAliasPlanUntypedInputAdapterError, Validated] =
    for
      present <- Option(plan).toRight(
        error("PLAN_INPUT_REQUIRED", "the N001 alias plan must be present.")
      )
      parameters <- Option(present.typeParameters)
        .filter(values => values.size == 3 && values.forall(_ != null))
        .toRight(
          invariant(
            "the N001 plan must contain exactly three present Type-parameter declarations."
          )
        )
      _ <- Either.cond(
        parameters.forall(_.binderId != null) &&
          parameters.map(_.binderId).distinct.size == 3,
        (),
        invariant("the N001 plan must retain three distinct binder identities.")
      )
      uParameters <- sequence(parameters.zipWithIndex.map { case (parameter, index) =>
        parameter.upperBound match
          case SourceName(value) =>
            Right(
              TypeParameter(
                binderId = parameter.binderId,
                displayName = parameter.displayName,
                lowerBound = None,
                upperBound = Some(TypeInput.SourceName(value))
              )
            )
          case _ =>
            Left(
              invariant(
                s"N001 Type parameter ${index + 1} must retain one source-named upper bound."
              )
            )
      })
      constructor <- present.appliedBase.constructor match
        case SourceName(value) => Right(TypeInput.SourceName(value))
        case _ =>
          Left(
            invariant("the N001 applied target constructor must be one source name.")
          )
      targetArguments <- Option(present.appliedBase.arguments)
        .filter(values => values.size == 2 && values.forall(_ != null))
        .toRight(
          invariant("the N001 applied target must retain exactly two references.")
        )
      uTargetArguments <- sequence(
        targetArguments.zip(parameters.take(2)).zipWithIndex.map {
          case ((TypeParameterReference(binderId, displayName), declaration), index)
              if binderId == declaration.binderId &&
                displayName == declaration.displayName =>
            Right(TypeInput.BinderReference(binderId, displayName))
          case ((_, _), index) =>
            Left(
              invariant(
                s"N001 target reference ${index + 1} must retain declaration binder ${index + 1}."
              )
            )
        }
      )
      _ <- Either.cond(
        present.rhs != null &&
          present.rhs.base == present.appliedBase &&
          present.rhs.members == Vector(present.refinementMember),
        (),
        invariant("the N001 root refinement fields must remain internally coherent.")
      )
      output <- Option(present.outputReference)
        .filter(reference =>
          reference.binderId == parameters(2).binderId &&
            reference.displayName == parameters(2).displayName
        )
        .toRight(
          invariant("the N001 refinement RHS must retain declaration binder 3.")
        )
      description = Description(
        aliasName = present.aliasDisplayName,
        parameters = uParameters,
        target = TypeInput.Applied(constructor, uTargetArguments),
        refinements = Vector(
          DirectTypeAlias(
            memberName = present.refinementMember.memberName,
            typeParameters = Vector.empty,
            lowerBound = None,
            upperBound = None,
            modifiers = Vector.empty,
            rhs = TypeInput.BinderReference(
              output.binderId,
              output.displayName
            )
          )
        )
      )
      validated <- AuxTypeAliasUntypedLoweringInput
        .validate(description)
        .left
        .map(problem =>
          error("U_INPUT_VALIDATION_FAILED", problem.message)
        )
    yield validated

  private def sequence[A](
      values: Vector[Either[AuxTypeAliasPlanUntypedInputAdapterError, A]]
  ): Either[AuxTypeAliasPlanUntypedInputAdapterError, Vector[A]] =
    values.foldLeft(
      Right(Vector.empty): Either[
        AuxTypeAliasPlanUntypedInputAdapterError,
        Vector[A]
      ]
    ) { (result, value) =>
      for
        accumulated <- result
        next <- value
      yield accumulated :+ next
    }

  private def invariant(detail: String): AuxTypeAliasPlanUntypedInputAdapterError =
    error("PLAN_SHAPE_INVARIANT_FAILED", detail)

  private def error(
      code: String,
      detail: String
  ): AuxTypeAliasPlanUntypedInputAdapterError =
    AuxTypeAliasPlanUntypedInputAdapterError(code, detail)
