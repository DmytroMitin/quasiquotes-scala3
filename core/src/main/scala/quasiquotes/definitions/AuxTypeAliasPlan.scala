package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

private[quasiquotes] final case class AuxTypeParameterExpectation(
    displayName: String,
    upperBoundName: String
) derives CanEqual

private[quasiquotes] final case class AuxTypeAliasExpectation(
    aliasName: String,
    firstParameter: AuxTypeParameterExpectation,
    secondParameter: AuxTypeParameterExpectation,
    outputParameter: AuxTypeParameterExpectation,
    targetName: String,
    refinementMemberName: String
) derives CanEqual:
  def parameters: Vector[AuxTypeParameterExpectation] =
    Vector(firstParameter, secondParameter, outputParameter)

private[quasiquotes] final case class AuxTypeAliasPlanError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** Compiler-free validated carrier for the bounded AUXify-039 alias family.
  *
  * Source spellings remain explicit caller expectations. Declarations and
  * references use the existing project [[BinderId]] model, so matching text
  * cannot substitute for binder identity.
  */
private[quasiquotes] final class AuxTypeAliasPlan private (
    val aliasDisplayName: String,
    val typeParameters: Vector[ScopedTypeParameter],
    val rhs: Refinement,
    val appliedBase: Applied,
    val refinementMember: ScopedTypeAlias,
    val outputReference: TypeParameterReference
):
  val argumentBinderPositions: Vector[Int] =
    val positions = typeParameters.map(_.binderId).zipWithIndex.toMap
    appliedBase.arguments.map {
      case TypeParameterReference(binderId, _) =>
        positions.getOrElse(binderId, -1)
      case _ => -1
    }

private[quasiquotes] object AuxTypeAliasPlan:
  def validateExpectation(
      expected: AuxTypeAliasExpectation
  ): Either[AuxTypeAliasPlanError, Unit] =
    for
      present <- Option(expected).toRight(
        error(
          "EXPECTED_ALIAS_MISSING",
          "the bounded alias expectation must be present."
        )
      )
      _ <- validateName(
        present.aliasName,
        "EXPECTED_ALIAS_NAME_INVALID",
        "the expected alias name"
      )
      parameters <- Option(present.parameters).toRight(
        error(
          "EXPECTED_TYPE_PARAMETERS_MISSING",
          "the three expected Type-parameter roles must be present."
        )
      )
      _ <- Either.cond(
        parameters.size == 3 && parameters.forall(_ != null),
        (),
        error(
          "EXPECTED_TYPE_PARAMETERS_MISSING",
          "the three expected Type-parameter roles must be present."
        )
      )
      _ <- parameters.zipWithIndex.foldLeft(
        Right(()): Either[AuxTypeAliasPlanError, Unit]
      ) { case (validated, (parameter, index)) =>
        validated.flatMap { _ =>
          for
            _ <- validateName(
              parameter.displayName,
              "EXPECTED_TYPE_PARAMETER_NAME_INVALID",
              s"expected Type parameter ${index + 1}"
            )
            _ <- validateName(
              parameter.upperBoundName,
              "EXPECTED_TYPE_PARAMETER_BOUND_INVALID",
              s"expected Type parameter ${index + 1} upper bound"
            )
          yield ()
        }
      }
      _ <- Either.cond(
        parameters.map(_.displayName).distinct.size == 3,
        (),
        error(
          "EXPECTED_TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT",
          "the three expected Type-parameter names must be distinct."
        )
      )
      _ <- validateName(
        present.targetName,
        "EXPECTED_TARGET_NAME_INVALID",
        "the expected target constructor name"
      )
      _ <- validateName(
        present.refinementMemberName,
        "EXPECTED_REFINEMENT_MEMBER_NAME_INVALID",
        "the expected refinement member name"
      )
    yield ()

  def create(
      aliasDisplayName: String,
      typeParameters: Vector[ScopedTypeParameter],
      rhs: ScopedType,
      expected: AuxTypeAliasExpectation
  ): Either[AuxTypeAliasPlanError, AuxTypeAliasPlan] =
    for
      _ <- validateExpectation(expected)
      _ <- validateName(
        aliasDisplayName,
        "ALIAS_NAME_INVALID",
        "the observed alias name"
      )
      _ <- require(
        aliasDisplayName == expected.aliasName,
        "ALIAS_NAME_MISMATCH",
        "the observed alias name must equal the explicit source expectation."
      )
      declarations <- validateDeclarations(typeParameters, expected.parameters)
      refinement <- rhs match
        case value: Refinement => Right(value)
        case _ =>
          Left(
            error(
              "RHS_REFINEMENT_REQUIRED",
              "the bounded alias RHS must be one refinement."
            )
          )
      _ <- require(
        refinement.members != null &&
          refinement.members.size == 1 &&
          refinement.members.head != null,
        "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
        "the bounded alias RHS must contain exactly one Type-alias member."
      )
      applied <- validateApplied(
        refinement.base,
        declarations,
        expected.targetName
      )
      member = refinement.members.head
      _ <- validateName(
        member.memberName,
        "REFINEMENT_MEMBER_NAME_INVALID",
        "the observed refinement member name"
      )
      _ <- require(
        member.memberName == expected.refinementMemberName,
        "REFINEMENT_MEMBER_NAME_MISMATCH",
        "the observed refinement member must equal the explicit source expectation."
      )
      output <- validateOutputReference(member.rhs, declarations(2))
    yield new AuxTypeAliasPlan(
      aliasDisplayName,
      declarations,
      refinement,
      applied,
      member,
      output
    )

  private def validateDeclarations(
      declarations: Vector[ScopedTypeParameter],
      expected: Vector[AuxTypeParameterExpectation]
  ): Either[AuxTypeAliasPlanError, Vector[ScopedTypeParameter]] =
    if declarations == null || declarations.size != 3 || declarations.exists(_ == null)
    then
      Left(
        error(
          "TYPE_PARAMETER_ARITY_UNSUPPORTED",
          "the bounded alias requires exactly three present Type-parameter declarations."
        )
      )
    else if declarations.exists(_.binderId == null) ||
        declarations.map(_.binderId).distinct.size != 3
    then
      Left(
        error(
          "TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT",
          "the three Type-parameter declarations must use distinct binder identities."
        )
      )
    else if declarations.map(_.displayName).distinct.size != 3 then
      Left(
        error(
          "TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT",
          "the three Type-parameter display names must be distinct."
        )
      )
    else
      declarations.zip(expected).zipWithIndex.foldLeft(
        Right(()): Either[AuxTypeAliasPlanError, Unit]
      ) { case (validated, ((declaration, expectation), index)) =>
        validated.flatMap { _ =>
          for
            _ <- validateName(
              declaration.displayName,
              "TYPE_PARAMETER_DISPLAY_NAME_INVALID",
              s"observed Type parameter ${index + 1}"
            )
            _ <- require(
              declaration.displayName == expectation.displayName,
              "TYPE_PARAMETER_DISPLAY_NAME_MISMATCH",
              s"observed Type parameter ${index + 1} must equal its explicit source expectation."
            )
            bound <- declaration.upperBound match
              case SourceName(value) => Right(value)
              case _ =>
                Left(
                  error(
                    "TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED",
                    s"observed Type parameter ${index + 1} must have one source-named upper bound."
                  )
                )
            _ <- validateName(
              bound,
              "TYPE_PARAMETER_UPPER_BOUND_INVALID",
              s"observed Type parameter ${index + 1} upper bound"
            )
            _ <- require(
              bound == expectation.upperBoundName,
              "TYPE_PARAMETER_UPPER_BOUND_MISMATCH",
              s"observed Type parameter ${index + 1} upper bound must equal its explicit source expectation."
            )
          yield ()
        }
      }.map(_ => declarations)

  private def validateApplied(
      value: ScopedType,
      declarations: Vector[ScopedTypeParameter],
      expectedTarget: String
  ): Either[AuxTypeAliasPlanError, Applied] =
    value match
      case applied @ Applied(SourceName(targetName), arguments) =>
        for
          _ <- validateName(
            targetName,
            "TARGET_NAME_INVALID",
            "the observed target constructor name"
          )
          _ <- require(
            targetName == expectedTarget,
            "TARGET_NAME_MISMATCH",
            "the observed target constructor must equal the explicit source expectation."
          )
          _ <- require(
            arguments != null && arguments.size == 2,
            "TARGET_ARGUMENT_ARITY_UNSUPPORTED",
            "the bounded target constructor must receive exactly two arguments."
          )
          _ <- arguments.zip(declarations.take(2)).zipWithIndex.foldLeft(
            Right(()): Either[AuxTypeAliasPlanError, Unit]
          ) { case (validated, ((argument, declaration), index)) =>
            validated.flatMap { _ =>
              argument match
                case TypeParameterReference(binderId, displayName) =>
                  for
                    _ <- require(
                      binderId == declaration.binderId,
                      "TARGET_BINDER_REFERENCE_MISMATCH",
                      s"target argument ${index + 1} must reference declaration binder ${index + 1}."
                    )
                    _ <- require(
                      displayName == declaration.displayName,
                      "TYPE_PARAMETER_REFERENCE_DISPLAY_NAME_MISMATCH",
                      s"target argument ${index + 1} display spelling must match its declaration."
                    )
                  yield ()
                case _ =>
                  Left(
                    error(
                      "TARGET_ARGUMENT_TOPOLOGY_UNSUPPORTED",
                      "the bounded target admits only direct Type-parameter references."
                    )
                  )
            }
          }
        yield applied
      case Applied(_, _) =>
        Left(
          error(
            "TARGET_CONSTRUCTOR_UNSUPPORTED",
            "the bounded target constructor must be one source-name node."
          )
        )
      case _ =>
        Left(
          error(
            "TARGET_APPLIED_TYPE_REQUIRED",
            "the bounded refinement base must be one applied Type."
          )
        )

  private def validateOutputReference(
      value: ScopedType,
      declaration: ScopedTypeParameter
  ): Either[AuxTypeAliasPlanError, TypeParameterReference] =
    value match
      case reference @ TypeParameterReference(binderId, displayName) =>
        for
          _ <- require(
            binderId == declaration.binderId,
            "OUTPUT_BINDER_REFERENCE_MISMATCH",
            "the refinement RHS must reference the exact third Type binder."
          )
          _ <- require(
            displayName == declaration.displayName,
            "TYPE_PARAMETER_REFERENCE_DISPLAY_NAME_MISMATCH",
            "the refinement RHS display spelling must match the third declaration."
          )
        yield reference
      case _ =>
        Left(
          error(
            "OUTPUT_BINDER_REFERENCE_REQUIRED",
            "the refinement RHS must be one direct Type-parameter reference."
          )
        )

  private def validateName(
      value: String,
      code: String,
      role: String
  ): Either[AuxTypeAliasPlanError, Unit] =
    Option(value)
      .toRight(error(code, s"$role must be present."))
      .flatMap(name =>
        DefinitionName
          .fromSource(name)
          .left
          .map(problem => error(code, s"$role is invalid: ${problem.message}"))
          .map(_ => ())
      )

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[AuxTypeAliasPlanError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): AuxTypeAliasPlanError =
    AuxTypeAliasPlanError(code, detail)
