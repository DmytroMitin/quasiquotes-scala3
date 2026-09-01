package quasiquotes.definitions.dotty

import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.BinderId

private[quasiquotes] final case class AuxTypeAliasUntypedLoweringError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

/** U-local structural input seam for the one admitted AUXify-039 alias family. */
private[quasiquotes] object AuxTypeAliasUntypedLoweringInput:
  enum Variance derives CanEqual:
    case Invariant, Covariant, Contravariant

  sealed trait TypeInput derives CanEqual
  object TypeInput:
    final case class SourceName(value: String) extends TypeInput
    final case class BinderReference(binderId: BinderId, displayName: String)
        extends TypeInput
    final case class Applied(constructor: TypeInput, arguments: Vector[TypeInput])
        extends TypeInput
    final case class Qualified(prefix: String, member: String) extends TypeInput

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String,
      lowerBound: Option[TypeInput],
      upperBound: Option[TypeInput],
      variance: Variance = Variance.Invariant,
      nestedTypeParameters: Vector[TypeParameter] = Vector.empty,
      contextBounds: Vector[TypeInput] = Vector.empty,
      viewBounds: Vector[TypeInput] = Vector.empty
  ) derives CanEqual

  sealed trait RefinementInput derives CanEqual

  final case class DirectTypeAlias(
      memberName: String,
      typeParameters: Vector[TypeParameter],
      lowerBound: Option[TypeInput],
      upperBound: Option[TypeInput],
      modifiers: Vector[String],
      rhs: TypeInput
  ) extends RefinementInput

  final case class AbstractTypeAlias(
      memberName: String,
      lowerBound: Option[TypeInput],
      upperBound: Option[TypeInput]
  ) extends RefinementInput

  final case class Description(
      aliasName: String,
      parameters: Vector[TypeParameter],
      target: TypeInput,
      refinements: Vector[RefinementInput]
  ) derives CanEqual

  final class Validated private[AuxTypeAliasUntypedLoweringInput] (
      val aliasName: String,
      val parameters: Vector[TypeParameter],
      val target: TypeInput.Applied,
      val refinement: DirectTypeAlias
  )

  def validate(
      description: Description
  ): Either[AuxTypeAliasUntypedLoweringError, Validated] =
    import TypeInput.*
    for
      present <- Option(description).toRight(
        error("VALIDATED_INPUT_REQUIRED", "the lowering description must be present.")
      )
      _ <- validateName(present.aliasName, "ALIAS_NAME_INVALID", "alias")
      parameters <- validateParameters(present.parameters)
      applied <- present.target match
        case value: Applied => Right(value)
        case _ =>
          Left(
            error(
              "TARGET_TOPOLOGY_UNSUPPORTED",
              "the target must be one applied Type."
            )
          )
      _ <- applied.constructor match
        case SourceName(value) =>
          validateName(
            value,
            "TARGET_CONSTRUCTOR_UNSUPPORTED",
            "target constructor"
          )
        case _ =>
          Left(
            error(
              "TARGET_CONSTRUCTOR_UNSUPPORTED",
              "the target constructor must be one simple source name."
            )
          )
      _ <- Either.cond(
        applied.arguments != null && applied.arguments.size == 2,
        (),
        error(
          "TARGET_ARITY_UNSUPPORTED",
          "the target constructor must receive exactly two Type arguments."
        )
      )
      _ <- validateTargetReference(applied.arguments(0), parameters(0), 1)
      _ <- validateTargetReference(applied.arguments(1), parameters(1), 2)
      refinements <- Option(present.refinements).toRight(
        error(
          "REFINEMENT_CARDINALITY_UNSUPPORTED",
          "the RHS requires exactly one direct refinement alias."
        )
      )
      _ <- Either.cond(
        refinements.size == 1,
        (),
        error(
          "REFINEMENT_CARDINALITY_UNSUPPORTED",
          "the RHS requires exactly one direct refinement alias."
        )
      )
      member <- refinements.head match
        case value: DirectTypeAlias => Right(value)
        case _ =>
          Left(
            error(
              "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
              "the refinement member must be one direct Type alias."
            )
          )
      _ <- validateName(
        member.memberName,
        "REFINEMENT_MEMBER_NAME_INVALID",
        "refinement member"
      )
      _ <- Either.cond(
        member.typeParameters != null && member.typeParameters.isEmpty &&
          member.lowerBound != null && member.lowerBound.isEmpty &&
          member.upperBound != null && member.upperBound.isEmpty &&
          member.modifiers != null && member.modifiers.isEmpty,
        (),
        error(
          "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
          "the refinement alias cannot have parameters, bounds, or modifiers."
        )
      )
      _ <- member.rhs match
        case reference: BinderReference =>
          validateBinderReference(
            reference,
            parameters(2),
            "REFINEMENT_RHS_BINDER_MISMATCH",
            "the refinement RHS must reference Type binder 3."
          )
        case _ =>
          Left(
            error(
              "REFINEMENT_RHS_TOPOLOGY_UNSUPPORTED",
              "the refinement RHS must be one direct Type-binder reference."
            )
          )
    yield new Validated(present.aliasName, parameters, applied, member)

  private def validateParameters(
      parameters: Vector[TypeParameter]
  ): Either[AuxTypeAliasUntypedLoweringError, Vector[TypeParameter]] =
    import TypeInput.SourceName
    if parameters == null || parameters.size != 3 || parameters.exists(_ == null)
    then
      Left(
        error(
          "TYPE_PARAMETER_ARITY_UNSUPPORTED",
          "the alias requires exactly three present Type parameters."
        )
      )
    else if parameters.exists(_.binderId == null) ||
        parameters.map(_.binderId).distinct.size != 3
    then
      Left(
        error(
          "TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT",
          "all three Type-binder identities must be present and distinct."
        )
      )
    else if parameters.map(_.displayName).distinct.size != 3 then
      Left(
        error(
          "TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT",
          "all three Type-parameter source names must be distinct."
        )
      )
    else
      parameters.zipWithIndex
        .foldLeft(
          Right(()): Either[AuxTypeAliasUntypedLoweringError, Unit]
        ) { case (result, (parameter, index)) =>
          result.flatMap(_ =>
            for
              _ <- validateName(
                parameter.displayName,
                "TYPE_PARAMETER_NAME_INVALID",
                s"Type parameter ${index + 1}"
              )
              _ <- Either.cond(
                parameter.lowerBound != null && parameter.lowerBound.isEmpty,
                (),
                error(
                  "TYPE_PARAMETER_BOUND_UNSUPPORTED",
                  s"Type parameter ${index + 1} cannot have a lower bound."
                )
              )
              _ <- parameter.upperBound match
                case Some(SourceName(value)) =>
                  validateName(
                    value,
                    "TYPE_PARAMETER_BOUND_UNSUPPORTED",
                    s"Type parameter ${index + 1} upper bound"
                  )
                case _ =>
                  Left(
                    error(
                      "TYPE_PARAMETER_BOUND_UNSUPPORTED",
                      s"Type parameter ${index + 1} requires one simple source-named upper bound."
                    )
                  )
              _ <- Either.cond(
                parameter.variance == Variance.Invariant,
                (),
                error(
                  "TYPE_PARAMETER_VARIANCE_UNSUPPORTED",
                  s"Type parameter ${index + 1} must be invariant."
                )
              )
              _ <- Either.cond(
                parameter.nestedTypeParameters != null &&
                  parameter.nestedTypeParameters.isEmpty,
                (),
                error(
                  "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
                  s"Type parameter ${index + 1} cannot be higher-kinded."
                )
              )
              _ <- Either.cond(
                parameter.contextBounds != null && parameter.contextBounds.isEmpty,
                (),
                error(
                  "TYPE_PARAMETER_CONTEXT_BOUNDS_UNSUPPORTED",
                  s"Type parameter ${index + 1} cannot have context bounds."
                )
              )
              _ <- Either.cond(
                parameter.viewBounds != null && parameter.viewBounds.isEmpty,
                (),
                error(
                  "TYPE_PARAMETER_VIEW_BOUNDS_UNSUPPORTED",
                  s"Type parameter ${index + 1} cannot have view bounds."
                )
              )
            yield ()
          )
        }
        .map(_ => parameters)

  private def validateTargetReference(
      input: TypeInput,
      expected: TypeParameter,
      position: Int
  ): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    input match
      case reference: TypeInput.BinderReference =>
        validateBinderReference(
          reference,
          expected,
          "TARGET_BINDER_REFERENCE_MISMATCH",
          s"target argument $position must reference Type binder $position."
        )
      case _ =>
        Left(
          error(
            "TARGET_ARGUMENT_TOPOLOGY_UNSUPPORTED",
            s"target argument $position must be one direct Type-binder reference."
          )
        )

  private def validateBinderReference(
      reference: TypeInput.BinderReference,
      expected: TypeParameter,
      code: String,
      detail: String
  ): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    Either.cond(
      reference != null && reference.binderId != null &&
        reference.binderId == expected.binderId &&
        reference.displayName == expected.displayName,
      (),
      error(code, detail)
    )

  private def validateName(
      value: String,
      code: String,
      role: String
  ): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    Option(value)
      .toRight(error(code, s"the $role name must be present."))
      .flatMap(name =>
        DefinitionName
          .fromSource(name)
          .left
          .map(problem => error(code, problem.message))
          .map(_ => ())
      )

  private def error(
      code: String,
      detail: String
  ): AuxTypeAliasUntypedLoweringError =
    AuxTypeAliasUntypedLoweringError(code, detail)
