package quasiquotes.definitions

import quasiquotes.parser.BinderId

private[quasiquotes] sealed trait ScopedType derives CanEqual

private[quasiquotes] object ScopedType:
  final case class SourceName(value: String) extends ScopedType

  final case class TypeParameterReference(
      binderId: BinderId,
      displayName: String
  ) extends ScopedType

  final case class Applied(
      constructor: ScopedType,
      arguments: Vector[ScopedType]
  ) extends ScopedType

  final case class DirectStableSelected(
      prefixTermBinderId: BinderId,
      memberExpectation: String
  ) extends ScopedType

  final case class Refinement(
      base: ScopedType,
      members: Vector[ScopedTypeAlias]
  ) extends ScopedType

private[quasiquotes] final case class ScopedTypeAlias(
    memberName: String,
    rhs: ScopedType
) derives CanEqual

private[quasiquotes] final case class ScopedTypeParameter(
    binderId: BinderId,
    displayName: String,
    upperBound: ScopedType
) derives CanEqual

private[quasiquotes] final case class ScopedContextualMethodPlanError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

private[quasiquotes] final case class ScopedContextualMethodAlphaKey(
    methodName: String,
    upperBounds: Vector[String],
    appliedConstructor: String,
    appliedArgumentPositions: Vector[Int],
    refinementMemberName: String,
    selectedPrefixCategoryPosition: Int,
    selectedMemberExpectation: String,
    bodyCategoryPosition: Int
) derives CanEqual

/** Validated package-private carrier for the exact AUXify-037 internal method.
  *
  * Type-parameter declarations/references and the contextual Term prefix use
  * the same project [[BinderId]] allocation, while their node categories keep
  * their meanings distinct. Display spelling is retained only for source
  * emission and deterministic validation.
  */
private[quasiquotes] final class ScopedContextualMethodPlan private (
    val methodDisplayName: String,
    val typeParameters: Vector[ScopedTypeParameter],
    val contextualTermBinderId: BinderId,
    val contextualDisplayName: String,
    val contextualType: ScopedType.Applied,
    val resultType: ScopedType.Refinement,
    val refinementMember: ScopedTypeAlias,
    val selectedResult: ScopedType.DirectStableSelected,
    val bodyTermBinderId: BinderId
):
  val typeArgumentBinderPositions: Vector[Int] =
    val positions = typeParameters.map(_.binderId).zipWithIndex.toMap
    contextualType.arguments.map {
      case ScopedType.TypeParameterReference(binderId, _) =>
        positions.getOrElse(binderId, -1)
      case _ => -1
    }

  val alphaKey: ScopedContextualMethodAlphaKey =
    ScopedContextualMethodAlphaKey(
      methodName = methodDisplayName,
      upperBounds = typeParameters.map {
        case ScopedTypeParameter(_, _, ScopedType.SourceName(value)) => value
        case _ => "<invalid>"
      },
      appliedConstructor = contextualType.constructor match
        case ScopedType.SourceName(value) => value
        case _ => "<invalid>",
      appliedArgumentPositions = typeArgumentBinderPositions,
      refinementMemberName = refinementMember.memberName,
      selectedPrefixCategoryPosition =
        if selectedResult.prefixTermBinderId == contextualTermBinderId then 0
        else -1,
      selectedMemberExpectation = selectedResult.memberExpectation,
      bodyCategoryPosition =
        if bodyTermBinderId == contextualTermBinderId then 0 else -1
    )

private[quasiquotes] object ScopedContextualMethodPlan:
  import ScopedType.*

  def create(
      methodDisplayName: String,
      typeParameters: Vector[ScopedTypeParameter],
      contextualTermBinderId: BinderId,
      contextualDisplayName: String,
      contextualType: ScopedType,
      resultType: ScopedType,
      bodyTermBinderId: BinderId
  ): Either[ScopedContextualMethodPlanError, ScopedContextualMethodPlan] =
    for
      declarations <- validateDeclarations(typeParameters, contextualTermBinderId)
      _ <- validateName(
        methodDisplayName,
        "METHOD_DISPLAY_NAME_INVALID",
        "the method display name"
      )
      _ <- validateName(
        contextualDisplayName,
        "CONTEXTUAL_DISPLAY_NAME_INVALID",
        "the contextual Term display name"
      )
      contextualApplied <- validateApplied(contextualType, declarations)
      validatedResult <- validateResult(
        resultType,
        contextualApplied,
        declarations,
        contextualTermBinderId
      )
      (refinement, member, selected) = validatedResult
      _ <- Either.cond(
        bodyTermBinderId == contextualTermBinderId,
        (),
        error(
          "CONTEXTUAL_BODY_BINDER_MISMATCH",
          "the method body must reference the exact contextual Term binder."
        )
      )
    yield new ScopedContextualMethodPlan(
      methodDisplayName,
      declarations,
      contextualTermBinderId,
      contextualDisplayName,
      contextualApplied,
      refinement,
      member,
      selected,
      bodyTermBinderId
    )

  private def validateDeclarations(
      declarations: Vector[ScopedTypeParameter],
      contextualBinder: BinderId
  ): Either[ScopedContextualMethodPlanError, Vector[ScopedTypeParameter]] =
    if declarations == null || declarations.size != 2 then
      Left(
        error(
          "TYPE_PARAMETER_ARITY_UNSUPPORTED",
          "the bounded method model requires exactly two Type-parameter declarations."
        )
      )
    else if declarations.exists(_ == null) then
      Left(
        error(
          "TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED",
          "a Type-parameter declaration was missing."
        )
      )
    else
      val binders = declarations.map(_.binderId)
      if contextualBinder == null || binders.exists(_ == null) ||
          binders.distinct.size != 2 || binders.contains(contextualBinder)
      then
        Left(
          error(
            "TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT",
            "both Type binders and the contextual Term binder must be distinct."
          )
        )
      else if declarations.map(_.displayName).distinct.size != 2 then
        Left(
          error(
            "TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT",
            "the two Type-parameter display names must be distinct."
          )
        )
      else
        declarations
          .zipWithIndex
          .foldLeft(
            Right(()): Either[ScopedContextualMethodPlanError, Unit]
          ) { case (validated, (declaration, index)) =>
            validated.flatMap { _ =>
              for
                _ <- validateName(
                  declaration.displayName,
                  "TYPE_PARAMETER_DISPLAY_NAME_INVALID",
                  s"Type-parameter ${index + 1} display name"
                )
                _ <- declaration.upperBound match
                  case SourceName(value) =>
                    validateName(
                      value,
                      "TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED",
                      s"Type-parameter ${index + 1} upper bound"
                    )
                  case _ =>
                    Left(
                      error(
                        "TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED",
                        s"Type-parameter ${index + 1} must have exactly one source-named upper bound."
                      )
                    )
              yield ()
            }
          }
          .map(_ => declarations)

  private def validateApplied(
      value: ScopedType,
      declarations: Vector[ScopedTypeParameter]
  ): Either[ScopedContextualMethodPlanError, Applied] =
    value match
      case applied @ Applied(SourceName(constructor), arguments) =>
        for
          _ <- validateName(
            constructor,
            "TYPE_CONSTRUCTOR_NAME_INVALID",
            "the applied Type constructor"
          )
          references <- collectReferences(arguments)
          declaredBinders = declarations.map(_.binderId)
          _ <- Either.cond(
            references.forall(reference => declaredBinders.contains(reference.binderId)),
            (),
            error(
              "TYPE_PARAMETER_REFERENCE_UNBOUND",
              "every applied Type argument must reference a Type binder declared by this method."
            )
          )
          _ <- Either.cond(
            references.size == 2 && references.map(_.binderId) == declaredBinders,
            (),
            error(
              "TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH",
              "the exact applied Type arguments must reference declaration positions 0 and 1 once, in order."
            )
          )
          _ <- Either.cond(
            references.map(_.displayName) == declarations.map(_.displayName),
            (),
            error(
              "TYPE_PARAMETER_REFERENCE_DISPLAY_NAME_MISMATCH",
              "a Type-parameter reference display spelling diverged from its declaration."
            )
          )
        yield applied
      case Applied(_, _) =>
        Left(
          error(
            "TYPE_CONSTRUCTOR_NAME_INVALID",
            "the applied Type constructor must be one bounded source-name node."
          )
        )
      case _ =>
        Left(
          error(
            "TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH",
            "the contextual and refinement base Types must be exact scoped applications."
          )
        )

  private def collectReferences(
      arguments: Vector[ScopedType]
  ): Either[ScopedContextualMethodPlanError, Vector[TypeParameterReference]] =
    if arguments == null then
      Left(
        error(
          "TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH",
          "the applied Type argument vector was missing."
        )
      )
    else
      arguments.foldLeft(
        Right(Vector.empty): Either[
          ScopedContextualMethodPlanError,
          Vector[TypeParameterReference]
        ]
      ) { (accumulated, argument) =>
        for
          values <- accumulated
          reference <- argument match
            case value: TypeParameterReference => Right(value)
            case _ =>
              Left(
                error(
                  "TYPE_PARAMETER_REFERENCE_UNBOUND",
                  "the exact applied Type admits only scoped Type-parameter reference arguments."
                )
              )
        yield values :+ reference
      }

  private def validateResult(
      value: ScopedType,
      contextualType: Applied,
      declarations: Vector[ScopedTypeParameter],
      contextualBinder: BinderId
  ): Either[
    ScopedContextualMethodPlanError,
    (Refinement, ScopedTypeAlias, DirectStableSelected)
  ] =
    value match
      case refinement @ Refinement(base, members) =>
        for
          _ <- Either.cond(
            members != null && members.size == 1 && members.head != null,
            (),
            error(
              "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
              "the exact result refinement requires one Type-alias member."
            )
          )
          refinedBase <- validateApplied(base, declarations)
          _ <- Either.cond(
            appliedKey(refinedBase) == appliedKey(contextualType),
            (),
            error(
              "CONTEXTUAL_TYPE_REFINEMENT_BASE_MISMATCH",
              "the refinement base must be the same scoped applied Type as the contextual parameter."
            )
          )
          member = members.head
          _ <- validateName(
            member.memberName,
            "REFINEMENT_MEMBER_NAME_INVALID",
            "the refinement Type-alias member name"
          )
          selected <- member.rhs match
            case value: DirectStableSelected => Right(value)
            case _ =>
              Left(
                error(
                  "REFINEMENT_MEMBER_RHS_UNSUPPORTED",
                  "the refinement alias RHS must be one direct stable selected Type."
                )
              )
          _ <- validateName(
            selected.memberExpectation,
            "STABLE_SELECTED_TYPE_MEMBER_EXPECTATION_INVALID",
            "the selected Type member expectation"
          )
          _ <- Either.cond(
            selected.prefixTermBinderId == contextualBinder,
            (),
            error(
              "STABLE_SELECTED_TYPE_PREFIX_UNBOUND",
              "the selected Type prefix must be the exact contextual Term binder."
            )
          )
          _ <- Either.cond(
            member.memberName == selected.memberExpectation,
            (),
            error(
              "REFINEMENT_MEMBER_NAME_MISMATCH",
              "the refinement alias name and selected member expectation must agree."
            )
          )
        yield (refinement, member, selected)
      case _ =>
        Left(
          error(
            "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
            "the exact result Type must contain one refinement member."
          )
        )

  private def appliedKey(value: Applied): (String, Vector[BinderId]) =
    val constructor = value.constructor match
      case SourceName(name) => name
      case _ => "<invalid>"
    val arguments = value.arguments.collect {
      case TypeParameterReference(binderId, _) => binderId
    }
    constructor -> arguments

  private def validateName(
      value: String,
      code: String,
      role: String
  ): Either[ScopedContextualMethodPlanError, Unit] =
    Either.cond(
      value != null && DefinitionName.fromSource(value).isRight,
      (),
      error(code, s"$role must be one legal Scala source identifier.")
    )

  private def error(
      code: String,
      detail: String
  ): ScopedContextualMethodPlanError =
    ScopedContextualMethodPlanError(code, detail)
