package quasiquotes.definitions

import scala.util.control.NonFatal

private[quasiquotes] final case class SelfAbstractTypeMemberPlanError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"

private[quasiquotes] final case class SelfAbstractTypeMemberExpectation(
    memberName: String,
    selfAliasName: String,
    upperBaseName: String
) derives CanEqual

private[quasiquotes] final case class ObservedSelfAbstractTypeMember(
    outerMemberName: String,
    lowerAliasName: String,
    upperBaseName: String,
    refinementAliasName: String,
    selectedPrefixName: String,
    selectedMemberName: String
) derives CanEqual

private[quasiquotes] final case class ObservedSelfAbstractTypeMemberRefinement(
    aliasName: String,
    selectedPrefixName: String,
    selectedMemberName: String
) derives CanEqual

private[quasiquotes] final case class ObservedSelfAbstractTypeMemberMatrix(
    outerMemberName: String,
    lowerAliasName: Option[String],
    upperBaseName: String,
    refinement: Option[ObservedSelfAbstractTypeMemberRefinement]
) derives CanEqual

private[quasiquotes] final case class ExternalStableAliasExpectation private[definitions] (
    source: String
)
    derives CanEqual

private[quasiquotes] final case class SingletonLowerBound(
    alias: ExternalStableAliasExpectation
) derives CanEqual

private[quasiquotes] final case class DirectExternalStableSelected(
    alias: ExternalStableAliasExpectation,
    memberName: String
) derives CanEqual

private[quasiquotes] final case class SingleAliasUpperRefinement(
    baseName: String,
    aliasName: String,
    rhs: DirectExternalStableSelected
) derives CanEqual

private[quasiquotes] final case class SelfAbstractTypeMemberRoleSnapshot(
    memberName: String,
    selfAliasName: String,
    upperBaseName: String,
    lowerPresent: Boolean,
    fBoundPresent: Boolean,
    lowerAliasRole: Option[String],
    refinementBaseRole: Option[String],
    refinementAliasRole: Option[String],
    selectedPrefixRole: Option[String],
    selectedMemberRole: Option[String]
) derives CanEqual

/** Compiler-free validated carrier for the closed AUXify-046/086 member family.
  *
  * The prepared self alias is an external source-name expectation. It is not
  * represented by a project binder identity or a compiler symbol.
  *
  * `lowerBound` and `upperBound` remain the legacy both-present
  * compatibility view used by the current U backend. They never invent an
  * absent edge.
  */
private[quasiquotes] final class SelfAbstractTypeMemberPlan private (
    val memberName: String,
    val selfAlias: ExternalStableAliasExpectation,
    val upperBaseName: String,
    val lowerBoundOption: Option[SingletonLowerBound],
    val fBoundRefinementOption: Option[SingleAliasUpperRefinement]
):
  def lowerBound: SingletonLowerBound =
    lowerBoundOption.getOrElse(
      throw new IllegalStateException(
        "SELF_MEMBER_LEGACY_LOWER_BOUND_ABSENT: the legacy both-present lower-bound view cannot represent this matrix row."
      )
    )

  def upperBound: SingleAliasUpperRefinement =
    fBoundRefinementOption.getOrElse(
      throw new IllegalStateException(
        "SELF_MEMBER_LEGACY_UPPER_REFINEMENT_ABSENT: the legacy both-present upper-refinement view cannot represent this matrix row."
      )
    )

  def roleSnapshot: SelfAbstractTypeMemberRoleSnapshot =
    SelfAbstractTypeMemberRoleSnapshot(
      memberName,
      selfAlias.source,
      upperBaseName,
      lowerBoundOption.nonEmpty,
      fBoundRefinementOption.nonEmpty,
      lowerBoundOption.map(_.alias.source),
      fBoundRefinementOption.map(_.baseName),
      fBoundRefinementOption.map(_.aliasName),
      fBoundRefinementOption.map(_.rhs.alias.source),
      fBoundRefinementOption.map(_.rhs.memberName)
    )

  def productIterator: Iterator[Any] =
    Iterator(
      memberName,
      selfAlias,
      upperBaseName,
      lowerBoundOption,
      fBoundRefinementOption
    )

private[quasiquotes] object SelfAbstractTypeMemberPlan:
  def validateExpectation(
      expected: SelfAbstractTypeMemberExpectation
  ): Either[SelfAbstractTypeMemberPlanError, Unit] =
    for
      present <- Option(expected).toRight(
        error(
          "EXPECTED_SELF_MEMBER_MISSING",
          "the expected self abstract-Type-member names must be present."
        )
      )
      _ <- validateDefinitionName(
        present.memberName,
        "EXPECTED_MEMBER_NAME_INVALID",
        "the expected member name"
      )
      _ <- validateExternalAlias(present.selfAliasName)
      _ <- validateDefinitionName(
        present.upperBaseName,
        "EXPECTED_UPPER_BASE_NAME_INVALID",
        "the expected upper-base name"
      )
    yield ()

  def create(
      observed: ObservedSelfAbstractTypeMember,
      expected: SelfAbstractTypeMemberExpectation
  ): Either[SelfAbstractTypeMemberPlanError, SelfAbstractTypeMemberPlan] =
    for
      presentObserved <- Option(observed).toRight(
        error(
          "OBSERVED_SELF_MEMBER_MISSING",
          "the observed self abstract-Type-member edges must be present."
        )
      )
      presentExpected <- Option(expected).toRight(
        error(
          "EXPECTED_SELF_MEMBER_MISSING",
          "the expected self abstract-Type-member names must be present."
        )
      )
      _ <- validateExpectation(presentExpected)
      alias <- validateExternalAlias(presentExpected.selfAliasName)
      _ <- require(
        presentObserved.outerMemberName == presentExpected.memberName,
        "OUTER_MEMBER_NAME_MISMATCH",
        "the outer member name must equal the explicit member expectation."
      )
      _ <- require(
        presentObserved.lowerAliasName == presentExpected.selfAliasName,
        "SINGLETON_LOWER_ALIAS_MISMATCH",
        "the singleton lower-bound alias must equal the prepared self-alias expectation."
      )
      _ <- require(
        presentObserved.upperBaseName == presentExpected.upperBaseName,
        "UPPER_BASE_NAME_MISMATCH",
        "the upper refinement base must equal the explicit upper-base expectation."
      )
      _ <- require(
        presentObserved.refinementAliasName == presentObserved.outerMemberName,
        "REFINEMENT_ALIAS_NAME_MISMATCH",
        "the refinement alias must equal the outer member name."
      )
      _ <- require(
        presentObserved.selectedPrefixName == presentObserved.lowerAliasName,
        "SELECTED_PREFIX_ALIAS_MISMATCH",
        "the selected-Type prefix must equal the singleton lower-bound alias."
      )
      _ <- require(
        presentObserved.selectedMemberName == presentObserved.outerMemberName,
        "SELECTED_MEMBER_NAME_MISMATCH",
        "the selected Type member must equal the outer member name."
      )
      selected = DirectExternalStableSelected(alias, presentExpected.memberName)
    yield new SelfAbstractTypeMemberPlan(
      presentExpected.memberName,
      alias,
      presentExpected.upperBaseName,
      Some(SingletonLowerBound(alias)),
      Some(
        SingleAliasUpperRefinement(
          presentExpected.upperBaseName,
          presentExpected.memberName,
          selected
        )
      )
    )

  def createMatrix(
      observed: ObservedSelfAbstractTypeMemberMatrix,
      expected: SelfAbstractTypeMemberExpectation
  ): Either[SelfAbstractTypeMemberPlanError, SelfAbstractTypeMemberPlan] =
    for
      presentObserved <- Option(observed).toRight(
        error(
          "OBSERVED_SELF_MEMBER_MISSING",
          "the observed self abstract-Type-member matrix must be present."
        )
      )
      presentExpected <- Option(expected).toRight(
        error(
          "EXPECTED_SELF_MEMBER_MISSING",
          "the expected self abstract-Type-member names must be present."
        )
      )
      _ <- validateExpectation(presentExpected)
      alias <- validateExternalAlias(presentExpected.selfAliasName)
      lowerAliasOption <- Option(presentObserved.lowerAliasName).toRight(
        error(
          "OBSERVED_SELF_MEMBER_MATRIX_INVALID",
          "the observed lower-bound option must be present as an Option."
        )
      )
      refinementOption <- Option(presentObserved.refinement).toRight(
        error(
          "OBSERVED_SELF_MEMBER_MATRIX_INVALID",
          "the observed refinement option must be present as an Option."
        )
      )
      _ <- require(
        presentObserved.outerMemberName == presentExpected.memberName,
        "OUTER_MEMBER_NAME_MISMATCH",
        "the outer member name must equal the explicit member expectation."
      )
      _ <- lowerAliasOption match
        case Some(lowerAlias) =>
          require(
            lowerAlias == presentExpected.selfAliasName,
            "SINGLETON_LOWER_ALIAS_MISMATCH",
            "the singleton lower-bound alias must equal the prepared self-alias expectation."
          )
        case None => Right(())
      _ <- require(
        presentObserved.upperBaseName == presentExpected.upperBaseName,
        "UPPER_BASE_NAME_MISMATCH",
        "the upper base must equal the explicit upper-base expectation."
      )
      _ <- refinementOption match
        case Some(refinement) => validateRefinement(refinement, presentExpected)
        case None => Right(())
      selected = DirectExternalStableSelected(alias, presentExpected.memberName)
      lower = lowerAliasOption.map(_ => SingletonLowerBound(alias))
      refinement = refinementOption.map(_ =>
        SingleAliasUpperRefinement(
          presentExpected.upperBaseName,
          presentExpected.memberName,
          selected
        )
      )
    yield new SelfAbstractTypeMemberPlan(
      presentExpected.memberName,
      alias,
      presentExpected.upperBaseName,
      lower,
      refinement
    )

  def validate(
      plan: SelfAbstractTypeMemberPlan
  ): Either[SelfAbstractTypeMemberPlanError, Unit] =
    Option(plan)
      .toRight(
        error(
          "SELF_MEMBER_PLAN_MISSING",
          "the self abstract-Type-member plan must be present."
        )
      )
      .flatMap { present =>
        try
          val lowerOption = Option(present.lowerBoundOption).toRight(
            error(
              "SELF_MEMBER_PLAN_INVALID",
              "the lower-bound option must be present as an Option."
            )
          )
          val refinementOption = Option(present.fBoundRefinementOption).toRight(
            error(
              "SELF_MEMBER_PLAN_INVALID",
              "the refinement option must be present as an Option."
            )
          )
          for
            lower <- lowerOption
            refinement <- refinementOption
            expected = SelfAbstractTypeMemberExpectation(
              present.memberName,
              present.selfAlias.source,
              present.upperBaseName
            )
            observed = ObservedSelfAbstractTypeMemberMatrix(
              present.memberName,
              lower.map(_.alias.source),
              present.upperBaseName,
              refinement.map(value =>
                ObservedSelfAbstractTypeMemberRefinement(
                  value.aliasName,
                  value.rhs.alias.source,
                  value.rhs.memberName
                )
              )
            )
            recreated <- createMatrix(observed, expected)
            _ <- require(
              recreated.roleSnapshot == present.roleSnapshot,
              "SELF_MEMBER_PLAN_INVALID",
              "the plan does not preserve the complete closed-matrix role snapshot."
            )
          yield ()
        catch
          case NonFatal(_) =>
            Left(
              error(
                "SELF_MEMBER_PLAN_INVALID",
                "the plan contains a corrupt or unsupported closed-matrix state."
              )
            )
      }

  private def validateRefinement(
      refinement: ObservedSelfAbstractTypeMemberRefinement,
      expected: SelfAbstractTypeMemberExpectation
  ): Either[SelfAbstractTypeMemberPlanError, Unit] =
    for
      present <- Option(refinement).toRight(
        error(
          "OBSERVED_SELF_MEMBER_REFINEMENT_MISSING",
          "the present refinement observation must be non-null."
        )
      )
      _ <- require(
        present.aliasName == expected.memberName,
        "REFINEMENT_ALIAS_NAME_MISMATCH",
        "the refinement alias must equal the outer member name."
      )
      _ <- require(
        present.selectedPrefixName == expected.selfAliasName,
        "SELECTED_PREFIX_ALIAS_MISMATCH",
        "the selected-Type prefix must equal the prepared self-alias expectation."
      )
      _ <- require(
        present.selectedMemberName == expected.memberName,
        "SELECTED_MEMBER_NAME_MISMATCH",
        "the selected Type member must equal the outer member name."
      )
    yield ()

  private def validateDefinitionName(
      value: String,
      code: String,
      role: String
  ): Either[SelfAbstractTypeMemberPlanError, Unit] =
    Option(value)
      .toRight(error(code, s"$role must be present."))
      .flatMap(name =>
        DefinitionName
          .fromSource(name)
          .left
          .map(problem => error(code, s"$role is invalid: ${problem.message}"))
          .map(_ => ())
      )

  private def validateExternalAlias(
      value: String
  ): Either[SelfAbstractTypeMemberPlanError, ExternalStableAliasExpectation] =
    Option(value)
      .filter(name => DefinitionName.plain(name).isRight || isPeerCollisionAlias(name))
      .map(new ExternalStableAliasExpectation(_))
      .toRight(
        error(
          "EXPECTED_SELF_ALIAS_NAME_INVALID",
          "the expected self alias must be one plain stable Term name or a plain base followed by `$N` for a positive decimal N without leading zero."
        )
      )

  private def isPeerCollisionAlias(value: String): Boolean =
    val separator = value.lastIndexOf('$')
    if separator <= 0 || separator == value.length - 1 then false
    else
      val base = value.substring(0, separator)
      val suffix = value.substring(separator + 1)
      DefinitionName.plain(base).isRight &&
        suffix.nonEmpty &&
        suffix.forall(char => char >= '0' && char <= '9') &&
        suffix.head != '0'

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[SelfAbstractTypeMemberPlanError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(
      code: String,
      detail: String
  ): SelfAbstractTypeMemberPlanError =
    SelfAbstractTypeMemberPlanError(code, detail)
