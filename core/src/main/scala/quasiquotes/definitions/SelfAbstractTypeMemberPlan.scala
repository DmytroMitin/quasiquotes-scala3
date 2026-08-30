package quasiquotes.definitions

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

/** Compiler-free validated carrier for the exact AUXify-046 member family.
  *
  * The prepared self alias is an external source-name expectation. It is not
  * represented by a project binder identity or a compiler symbol.
  */
private[quasiquotes] final class SelfAbstractTypeMemberPlan private (
    val memberName: String,
    val selfAlias: ExternalStableAliasExpectation,
    val lowerBound: SingletonLowerBound,
    val upperBound: SingleAliasUpperRefinement
):
  def productIterator: Iterator[Any] =
    Iterator(memberName, selfAlias, lowerBound, upperBound)

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
      SingletonLowerBound(alias),
      SingleAliasUpperRefinement(
        presentExpected.upperBaseName,
        presentExpected.memberName,
        selected
      )
    )

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
