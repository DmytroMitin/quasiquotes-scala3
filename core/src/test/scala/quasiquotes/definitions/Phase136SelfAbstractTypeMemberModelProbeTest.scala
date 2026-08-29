package quasiquotes.definitions

class Phase136SelfAbstractTypeMemberModelProbeTest extends munit.FunSuite:
  private final case class ExternalStableAliasExpectation(value: String)
  private final case class SingletonLowerBound(
      alias: ExternalStableAliasExpectation
  )
  private final case class DirectExternalStableSelected(
      alias: ExternalStableAliasExpectation,
      memberName: String
  )
  private final case class SingleAliasUpperRefinement(
      baseName: String,
      aliasName: String,
      rhs: DirectExternalStableSelected
  )
  private final case class ObservedSelfMember(
      outerName: String,
      lowerAliasName: String,
      upperBaseName: String,
      refinementAliasName: String,
      selectedPrefixName: String,
      selectedMemberName: String
  )
  private final case class ExpectedSelfMember(
      memberName: String,
      selfAliasName: String,
      upperBaseName: String
  )
  private final case class SelfAbstractTypeMemberPlan private (
      memberName: String,
      selfAlias: ExternalStableAliasExpectation,
      lowerBound: SingletonLowerBound,
      upperBound: SingleAliasUpperRefinement
  )

  private object SelfAbstractTypeMemberPlan:
    def create(
        observed: ObservedSelfMember,
        expected: ExpectedSelfMember
    ): Either[String, SelfAbstractTypeMemberPlan] =
      for
        _ <- validateName(expected.memberName, "EXPECTED_MEMBER_NAME_INVALID")
        _ <- validateStableTermName(
          expected.selfAliasName,
          "EXPECTED_SELF_ALIAS_NAME_INVALID"
        )
        _ <- validateName(expected.upperBaseName, "EXPECTED_UPPER_BASE_NAME_INVALID")
        _ <- require(
          observed.outerName == expected.memberName,
          "OUTER_MEMBER_NAME_MISMATCH"
        )
        _ <- require(
          observed.lowerAliasName == expected.selfAliasName,
          "SINGLETON_LOWER_ALIAS_MISMATCH"
        )
        _ <- require(
          observed.upperBaseName == expected.upperBaseName,
          "UPPER_BASE_NAME_MISMATCH"
        )
        _ <- require(
          observed.refinementAliasName == observed.outerName,
          "REFINEMENT_ALIAS_NAME_MISMATCH"
        )
        _ <- require(
          observed.selectedPrefixName == observed.lowerAliasName,
          "SELECTED_PREFIX_ALIAS_MISMATCH"
        )
        _ <- require(
          observed.selectedMemberName == observed.outerName,
          "SELECTED_MEMBER_NAME_MISMATCH"
        )
        alias = ExternalStableAliasExpectation(expected.selfAliasName)
        selected = DirectExternalStableSelected(alias, expected.memberName)
      yield SelfAbstractTypeMemberPlan(
        expected.memberName,
        alias,
        SingletonLowerBound(alias),
        SingleAliasUpperRefinement(
          expected.upperBaseName,
          expected.memberName,
          selected
        )
      )

    private def validateName(value: String, code: String): Either[String, Unit] =
      Either.cond(
        value != null && DefinitionName.fromSource(value).isRight,
        (),
        code
      )

    private def validateStableTermName(
        value: String,
        code: String
    ): Either[String, Unit] =
      Either.cond(
        value != null && (
          DefinitionName.fromSource(value).isRight ||
            isPeerCollisionAlias(value)
        ),
        (),
        code
      )

    private def isPeerCollisionAlias(value: String): Boolean =
      val separator = value.lastIndexOf('$')
      if separator <= 0 || separator == value.length - 1 then false
      else
        val base = value.substring(0, separator)
        val suffix = value.substring(separator + 1)
        DefinitionName.plain(base).isRight &&
          suffix.forall(_.isDigit) &&
          suffix.head != '0'

    private def require(condition: Boolean, code: String): Either[String, Unit] =
      Either.cond(condition, (), code)

  test("models the exact member with one external stable-name expectation and no BinderId") {
    val expected = ExpectedSelfMember("Self", "self", "Nat")
    val plan = SelfAbstractTypeMemberPlan
      .create(observed("Self", "self", "Nat"), expected)
      .fold(problem => fail(problem), identity)

    assertEquals(plan.memberName, "Self")
    assertEquals(plan.selfAlias, ExternalStableAliasExpectation("self"))
    assertEquals(plan.lowerBound.alias, plan.selfAlias)
    assertEquals(plan.upperBound.baseName, "Nat")
    assertEquals(plan.upperBound.aliasName, plan.memberName)
    assertEquals(plan.upperBound.rhs.alias, plan.selfAlias)
    assertEquals(plan.upperBound.rhs.memberName, plan.memberName)
    assert(!plan.productIterator.exists(_.isInstanceOf[quasiquotes.parser.BinderId]))
  }

  test("fully renamed legal names retain the same coherence graph") {
    val expected = ExpectedSelfMember("Element", "owner$2", "Domain")
    val plan = SelfAbstractTypeMemberPlan
      .create(observed("Element", "owner$2", "Domain"), expected)
      .fold(problem => fail(problem), identity)

    assertEquals(plan.lowerBound.alias, plan.upperBound.rhs.alias)
    assertEquals(plan.memberName, plan.upperBound.aliasName)
    assertEquals(plan.memberName, plan.upperBound.rhs.memberName)
  }

  test("rejects each detached external-name or cross-reference edge deterministically") {
    val expected = ExpectedSelfMember("Self", "self", "Nat")
    assertRejected(
      observed("Self", "self", "Nat"),
      ExpectedSelfMember("Self", "type", "Nat"),
      "EXPECTED_SELF_ALIAS_NAME_INVALID"
    )
    assertRejected(
      observed("Self", "self$0", "Nat"),
      ExpectedSelfMember("Self", "self$0", "Nat"),
      "EXPECTED_SELF_ALIAS_NAME_INVALID"
    )
    assertRejected(
      observed("Self", "self$$", "Nat"),
      ExpectedSelfMember("Self", "self$$", "Nat"),
      "EXPECTED_SELF_ALIAS_NAME_INVALID"
    )
    assertRejected(observed("Other", "self", "Nat"), expected, "OUTER_MEMBER_NAME_MISMATCH")
    assertRejected(observed("Self", "other", "Nat"), expected, "SINGLETON_LOWER_ALIAS_MISMATCH")
    assertRejected(observed("Self", "self", "Other"), expected, "UPPER_BASE_NAME_MISMATCH")
    assertRejected(
      observed("Self", "self", "Nat").copy(refinementAliasName = "Other"),
      expected,
      "REFINEMENT_ALIAS_NAME_MISMATCH"
    )
    assertRejected(
      observed("Self", "self", "Nat").copy(selectedPrefixName = "other"),
      expected,
      "SELECTED_PREFIX_ALIAS_MISMATCH"
    )
    assertRejected(
      observed("Self", "self", "Nat").copy(selectedMemberName = "Other"),
      expected,
      "SELECTED_MEMBER_NAME_MISMATCH"
    )
  }

  private def observed(
      member: String,
      selfAlias: String,
      upperBase: String
  ): ObservedSelfMember =
    ObservedSelfMember(member, selfAlias, upperBase, member, selfAlias, member)

  private def assertRejected(
      observed: ObservedSelfMember,
      expected: ExpectedSelfMember,
      code: String
  ): Unit =
    assertEquals(
      SelfAbstractTypeMemberPlan.create(observed, expected).left.toOption,
      Some(code)
    )
