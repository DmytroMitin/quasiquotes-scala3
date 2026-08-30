package quasiquotes.definitions

class SelfAbstractTypeMemberPlanTest extends munit.FunSuite:
  test("models the exact member with one external stable-name expectation and no BinderId") {
    val expected = SelfAbstractTypeMemberExpectation("Self", "self", "Nat")
    val plan = SelfAbstractTypeMemberPlan
      .create(observed("Self", "self", "Nat"), expected)
      .fold(problem => fail(problem.message), identity)

    assertEquals(plan.memberName, "Self")
    assertEquals(plan.selfAlias.source, "self")
    assertEquals(plan.lowerBound.alias, plan.selfAlias)
    assertEquals(plan.upperBound.baseName, "Nat")
    assertEquals(plan.upperBound.aliasName, plan.memberName)
    assertEquals(plan.upperBound.rhs.alias, plan.selfAlias)
    assertEquals(plan.upperBound.rhs.memberName, plan.memberName)
    assert(!plan.productIterator.exists(_.isInstanceOf[quasiquotes.parser.BinderId]))
  }

  test("fully renamed legal names retain the same coherence graph") {
    val expected = SelfAbstractTypeMemberExpectation("Element", "owner$2", "Domain")
    val plan = SelfAbstractTypeMemberPlan
      .create(observed("Element", "owner$2", "Domain"), expected)
      .fold(problem => fail(problem.message), identity)

    assertEquals(plan.lowerBound.alias, plan.upperBound.rhs.alias)
    assertEquals(plan.memberName, plan.upperBound.aliasName)
    assertEquals(plan.memberName, plan.upperBound.rhs.memberName)
  }

  test("rejects each detached external-name or cross-reference edge deterministically") {
    val expected = SelfAbstractTypeMemberExpectation("Self", "self", "Nat")
    assertRejected(
      observed("Self", "self", "Nat"),
      SelfAbstractTypeMemberExpectation("Self", "type", "Nat"),
      "EXPECTED_SELF_ALIAS_NAME_INVALID"
    )
    assertRejected(
      observed("Self", "self$0", "Nat"),
      SelfAbstractTypeMemberExpectation("Self", "self$0", "Nat"),
      "EXPECTED_SELF_ALIAS_NAME_INVALID"
    )
    assertRejected(
      observed("Self", "self$$", "Nat"),
      SelfAbstractTypeMemberExpectation("Self", "self$$", "Nat"),
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

  test("admits only plain stable aliases and the positive collision suffix sequence") {
    Vector("self", "self$1", "self$2", "owner$938").foreach { alias =>
      val plan = SelfAbstractTypeMemberPlan
        .create(
          observed("Self", alias, "Nat"),
          SelfAbstractTypeMemberExpectation("Self", alias, "Nat")
        )
        .fold(problem => fail(problem.message), identity)
      assertEquals(plan.selfAlias.source, alias)
    }

    Vector(
      "self$0",
      "self$01",
      "self$",
      "self$$",
      "type",
      "`type`",
      "self\n",
      "owner.self",
      "self()",
      "this.self",
      "var self",
      "self_=",
      "$anon"
    ).foreach { alias =>
      assertRejected(
        observed("Self", alias, "Nat"),
        SelfAbstractTypeMemberExpectation("Self", alias, "Nat"),
        "EXPECTED_SELF_ALIAS_NAME_INVALID"
      )
    }
  }

  test("invalid member and upper-base source names fail before coherence checks") {
    assertRejected(
      observed("bad-name", "self", "Nat"),
      SelfAbstractTypeMemberExpectation("bad-name", "self", "Nat"),
      "EXPECTED_MEMBER_NAME_INVALID"
    )
    assertRejected(
      observed("Self", "self", "bad-name"),
      SelfAbstractTypeMemberExpectation("Self", "self", "bad-name"),
      "EXPECTED_UPPER_BASE_NAME_INVALID"
    )
  }

  private def observed(
      member: String,
      selfAlias: String,
      upperBase: String
  ): ObservedSelfAbstractTypeMember =
    ObservedSelfAbstractTypeMember(
      member,
      selfAlias,
      upperBase,
      member,
      selfAlias,
      member
    )

  private def assertRejected(
      observed: ObservedSelfAbstractTypeMember,
      expected: SelfAbstractTypeMemberExpectation,
      code: String
  ): Unit =
    assertEquals(
      SelfAbstractTypeMemberPlan.create(observed, expected).left.toOption.map(_.code),
      Some(code)
    )
