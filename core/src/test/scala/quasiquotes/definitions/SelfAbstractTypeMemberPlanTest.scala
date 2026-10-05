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

  test("models the closed lower and F-bound option matrix with stable distinct role snapshots") {
    val expected = SelfAbstractTypeMemberExpectation("Element", "owner$2", "Domain")
    val rows = Vector((true, true), (true, false), (false, true), (false, false))

    val snapshots = rows.map { case (lowerPresent, fBoundPresent) =>
      val first = SelfAbstractTypeMemberPlan
        .createMatrix(
          observedMatrix("Element", "owner$2", "Domain", lowerPresent, fBoundPresent),
          expected
        )
        .fold(problem => fail(problem.message), identity)
      val repeated = SelfAbstractTypeMemberPlan
        .createMatrix(
          observedMatrix("Element", "owner$2", "Domain", lowerPresent, fBoundPresent),
          expected
        )
        .fold(problem => fail(problem.message), identity)

      assertEquals(first.memberName, "Element")
      assertEquals(first.selfAlias.source, "owner$2")
      assertEquals(first.upperBaseName, "Domain")
      assertEquals(first.lowerBoundOption.nonEmpty, lowerPresent)
      assertEquals(first.fBoundRefinementOption.nonEmpty, fBoundPresent)
      assertEquals(first.roleSnapshot, repeated.roleSnapshot)
      assertEquals(first.roleSnapshot.lowerPresent, lowerPresent)
      assertEquals(first.roleSnapshot.fBoundPresent, fBoundPresent)
      assertEquals(first.roleSnapshot.lowerAliasRole, Option.when(lowerPresent)("owner$2"))
      assertEquals(first.roleSnapshot.refinementBaseRole, Option.when(fBoundPresent)("Domain"))
      assertEquals(first.roleSnapshot.refinementAliasRole, Option.when(fBoundPresent)("Element"))
      assertEquals(first.roleSnapshot.selectedPrefixRole, Option.when(fBoundPresent)("owner$2"))
      assertEquals(first.roleSnapshot.selectedMemberRole, Option.when(fBoundPresent)("Element"))
      first.roleSnapshot
    }

    assertEquals(snapshots.distinct.size, 4)
  }

  test("matrix validation rejects incoherent present edges without fabricating absent edges") {
    val expected = SelfAbstractTypeMemberExpectation("Self", "self", "Nat")
    assertMatrixRejected(
      observedMatrix("Other", "self", "Nat", lowerPresent = false, fBoundPresent = false),
      expected,
      "OUTER_MEMBER_NAME_MISMATCH"
    )
    assertMatrixRejected(
      observedMatrix("Self", "other", "Nat", lowerPresent = true, fBoundPresent = false),
      expected,
      "SINGLETON_LOWER_ALIAS_MISMATCH"
    )
    assertMatrixRejected(
      observedMatrix("Self", "self", "Other", lowerPresent = false, fBoundPresent = false),
      expected,
      "UPPER_BASE_NAME_MISMATCH"
    )
    assertMatrixRejected(
      observedMatrix("Self", "self", "Nat", lowerPresent = false, fBoundPresent = true)
        .copy(refinement = Some(ObservedSelfAbstractTypeMemberRefinement("Other", "self", "Self"))),
      expected,
      "REFINEMENT_ALIAS_NAME_MISMATCH"
    )
    assertMatrixRejected(
      observedMatrix("Self", "self", "Nat", lowerPresent = false, fBoundPresent = true)
        .copy(refinement = Some(ObservedSelfAbstractTypeMemberRefinement("Self", "other", "Self"))),
      expected,
      "SELECTED_PREFIX_ALIAS_MISMATCH"
    )
    assertMatrixRejected(
      observedMatrix("Self", "self", "Nat", lowerPresent = false, fBoundPresent = true)
        .copy(refinement = Some(ObservedSelfAbstractTypeMemberRefinement("Self", "self", "Other"))),
      expected,
      "SELECTED_MEMBER_NAME_MISMATCH"
    )
  }

  test("legacy both-present accessors fail deterministically for absent matrix edges") {
    val expected = SelfAbstractTypeMemberExpectation("Self", "self", "Nat")
    val lowerOnly = SelfAbstractTypeMemberPlan
      .createMatrix(
        observedMatrix("Self", "self", "Nat", lowerPresent = true, fBoundPresent = false),
        expected
      )
      .fold(problem => fail(problem.message), identity)
    val fBoundOnly = SelfAbstractTypeMemberPlan
      .createMatrix(
        observedMatrix("Self", "self", "Nat", lowerPresent = false, fBoundPresent = true),
        expected
      )
      .fold(problem => fail(problem.message), identity)

    assert(SelfAbstractTypeMemberPlan.validate(lowerOnly).isRight)
    assert(SelfAbstractTypeMemberPlan.validate(fBoundOnly).isRight)
    assert(
      intercept[IllegalStateException](lowerOnly.upperBound)
        .getMessage
        .startsWith("SELF_MEMBER_LEGACY_UPPER_REFINEMENT_ABSENT")
    )
    assert(
      intercept[IllegalStateException](fBoundOnly.lowerBound)
        .getMessage
        .startsWith("SELF_MEMBER_LEGACY_LOWER_BOUND_ABSENT")
    )
  }

  test("complete validation rejects null and forged corrupt option storage") {
    assertEquals(
      SelfAbstractTypeMemberPlan.validate(null).left.toOption.map(_.code),
      Some("SELF_MEMBER_PLAN_MISSING")
    )

    val constructor = classOf[SelfAbstractTypeMemberPlan]
      .getDeclaredConstructors
      .find(_.getParameterCount == 5)
      .getOrElse(fail("expected the closed-matrix plan constructor"))
    constructor.setAccessible(true)
    val corrupt = constructor
      .newInstance(
        "Self",
        new ExternalStableAliasExpectation("self"),
        "Nat",
        null,
        None
      )
      .asInstanceOf[SelfAbstractTypeMemberPlan]

    assertEquals(
      SelfAbstractTypeMemberPlan.validate(corrupt).left.toOption.map(_.code),
      Some("SELF_MEMBER_PLAN_INVALID")
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

  private def observedMatrix(
      member: String,
      selfAlias: String,
      upperBase: String,
      lowerPresent: Boolean,
      fBoundPresent: Boolean
  ): ObservedSelfAbstractTypeMemberMatrix =
    ObservedSelfAbstractTypeMemberMatrix(
      member,
      Option.when(lowerPresent)(selfAlias),
      upperBase,
      Option.when(fBoundPresent)(
        ObservedSelfAbstractTypeMemberRefinement(member, selfAlias, member)
      )
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

  private def assertMatrixRejected(
      observed: ObservedSelfAbstractTypeMemberMatrix,
      expected: SelfAbstractTypeMemberExpectation,
      code: String
  ): Unit =
    assertEquals(
      SelfAbstractTypeMemberPlan
        .createMatrix(observed, expected)
        .left
        .toOption
        .map(_.code),
      Some(code)
    )
