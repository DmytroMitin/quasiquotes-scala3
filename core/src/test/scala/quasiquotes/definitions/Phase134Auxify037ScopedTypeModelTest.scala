package quasiquotes.definitions

import quasiquotes.parser.BinderId

class Phase134Auxify037ScopedTypeModelTest extends munit.FunSuite:
  import ScopedType.*

  test("alpha-renaming preserves the full scoped method key") {
    val original = validPlan(
      BinderId(3),
      "N",
      BinderId(8),
      "M",
      BinderId(13),
      "inst"
    )
    val renamed = validPlan(
      BinderId(21),
      "Left",
      BinderId(34),
      "Right",
      BinderId(55),
      "evidence"
    )

    assertEquals(original.alphaKey, renamed.alphaKey)
    assertEquals(original.typeArgumentBinderPositions, Vector(0, 1))
    assertEquals(renamed.typeArgumentBinderPositions, Vector(0, 1))
  }

  test("reversing the applied Type binders is rejected rather than name-substituted") {
    val first = ScopedTypeParameter(BinderId(1), "N", SourceName("Nat"))
    val second = ScopedTypeParameter(BinderId(2), "M", SourceName("Nat"))
    val contextualBinder = BinderId(3)
    val reversed = Applied(
      SourceName("Add"),
      Vector(
        TypeParameterReference(second.binderId, second.displayName),
        TypeParameterReference(first.binderId, first.displayName)
      )
    )

    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        reversed,
        refinement(reversed, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH")
    )
  }

  test("duplicate display spelling fails after distinct binder identities remain observable") {
    val first = ScopedTypeParameter(BinderId(1), "T", SourceName("Nat"))
    val second = ScopedTypeParameter(BinderId(2), "T", SourceName("Nat"))
    val contextualBinder = BinderId(3)
    val applied = application(first, second)

    assertNotEquals(first.binderId, second.binderId)
    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        applied,
        refinement(applied, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT")
    )
  }

  test("upper bounds are scoped Type nodes and unsupported bound shapes fail deterministically") {
    val first = ScopedTypeParameter(
      BinderId(1),
      "N",
      Applied(SourceName("Nat"), Vector.empty)
    )
    val second = ScopedTypeParameter(BinderId(2), "M", SourceName("Nat"))
    val contextualBinder = BinderId(3)
    val applied = application(first, second)

    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        applied,
        refinement(applied, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED")
    )
  }

  test("exact arity and cross-category binder distinctness fail with separate categories") {
    val first = ScopedTypeParameter(BinderId(1), "N", SourceName("Nat"))
    val second = ScopedTypeParameter(BinderId(2), "M", SourceName("Nat"))
    val applied = application(first, second)

    assertEquals(
      create(
        Vector(first),
        BinderId(3),
        "inst",
        applied,
        refinement(applied, BinderId(3)),
        BinderId(3)
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_ARITY_UNSUPPORTED")
    )
    assertEquals(
      create(
        Vector(first, second.copy(binderId = first.binderId)),
        BinderId(3),
        "inst",
        applied,
        refinement(applied, BinderId(3)),
        BinderId(3)
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT")
    )
    assertEquals(
      create(
        Vector(first, second),
        first.binderId,
        "inst",
        applied,
        refinement(applied, first.binderId),
        first.binderId
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT")
    )
  }

  test("the exact refinement reuses the applied base and contextual Term binder") {
    val target = validPlan(
      BinderId(5),
      "N",
      BinderId(6),
      "M",
      BinderId(7),
      "inst"
    )

    assertEquals(target.refinementMember.memberName, "Out")
    assertEquals(target.selectedResult.prefixTermBinderId, BinderId(7))
    assertEquals(target.selectedResult.memberExpectation, "Out")
    assertEquals(target.bodyTermBinderId, BinderId(7))
    assertEquals(target.contextualType, target.resultType.base)
  }

  test("unbound Type references and detached selected prefixes remain distinct failures") {
    val first = ScopedTypeParameter(BinderId(5), "N", SourceName("Nat"))
    val second = ScopedTypeParameter(BinderId(6), "M", SourceName("Nat"))
    val contextualBinder = BinderId(7)
    val unbound = Applied(
      SourceName("Add"),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(BinderId(99), second.displayName)
      )
    )
    val repeated = Applied(
      SourceName("Add"),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(first.binderId, first.displayName)
      )
    )
    val displayDrift = Applied(
      SourceName("Add"),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(second.binderId, "Other")
      )
    )
    val validApplied = application(first, second)

    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        unbound,
        refinement(unbound, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_REFERENCE_UNBOUND")
    )
    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        repeated,
        refinement(repeated, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH")
    )
    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        displayDrift,
        refinement(displayDrift, contextualBinder),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("TYPE_PARAMETER_REFERENCE_DISPLAY_NAME_MISMATCH")
    )
    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        validApplied,
        refinement(validApplied, BinderId(99)),
        contextualBinder
      ).left.toOption.map(_.code),
      Some("STABLE_SELECTED_TYPE_PREFIX_UNBOUND")
    )
  }

  test("refinement count, member expectation, base, and body mismatches have stable categories") {
    val first = ScopedTypeParameter(BinderId(5), "N", SourceName("Nat"))
    val second = ScopedTypeParameter(BinderId(6), "M", SourceName("Nat"))
    val contextualBinder = BinderId(7)
    val applied = application(first, second)
    val otherBase = applied.copy(constructor = SourceName("Other"))

    val cases = List(
      Refinement(applied, Vector.empty) -> "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      Refinement(
        applied,
        Vector(
          ScopedTypeAlias("Out", DirectStableSelected(contextualBinder, "Out")),
          ScopedTypeAlias("Other", DirectStableSelected(contextualBinder, "Other"))
        )
      ) -> "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      Refinement(
        applied,
        Vector(ScopedTypeAlias("Out", DirectStableSelected(contextualBinder, "type")))
      ) -> "STABLE_SELECTED_TYPE_MEMBER_EXPECTATION_INVALID",
      Refinement(
        applied,
        Vector(ScopedTypeAlias("Out", DirectStableSelected(contextualBinder, "Result")))
      ) -> "REFINEMENT_MEMBER_NAME_MISMATCH",
      refinement(otherBase, contextualBinder) -> "CONTEXTUAL_TYPE_REFINEMENT_BASE_MISMATCH"
    )

    cases.foreach { case (resultType, expectedCode) =>
      assertEquals(
        create(
          Vector(first, second),
          contextualBinder,
          "inst",
          applied,
          resultType,
          contextualBinder
        ).left.toOption.map(_.code),
        Some(expectedCode)
      )
    }
    assertEquals(
      create(
        Vector(first, second),
        contextualBinder,
        "inst",
        applied,
        refinement(applied, contextualBinder),
        BinderId(99)
      ).left.toOption.map(_.code),
      Some("CONTEXTUAL_BODY_BINDER_MISMATCH")
    )
  }

  private def validPlan(
      firstBinder: BinderId,
      firstName: String,
      secondBinder: BinderId,
      secondName: String,
      contextualBinder: BinderId,
      contextualName: String
  ): ScopedContextualMethodPlan =
    val first = ScopedTypeParameter(firstBinder, firstName, SourceName("Nat"))
    val second = ScopedTypeParameter(secondBinder, secondName, SourceName("Nat"))
    val applied = application(first, second)
    create(
      Vector(first, second),
      contextualBinder,
      contextualName,
      applied,
      refinement(applied, contextualBinder),
      contextualBinder
    ).fold(error => fail(error.message), identity)

  private def create(
      typeParameters: Vector[ScopedTypeParameter],
      contextualBinder: BinderId,
      contextualName: String,
      contextualType: ScopedType,
      resultType: ScopedType,
      bodyBinder: BinderId
  ): Either[ScopedContextualMethodPlanError, ScopedContextualMethodPlan] =
    ScopedContextualMethodPlan.create(
      methodDisplayName = "apply",
      typeParameters = typeParameters,
      contextualTermBinderId = contextualBinder,
      contextualDisplayName = contextualName,
      contextualType = contextualType,
      resultType = resultType,
      bodyTermBinderId = bodyBinder
    )

  private def application(
      first: ScopedTypeParameter,
      second: ScopedTypeParameter
  ): Applied =
    Applied(
      SourceName("Add"),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(second.binderId, second.displayName)
      )
    )

  private def refinement(
      base: ScopedType,
      prefix: BinderId
  ): Refinement =
    Refinement(
      base,
      Vector(ScopedTypeAlias("Out", DirectStableSelected(prefix, "Out")))
    )
