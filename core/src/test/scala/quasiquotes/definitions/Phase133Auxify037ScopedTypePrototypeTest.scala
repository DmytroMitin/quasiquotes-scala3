package quasiquotes.definitions

import quasiquotes.parser.BinderId

class Phase133Auxify037ScopedTypePrototypeTest extends munit.FunSuite:
  import Phase133Auxify037ScopedTypePrototype.*

  test("type-parameter references retain binder position across alpha renaming") {
    val original = plan(
      first = TypeParameter(BinderId(3), "N", SourceTypeName("Nat")),
      second = TypeParameter(BinderId(8), "M", SourceTypeName("Nat")),
      contextualBinder = BinderId(13),
      contextualName = "inst"
    )
    val renamed = plan(
      first = TypeParameter(BinderId(21), "Left", SourceTypeName("Nat")),
      second = TypeParameter(BinderId(34), "Right", SourceTypeName("Nat")),
      contextualBinder = BinderId(55),
      contextualName = "evidence"
    )

    assertEquals(original.alphaKey, renamed.alphaKey)
    assertEquals(original.appliedArgumentPositions, Vector(0, 1))
    assertEquals(renamed.appliedArgumentPositions, Vector(0, 1))
  }

  test("hostile duplicate display names do not collapse binder identity and fail source validation") {
    val hostile = plan(
      first = TypeParameter(BinderId(1), "T", SourceTypeName("Nat")),
      second = TypeParameter(BinderId(2), "T", SourceTypeName("Nat")),
      contextualBinder = BinderId(3),
      contextualName = "inst"
    )

    assertEquals(hostile.appliedArgumentPositions, Vector(0, 1))
    assertEquals(
      hostile.validate.left.toOption,
      Some("TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT")
    )
  }

  test("the exact result is one refinement whose RHS uses the contextual binder") {
    val target = plan(
      first = TypeParameter(BinderId(5), "N", SourceTypeName("Nat")),
      second = TypeParameter(BinderId(6), "M", SourceTypeName("Nat")),
      contextualBinder = BinderId(7),
      contextualName = "inst"
    )

    assertEquals(target.validate, Right(()))
    assertEquals(target.refinement.memberName, "Out")
    assertEquals(target.refinement.rhs.prefixBinder, BinderId(7))
    assertEquals(target.refinement.rhs.memberSourceName, "Out")
    assertEquals(target.bodyBinder, BinderId(7))
  }

  test("an undeclared Type argument or detached selected prefix fails closed") {
    val valid = plan(
      first = TypeParameter(BinderId(5), "N", SourceTypeName("Nat")),
      second = TypeParameter(BinderId(6), "M", SourceTypeName("Nat")),
      contextualBinder = BinderId(7),
      contextualName = "inst"
    )
    val undeclaredArgument = valid.copy(
      contextualType = valid.contextualType.copy(
        arguments = Vector(
          TypeParameterReference(BinderId(5), "N"),
          TypeParameterReference(BinderId(99), "M")
        )
      )
    )
    val detachedPrefix = valid.copy(
      refinement = valid.refinement.copy(
        rhs = valid.refinement.rhs.copy(prefixBinder = BinderId(99))
      )
    )

    assertEquals(
      undeclaredArgument.validate.left.toOption,
      Some("TYPE_PARAMETER_REFERENCE_UNBOUND")
    )
    assertEquals(
      detachedPrefix.validate.left.toOption,
      Some("STABLE_SELECTED_TYPE_PREFIX_UNBOUND")
    )
  }

  private def plan(
      first: TypeParameter,
      second: TypeParameter,
      contextualBinder: BinderId,
      contextualName: String
  ): MethodPlan =
    val applied = AppliedType(
      SourceTypeName("Add"),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(second.binderId, second.displayName)
      )
    )
    MethodPlan(
      Vector(first, second),
      contextualBinder,
      contextualName,
      applied,
      SingleTypeAliasRefinement(
        applied,
        "Out",
        DirectStableSelectedType(contextualBinder, "Out")
      ),
      contextualBinder
    )

private object Phase133Auxify037ScopedTypePrototype:
  final case class SourceTypeName(value: String) derives CanEqual

  final case class TypeParameter(
      binderId: BinderId,
      displayName: String,
      upperBound: SourceTypeName
  ) derives CanEqual

  final case class TypeParameterReference(
      binderId: BinderId,
      displayName: String
  ) derives CanEqual

  final case class AppliedType(
      constructor: SourceTypeName,
      arguments: Vector[TypeParameterReference]
  ) derives CanEqual

  final case class DirectStableSelectedType(
      prefixBinder: BinderId,
      memberSourceName: String
  ) derives CanEqual

  final case class SingleTypeAliasRefinement(
      base: AppliedType,
      memberName: String,
      rhs: DirectStableSelectedType
  ) derives CanEqual

  final case class MethodPlan(
      typeParameters: Vector[TypeParameter],
      contextualBinder: BinderId,
      contextualName: String,
      contextualType: AppliedType,
      refinement: SingleTypeAliasRefinement,
      bodyBinder: BinderId
  ) derives CanEqual:
    def appliedArgumentPositions: Vector[Int] =
      val positions = typeParameters.map(_.binderId).zipWithIndex.toMap
      contextualType.arguments.map(argument => positions.getOrElse(argument.binderId, -1))

    def alphaKey: AlphaKey =
      AlphaKey(
        typeParameters.map(_.upperBound.value),
        contextualType.constructor.value,
        appliedArgumentPositions,
        refinement.memberName,
        Option.when(refinement.rhs.prefixBinder == contextualBinder)(0),
        refinement.rhs.memberSourceName,
        bodyBinder == contextualBinder
      )

    def validate: Either[String, Unit] =
      val declared = typeParameters.map(_.binderId)
      if typeParameters.size != 2 then Left("TYPE_PARAMETER_ARITY_UNSUPPORTED")
      else if declared.distinct.size != declared.size then Left("TYPE_PARAMETER_BINDERS_MUST_BE_DISTINCT")
      else if typeParameters.map(_.displayName).distinct.size != 2 then
        Left("TYPE_PARAMETER_DISPLAY_NAMES_MUST_BE_DISTINCT")
      else if contextualType.arguments.exists(argument => !declared.contains(argument.binderId)) then
        Left("TYPE_PARAMETER_REFERENCE_UNBOUND")
      else if contextualType.arguments.map(_.binderId) != declared then
        Left("TYPE_PARAMETER_ARGUMENT_ORDER_MISMATCH")
      else if refinement.base != contextualType then Left("REFINEMENT_BASE_MISMATCH")
      else if refinement.rhs.prefixBinder != contextualBinder then
        Left("STABLE_SELECTED_TYPE_PREFIX_UNBOUND")
      else if refinement.memberName != refinement.rhs.memberSourceName then
        Left("REFINEMENT_MEMBER_NAME_MISMATCH")
      else if bodyBinder != contextualBinder then Left("CONTEXTUAL_BODY_BINDER_MISMATCH")
      else Right(())

  final case class AlphaKey(
      upperBounds: Vector[String],
      constructor: String,
      argumentPositions: Vector[Int],
      refinementMember: String,
      selectedPrefixPosition: Option[Int],
      selectedMemberSourceName: String,
      bodyUsesContextualBinder: Boolean
  ) derives CanEqual
