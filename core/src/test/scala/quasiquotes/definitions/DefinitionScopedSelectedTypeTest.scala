package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

class DefinitionScopedSelectedTypeTest extends munit.FunSuite:
  private val holderAOut = member("HolderA", "Out")
  private val holderBOut = member("HolderB", "Out")

  test("alpha-renamed one-binder definitions retain one scoped selected-Type identity") {
    val original = plan(Vector(BinderId(0)), BinderId(0), holderAOut)
    val renamed = plan(Vector(BinderId(17)), BinderId(17), holderAOut)

    assert(original.alphaEquivalentTo(renamed))
    assert(renamed.alphaEquivalentTo(original))
  }

  test("binder position and structured member declaration both participate in scoped equality") {
    val firstPrefix = plan(
      Vector(BinderId(4), BinderId(9)),
      BinderId(4),
      holderAOut
    )
    val secondPrefix = plan(
      Vector(BinderId(4), BinderId(9)),
      BinderId(9),
      holderAOut
    )
    val otherDeclaration = plan(
      Vector(BinderId(4), BinderId(9)),
      BinderId(4),
      holderBOut
    )

    assert(!firstPrefix.alphaEquivalentTo(secondPrefix))
    assert(!firstPrefix.alphaEquivalentTo(otherDeclaration))
  }

  test("display spelling cannot collapse distinct binders") {
    val first = plan(
      Vector(BinderId(2), BinderId(3)),
      BinderId(2),
      holderAOut
    )
    val second = plan(
      Vector(BinderId(2), BinderId(3)),
      BinderId(3),
      holderAOut
    )

    // The carrier deliberately has no display-name input. Even a caller that
    // renders both binders with the same test spelling cannot affect identity.
    assertEquals(first.prefixBinderPosition, 0)
    assertEquals(second.prefixBinderPosition, 1)
    assert(!first.alphaEquivalentTo(second))
  }

  test("scope construction rejects undeclared and duplicate project binders") {
    val undeclared = DefinitionScopedSelectedTypePlan.create(
      Vector(BinderId(0)),
      BinderId(1),
      holderAOut
    )
    val duplicate = DefinitionScopedSelectedTypePlan.create(
      Vector(BinderId(0), BinderId(0)),
      BinderId(0),
      holderAOut
    )

    assertEquals(
      undeclared.left.toOption.map(_.code),
      Some("STABLE_SELECTED_TYPE_PREFIX_UNBOUND")
    )
    assertEquals(
      duplicate.left.toOption.map(_.code),
      Some("STABLE_SELECTED_TYPE_SCOPE_INVALID")
    )
  }

  test("a scoped value is compared only through its enclosing carrier") {
    val scope = plan(Vector(BinderId(0)), BinderId(0), holderAOut)
    val separatelyOwned = plan(Vector(BinderId(0)), BinderId(0), holderAOut)

    assert(!scope.owns(separatelyOwned.selectedType))
    assert(scope.alphaEquivalentTo(separatelyOwned))
  }

  test("existing constructed definition binders seed the same scoped plan") {
    val firstBinder = BinderId(3)
    val secondBinder = BinderId(8)
    val single = ConstructedDefinition
      .singleParameterDef(
        definitionName("single"),
        firstBinder,
        definitionName("value"),
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("Int"),
        boundBody(firstBinder, "value", Vector(firstBinder))
      )
      .fold(error => fail(error.message), identity)
    val two = ConstructedDefinition
      .twoParameterDef(
        definitionName("pair"),
        firstBinder,
        definitionName("first"),
        TypeNormalForm.STypeIdent("Int"),
        secondBinder,
        definitionName("second"),
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("Int"),
        boundBody(secondBinder, "second", Vector(firstBinder, secondBinder))
      )
      .fold(error => fail(error.message), identity)

    val singlePlan = DefinitionScopedSelectedTypePlan
      .fromSingleParameterDefinition(single, holderAOut)
      .fold(error => fail(error.message), identity)
    val twoPlan = DefinitionScopedSelectedTypePlan
      .fromTwoParameterDefinition(two, secondBinder, holderAOut)
      .fold(error => fail(error.message), identity)

    assertEquals(singlePlan.prefixBinderPosition, 0)
    assertEquals(twoPlan.prefixBinderPosition, 1)
  }

  private def plan(
      binders: Vector[BinderId],
      prefix: BinderId,
      memberId: ResolvedTypeNameId
  ): DefinitionScopedSelectedTypePlan =
    DefinitionScopedSelectedTypePlan
      .create(binders, prefix, memberId)
      .fold(error => fail(error.message), identity)

  private def member(owner: String, terminal: String): ResolvedTypeNameId =
    ResolvedTypeNameId(
      Vector(
        ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "phase126"),
        ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Type, owner)
      ),
      terminal
    )

  private def definitionName(value: String): DefinitionName =
    DefinitionName.plain(value).fold(error => fail(error.message), identity)

  private def boundBody(
      binder: BinderId,
      displayName: String,
      scope: Vector[BinderId]
  ): ConstructedTerm =
    ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(binder, displayName), scope)
      .fold(error => fail(error.message), identity)
