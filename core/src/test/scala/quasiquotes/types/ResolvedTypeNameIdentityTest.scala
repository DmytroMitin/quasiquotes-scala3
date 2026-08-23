package quasiquotes.types

import quasiquotes.parser.TypeShape

class ResolvedTypeNameIdentityTest extends munit.FunSuite:
  private val scalaPackage = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")
  private val collectionPackage = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "collection")
  private val immutablePackage = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "immutable")

  private val listId = ResolvedTypeNameId(
    Vector(scalaPackage, collectionPackage, immutablePackage),
    "List"
  )
  private val optionId = ResolvedTypeNameId(Vector(scalaPackage), "Option")
  private val eitherId = ResolvedTypeNameId(
    Vector(scalaPackage, ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "util")),
    "Either"
  )
  private val ownerOneSame = ResolvedTypeNameId(
    Vector(
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "phase119"),
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Module, "OwnerOne")
    ),
    "Same"
  )
  private val ownerTwoSame = ResolvedTypeNameId(
    Vector(
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "phase119"),
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Module, "OwnerTwo")
    ),
    "Same"
  )

  test("resolved identity uses every ordered owner segment and renders deterministically"):
    assertNotEquals(ownerOneSame, ownerTwoSame)
    assertEquals(ownerOneSame.canonicalSource, "phase119.OwnerOne.Same")
    assertEquals(
      ownerOneSame.render,
      "ResolvedTypeNameId(Package(phase119)/Module(OwnerOne)::Same)"
    )

  test("canonical selected paths resolve without arbitrary source labels"):
    val environment = ResolvedTypeEnvironment.fromIds(Vector(ownerOneSame)).toOption.get
    val shape = TypeShape.Select(
      TypeShape.Select(TypeShape.Identifier("phase119"), "OwnerOne"),
      "Same"
    )

    assertEquals(
      TypeNormalForm.fromShapeResolved(shape, environment),
      Right(TypeNormalForm.STypeResolved(ownerOneSame))
    )
    val unresolved = TypeNormalForm.fromShapeResolved(
      TypeShape.Select(TypeShape.Identifier("phase119"), "Alias"),
      environment
    )
    assert(unresolved.swap.toOption.exists(_.message.startsWith("TYPE_NAME_RESOLUTION_UNRESOLVED")))

  test("duplicate canonical bindings fail as ambiguity"):
    val duplicate = ResolvedTypeEnvironment.fromIds(Vector(ownerOneSame, ownerOneSame))
    assert(duplicate.swap.toOption.exists(_.message.startsWith("TYPE_NAME_RESOLUTION_AMBIGUOUS")))

  test("selected fixed constructors require full standard declaration identity and exact arity"):
    val environment = ResolvedTypeEnvironment.fromIds(Vector(listId, optionId, eitherId)).toOption.get
    val listInt = TypeShape.Apply(
      TypeShape.Select(
        TypeShape.Select(
          TypeShape.Select(TypeShape.Identifier("scala"), "collection"),
          "immutable"
        ),
        "List"
      ),
      List(TypeShape.Identifier("Int"))
    )
    assertEquals(
      TypeNormalForm.fromShapeResolved(listInt, environment),
      Right(
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeResolved(listId),
          List(TypeNormalForm.STypeIdent("Int"))
        )
      )
    )

    val fakeList = ResolvedTypeNameId(
      Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "user")),
      "List"
    )
    val fakeEnvironment = ResolvedTypeEnvironment.fromIds(Vector(fakeList)).toOption.get
    val rejected = TypeNormalForm.fromShapeResolved(
      TypeShape.Apply(
        TypeShape.Select(TypeShape.Identifier("user"), "List"),
        List(TypeShape.Identifier("Int"))
      ),
      fakeEnvironment
    )
    assert(rejected.swap.toOption.exists(_.message.startsWith("TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH")))

  test("resolved identity participates in literal and repeated-hole structural matching"):
    val literal = TypePattern.TPResolved(ownerOneSame)
    assert(TypePattern.matchNormalForm(literal, TypeNormalForm.STypeResolved(ownerOneSame)).nonEmpty)
    assert(TypePattern.matchNormalForm(literal, TypeNormalForm.STypeResolved(ownerTwoSame)).isEmpty)

    val repeated = TypePattern.TPApply(
      TypePattern.TPIdent("Either"),
      List(TypePattern.TPHole("same"), TypePattern.TPHole("same"))
    )
    val equalTarget = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("Either"),
      List(TypeNormalForm.STypeResolved(ownerOneSame), TypeNormalForm.STypeResolved(ownerOneSame))
    )
    val unequalTarget = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("Either"),
      List(TypeNormalForm.STypeResolved(ownerOneSame), TypeNormalForm.STypeResolved(ownerTwoSame))
    )
    assert(TypePattern.matchNormalForm(repeated, equalTarget).nonEmpty)
    assert(TypePattern.matchNormalForm(repeated, unequalTarget).isEmpty)

  test("resolved templates carry identity into construction and source rendering"):
    val template = TypeTemplate.TTResolved(ownerOneSame)
    val constructed = TypeTemplate.construct(template, Map.empty)
    assertEquals(constructed, Right(TypeNormalForm.STypeResolved(ownerOneSame)))
    assertEquals(constructed.map(ConstructedType.renderSource), Right("phase119.OwnerOne.Same"))
