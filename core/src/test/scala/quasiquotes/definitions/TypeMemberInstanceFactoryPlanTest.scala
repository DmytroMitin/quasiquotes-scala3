package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.*

final class TypeMemberInstanceFactoryPlanTest extends munit.FunSuite:
  test("creates canonical and renamed abstract-Type-member factory graphs"):
    val canonical = createPlan("instance", "A", "Out0", "HasOut", "Out")
    val renamed = createPlan("make", "Element", "Result0", "Container", "Result")

    assertPlan(canonical, "instance", "A", "Out0", "HasOut", "Out")
    assertPlan(renamed, "make", "Element", "Result0", "Container", "Result")
    assertEquals(canonical.roleSnapshot.topology, renamed.roleSnapshot.topology)
    assertEquals(
      canonical.roleSnapshot.topology,
      RoleTopology(
        targetArgumentRole = DeclarationRole.FirstTypeParameter,
        resultRefinementBaseRole = ExternalTypeRole.TargetConstructor,
        resultAliasRhsRole = DeclarationRole.SecondTypeParameter,
        anonymousParentRole = ExternalTypeRole.TargetConstructor,
        anonymousAliasRhsRole = DeclarationRole.SecondTypeParameter,
        anonymousAliasMemberRole = MemberRole.ResultAliasMember
      )
    )

  test("allocates graph-local identities while role snapshots remain alpha-equivalent"):
    val first = createPlan("instance", "A", "Out0", "HasOut", "Out")
    val second = createPlan("instance", "A", "Out0", "HasOut", "Out")

    assertNotEquals(declarationBinderIds(first), declarationBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

  test("rejects every missing or illegal source role with stable codes"):
    val valid = Vector("instance", "A", "Out0", "HasOut", "Out")
    val expected = Vector(
      "FACTORY_NAME_INVALID",
      "FIRST_TYPE_PARAMETER_NAME_INVALID",
      "SECOND_TYPE_PARAMETER_NAME_INVALID",
      "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
      "MEMBER_NAME_INVALID"
    )

    valid.indices.foreach { index =>
      assertCode(createFrom(valid.updated(index, null)), expected(index))
      assertCode(createFrom(valid.updated(index, "")), expected(index))
      assertCode(createFrom(valid.updated(index, "not legal")), expected(index))
    }

  test("rejects compiler-proven lexical role collapses"):
    assertCode(
      TypeMemberInstanceFactoryPlan.create("instance", "A", "A", "HasOut", "Out"),
      "TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT"
    )
    assertCode(
      TypeMemberInstanceFactoryPlan.create("instance", "A", "Out0", "A", "Out"),
      "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE"
    )
    assertCode(
      TypeMemberInstanceFactoryPlan.create("instance", "A", "Out0", "Out0", "Out"),
      "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE"
    )
    assertCode(
      TypeMemberInstanceFactoryPlan.create("instance", "A", "Out0", "HasOut", "Out0"),
      "MEMBER_SECOND_TYPE_PARAMETER_ROLE_COLLAPSE"
    )

  test("admits compiler-proven safe same-spelling controls"):
    val memberFirst = createPlan("instance", "A", "Out0", "HasOut", "A")
    val factoryTypesAndMember = createPlan("same", "same", "Other", "Target", "same")

    assertEquals(memberFirst.resultAlias.memberName, "A")
    assertEquals(factoryTypesAndMember.factoryDisplayName, "same")
    assertEquals(factoryTypesAndMember.firstTypeParameter.displayName, "same")
    assertEquals(factoryTypesAndMember.resultAlias.memberName, "same")

  test("Core accepts legal backticked roles for neutral representability checks"):
    val plan = createPlan("`def`", "`type`", "`val`", "`class`", "`object`")

    assertEquals(plan.roleSnapshot.factorySourceName, "`def`")
    assertEquals(plan.roleSnapshot.targetConstructorSourceName, "`class`")

  test("revalidates every retained edge and rejects fabricated corruption"):
    val valid = createPlan("instance", "A", "Out0", "HasOut", "Out")
    assertEquals(TypeMemberInstanceFactoryPlan.validate(valid), Right(valid.roleSnapshot))

    val wrongFirstReference = TypeParameterReference(
      valid.secondTypeParameter.binderId,
      valid.firstTypeParameter.displayName
    )
    val wrongSecondReference = TypeParameterReference(
      valid.firstTypeParameter.binderId,
      valid.secondTypeParameter.displayName
    )
    val corruptions = List(
      copyPlan(valid, resultTarget = Applied(SourceName("HasOut"), Vector(wrongFirstReference))),
      copyPlan(valid, resultType = Refinement(valid.resultTarget, Vector(valid.anonymousAlias))),
      copyPlan(valid, resultAlias = valid.resultAlias.copy(rhs = wrongSecondReference)),
      copyPlan(valid, anonymousParentTarget = Applied(SourceName("Other"), valid.resultTarget.arguments)),
      copyPlan(valid, anonymousAlias = valid.anonymousAlias.copy(memberName = "Other")),
      copyPlan(valid, anonymousAlias = valid.anonymousAlias.copy(rhs = wrongSecondReference)),
      copyPlan(valid, anonymousAlias = valid.resultAlias),
      copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "Other"))
    )

    corruptions.foreach { plan =>
      assertCode(TypeMemberInstanceFactoryPlan.validate(plan).map(_ => plan), "PLAN_GRAPH_INVALID")
    }

  private def copyPlan(
      plan: Plan,
      resultType: Refinement = null,
      resultTarget: Applied = null,
      resultAlias: ScopedTypeAlias = null,
      anonymousParentTarget: Applied = null,
      anonymousAlias: ScopedTypeAlias = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.factoryDisplayName,
      plan.firstTypeParameter,
      plan.secondTypeParameter,
      Option(resultType).getOrElse(plan.resultType),
      Option(resultTarget).getOrElse(plan.resultTarget),
      Option(resultAlias).getOrElse(plan.resultAlias),
      Option(anonymousParentTarget).getOrElse(plan.anonymousParentTarget),
      Option(anonymousAlias).getOrElse(plan.anonymousAlias),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def createFrom(values: Vector[String]) =
    TypeMemberInstanceFactoryPlan.create(values(0), values(1), values(2), values(3), values(4))

  private def createPlan(
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  ): Plan =
    TypeMemberInstanceFactoryPlan
      .create(factory, firstTypeParameter, secondTypeParameter, target, member)
      .fold(problem => fail(problem.message), identity)

  private def assertPlan(
      plan: Plan,
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  ): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.firstTypeParameter.displayName, firstTypeParameter)
    assertEquals(plan.secondTypeParameter.displayName, secondTypeParameter)
    assertNotEquals(plan.firstTypeParameter.binderId, plan.secondTypeParameter.binderId)
    assertEquals(plan.resultAlias.memberName, member)
    assertEquals(plan.anonymousAlias.memberName, member)
    assert(!(plan.resultAlias eq plan.anonymousAlias))
    assertEquals(plan.resultType, Refinement(plan.resultTarget, Vector(plan.resultAlias)))
    assertEquals(plan.resultTarget, plan.anonymousParentTarget)
    plan.resultTarget match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, target)
        assertEquals(reference.binderId, plan.firstTypeParameter.binderId)
      case other => fail(s"unexpected target $other")
    List(plan.resultAlias, plan.anonymousAlias).foreach { alias =>
      alias.rhs match
        case reference: TypeParameterReference =>
          assertEquals(reference.binderId, plan.secondTypeParameter.binderId)
        case other => fail(s"unexpected alias RHS $other")
    }

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.firstTypeParameter.binderId, plan.secondTypeParameter.binderId)

  private def assertCode(result: Either[ModelError, Plan], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
