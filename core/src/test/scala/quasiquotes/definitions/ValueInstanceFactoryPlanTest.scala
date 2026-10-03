package quasiquotes.definitions

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.ValueInstanceFactoryPlan.*

final class ValueInstanceFactoryPlanTest extends munit.FunSuite:
  test("creates canonical and renamed one-value role graphs"):
    val canonical = createPlan("instance", "A", "valueValue", "HasValue", "value")
    val renamed = createPlan("make", "Element", "elementCarrier", "Container", "element")

    assertPlan(canonical, "instance", "A", "valueValue", "HasValue", "value")
    assertPlan(renamed, "make", "Element", "elementCarrier", "Container", "element")
    assertEquals(canonical.roleSnapshot.topology, renamed.roleSnapshot.topology)
    assertEquals(
      canonical.roleSnapshot.topology,
      RoleTopology(
        carrierTypeRole = DeclarationRole.TypeParameter,
        targetArgumentRole = DeclarationRole.TypeParameter,
        resultTargetRole = ExternalTypeRole.TargetConstructor,
        parentTargetRole = ExternalTypeRole.TargetConstructor,
        memberTypeRole = DeclarationRole.TypeParameter,
        rhsCarrierRole = DeclarationRole.StrictCarrier
      )
    )

  test("allocates graph-local identities while role snapshots remain alpha-equivalent"):
    val first = createPlan("instance", "A", "valueValue", "HasValue", "value")
    val second = createPlan("instance", "A", "valueValue", "HasValue", "value")

    assertNotEquals(declarationBinderIds(first), declarationBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

  test("rejects every missing or illegal source role with stable codes"):
    val valid = Vector("instance", "A", "valueValue", "HasValue", "value")
    val expected = Vector(
      "FACTORY_NAME_INVALID",
      "TYPE_PARAMETER_NAME_INVALID",
      "CARRIER_NAME_INVALID",
      "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
      "MEMBER_NAME_INVALID"
    )

    valid.indices.foreach { index =>
      assertCode(createFrom(valid.updated(index, null)), expected(index))
      assertCode(createFrom(valid.updated(index, "")), expected(index))
      assertCode(createFrom(valid.updated(index, "not legal")), expected(index))
    }

  test("rejects only the proven unsafe lexical role collapses"):
    assertCode(
      ValueInstanceFactoryPlan.create("instance", "A", "valueValue", "A", "value"),
      "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE"
    )
    assertCode(
      ValueInstanceFactoryPlan.create("instance", "A", "value", "HasValue", "value"),
      "CARRIER_MEMBER_ROLE_COLLAPSE"
    )

  test("admits safe factory Type carrier and member same-spelling controls"):
    val factoryTypeCarrier = createPlan("same", "same", "same", "HasValue", "member")
    val factoryMember = createPlan("same", "A", "carrier", "HasValue", "same")

    assertEquals(factoryTypeCarrier.factoryDisplayName, "same")
    assertEquals(factoryTypeCarrier.typeParameter.displayName, "same")
    assertEquals(factoryTypeCarrier.strictCarrier.displayName, "same")
    assertEquals(factoryMember.valueOverride.memberDisplayName, "same")

  test("Core accepts legal backticked roles for neutral representability checks"):
    val plan = createPlan("`def`", "`type`", "`val`", "`class`", "`object`")

    assertEquals(plan.roleSnapshot.factorySourceName, "`def`")
    assertEquals(plan.roleSnapshot.targetConstructorSourceName, "`class`")

  test("revalidates the complete semantic graph and rejects fabricated corruption"):
    val valid = createPlan("instance", "A", "valueValue", "HasValue", "value")
    assertEquals(ValueInstanceFactoryPlan.validate(valid), Right(valid.roleSnapshot))

    val wrongTypeReference = TypeParameterReference(
      valid.strictCarrier.binderId,
      valid.typeParameter.displayName
    )
    val corruptions = List(
      copyPlan(valid, strictCarrier = valid.strictCarrier.copy(parameterType = wrongTypeReference)),
      copyPlan(valid, resultTarget = Applied(SourceName("HasValue"), Vector(wrongTypeReference))),
      copyPlan(valid, anonymousParentTarget = Applied(SourceName("Other"), valid.resultTarget.arguments)),
      copyPlan(valid, valueOverride = valid.valueOverride.copy(valueType = wrongTypeReference)),
      copyPlan(valid, valueOverride = valid.valueOverride.copy(body = TermReference(valid.typeParameter.binderId))),
      copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "other"))
    )

    corruptions.foreach { plan =>
      assertCode(ValueInstanceFactoryPlan.validate(plan).map(_ => plan), "PLAN_GRAPH_INVALID")
    }

  private def copyPlan(
      plan: Plan,
      strictCarrier: StrictCarrier = null,
      resultTarget: Applied = null,
      anonymousParentTarget: Applied = null,
      valueOverride: ValueOverride = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.factoryDisplayName,
      plan.typeParameter,
      Option(strictCarrier).getOrElse(plan.strictCarrier),
      Option(resultTarget).getOrElse(plan.resultTarget),
      Option(anonymousParentTarget).getOrElse(plan.anonymousParentTarget),
      Option(valueOverride).getOrElse(plan.valueOverride),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def createFrom(values: Vector[String]) =
    ValueInstanceFactoryPlan.create(values(0), values(1), values(2), values(3), values(4))

  private def createPlan(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ): Plan =
    ValueInstanceFactoryPlan
      .create(factory, typeParameter, carrier, target, member)
      .fold(problem => fail(problem.message), identity)

  private def assertPlan(
      plan: Plan,
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.strictCarrier.displayName, carrier)
    assertEquals(plan.strictCarrier.mode, ParameterMode.ByValue)
    assertEquals(plan.valueOverride.memberDisplayName, member)
    assertEquals(plan.strictCarrier.parameterType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.valueOverride.valueType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.valueOverride.body.binderId, plan.strictCarrier.binderId)
    assertEquals(plan.resultTarget, plan.anonymousParentTarget)
    plan.resultTarget match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, target)
        assertEquals(reference.binderId, plan.typeParameter.binderId)
      case other => fail(s"unexpected target $other")

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.typeParameter.binderId, plan.strictCarrier.binderId)

  private def assertCode(result: Either[ModelError, Plan], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
