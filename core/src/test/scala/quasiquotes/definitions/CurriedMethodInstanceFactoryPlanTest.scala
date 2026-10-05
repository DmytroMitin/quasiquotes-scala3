package quasiquotes.definitions

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

final class CurriedMethodInstanceFactoryPlanTest extends munit.FunSuite:
  test("creates exact canonical and renamed curried-method role graphs"):
    val canonical = createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    val renamed = createPlan("make", "Element", "operation", "Combiner", "merge", "left", "right")

    assertPlan(canonical, "instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    assertPlan(renamed, "make", "Element", "operation", "Combiner", "merge", "left", "right")
    assertEquals(canonical.roleSnapshot.topology, renamed.roleSnapshot.topology)

  test("allocates four graph-local identities while snapshots remain alpha-equivalent"):
    val first = createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    val second = createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b")

    assertEquals(declarationBinderIds(first).distinct.size, 4)
    assertEquals(declarationBinderIds(second).distinct.size, 4)
    assertNotEquals(declarationBinderIds(first), declarationBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

  test("rejects every missing or illegal source role with stable codes"):
    val valid = Vector("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    val expected = Vector(
      "FACTORY_NAME_INVALID",
      "TYPE_PARAMETER_NAME_INVALID",
      "CARRIER_NAME_INVALID",
      "TARGET_TYPE_CONSTRUCTOR_NAME_INVALID",
      "MEMBER_NAME_INVALID",
      "FIRST_PARAMETER_NAME_INVALID",
      "SECOND_PARAMETER_NAME_INVALID"
    )

    valid.indices.foreach { index =>
      assertCode(createFrom(valid.updated(index, null)), expected(index))
      assertCode(createFrom(valid.updated(index, "")), expected(index))
      assertCode(createFrom(valid.updated(index, "not legal")), expected(index))
    }

  test("rejects compiler-proven lexical collapses"):
    assertCode(createFrom(Vector("instance", "A", "f", "A", "combine", "a", "b")), "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE")
    assertCode(createFrom(Vector("instance", "A", "combine", "Curried", "combine", "a", "b")), "CARRIER_MEMBER_ROLE_COLLAPSE")
    assertCode(createFrom(Vector("instance", "A", "a", "Curried", "combine", "a", "b")), "CARRIER_FIRST_PARAMETER_ROLE_COLLAPSE")
    assertCode(createFrom(Vector("instance", "A", "b", "Curried", "combine", "a", "b")), "CARRIER_SECOND_PARAMETER_ROLE_COLLAPSE")
    assertCode(createFrom(Vector("instance", "A", "f", "Curried", "combine", "same", "same")), "NESTED_PARAMETER_NAMES_MUST_BE_DISTINCT")

  test("admits compiler-proven safe same-spelling controls"):
    val rows = List(
      Vector("combine", "A", "f", "Curried", "combine", "a", "b"),
      Vector("instance", "A", "f", "Curried", "a", "a", "b"),
      Vector("instance", "A", "f", "Curried", "b", "a", "b"),
      Vector("same", "same", "same", "Target", "member", "left", "right"),
      Vector("Target", "A", "f", "Target", "combine", "a", "b")
    )

    rows.foreach(values => assertEquals(CurriedMethodInstanceFactoryPlan.validate(createFrom(values).toOption.get).isRight, true))

  test("Core accepts legal backticked roles for the neutral representability boundary"):
    val plan = createPlan("`def`", "`type`", "`val`", "`class`", "`object`", "`match`", "`given`")
    assertEquals(plan.roleSnapshot.factorySourceName, "`def`")
    assertEquals(plan.roleSnapshot.secondParameterSourceName, "`given`")

  test("revalidates every retained edge and rejects forged graph corruption"):
    val valid = createPlan("instance", "A", "combineFunction", "Curried", "combine", "a", "b")
    assertEquals(CurriedMethodInstanceFactoryPlan.validate(valid), Right(valid.roleSnapshot))
    assertEquals(
      CurriedMethodInstanceFactoryPlan.validate(null).left.toOption.map(_.code),
      Some("PLAN_MISSING")
    )
    val wrongType = TypeParameterReference(valid.strictCarrier.binderId, "A")
    val wrongTerm = TermReference(valid.typeParameter.binderId)
    val corruptions = List(
      copyPlan(valid, strictCarrier = valid.strictCarrier.copy(parameterType = valid.strictCarrier.parameterType.copy(result = wrongType))),
      copyPlan(valid, resultTarget = Applied(SourceName("Curried"), Vector(wrongType))),
      copyPlan(valid, anonymousParentTarget = Applied(SourceName("Other"), valid.resultTarget.arguments)),
      copyPlan(valid, methodOverride = valid.methodOverride.copy(resultType = wrongType)),
      copyPlan(valid, methodOverride = valid.methodOverride.copy(body = valid.methodOverride.body.copy(callee = wrongTerm))),
      copyPlan(valid, methodOverride = valid.methodOverride.copy(body = valid.methodOverride.body.copy(firstArgument = TermReference(valid.secondParameter.binderId)))),
      copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "other")),
      new Plan(
        valid.factoryDisplayName,
        valid.typeParameter,
        null,
        valid.resultTarget,
        valid.anonymousParentTarget,
        valid.firstParameter,
        valid.secondParameter,
        valid.methodOverride,
        valid.roleSnapshot
      )
    )
    corruptions.foreach(plan => assertCode(CurriedMethodInstanceFactoryPlan.validate(plan).map(_ => plan), "PLAN_GRAPH_INVALID"))

    val duplicate = new Plan(
      valid.factoryDisplayName,
      valid.typeParameter,
      valid.strictCarrier,
      valid.resultTarget,
      valid.anonymousParentTarget,
      valid.firstParameter.copy(binderId = valid.strictCarrier.binderId),
      valid.secondParameter,
      valid.methodOverride,
      valid.roleSnapshot
    )
    assertCode(CurriedMethodInstanceFactoryPlan.validate(duplicate).map(_ => duplicate), "PLAN_GRAPH_INVALID")

  private def createFrom(values: Vector[String]) =
    CurriedMethodInstanceFactoryPlan.create(values(0), values(1), values(2), values(3), values(4), values(5), values(6))

  private def createPlan(factory: String, tpe: String, carrier: String, target: String, member: String, first: String, second: String): Plan =
    CurriedMethodInstanceFactoryPlan.create(factory, tpe, carrier, target, member, first, second).fold(problem => fail(problem.message), identity)

  private def assertPlan(plan: Plan, factory: String, tpe: String, carrier: String, target: String, member: String, first: String, second: String): Unit =
    assertEquals(plan.factoryDisplayName, factory)
    assertEquals(plan.typeParameter.displayName, tpe)
    assertEquals(plan.strictCarrier.displayName, carrier)
    assertEquals(plan.firstParameter.displayName, first)
    assertEquals(plan.secondParameter.displayName, second)
    assertEquals(plan.methodOverride.memberDisplayName, member)
    plan.strictCarrier.parameterType match
      case NestedUnaryFunctionType(a, b, result) =>
        List(a, b, result).foreach(ref => assertEquals(ref.binderId, plan.typeParameter.binderId))
      case other => fail(s"unexpected carrier Type $other")
    plan.resultTarget match
      case Applied(SourceName(constructor), Vector(reference: TypeParameterReference)) =>
        assertEquals(constructor, target)
        assertEquals(reference.binderId, plan.typeParameter.binderId)
      case other => fail(s"unexpected target $other")
    assertEquals(plan.methodOverride.body.callee.binderId, plan.strictCarrier.binderId)
    assertEquals(plan.methodOverride.body.firstArgument.binderId, plan.firstParameter.binderId)
    assertEquals(plan.methodOverride.body.secondArgument.binderId, plan.secondParameter.binderId)

  private def copyPlan(plan: Plan, strictCarrier: StrictCarrier = null, resultTarget: Applied = null, anonymousParentTarget: Applied = null, methodOverride: MethodOverride = null, roleSnapshot: RoleSnapshot = null): Plan =
    new Plan(
      plan.factoryDisplayName,
      plan.typeParameter,
      Option(strictCarrier).getOrElse(plan.strictCarrier),
      Option(resultTarget).getOrElse(plan.resultTarget),
      Option(anonymousParentTarget).getOrElse(plan.anonymousParentTarget),
      plan.firstParameter,
      plan.secondParameter,
      Option(methodOverride).getOrElse(plan.methodOverride),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def declarationBinderIds(plan: Plan): Vector[BinderId] =
    Vector(plan.typeParameter.binderId, plan.strictCarrier.binderId, plan.firstParameter.binderId, plan.secondParameter.binderId)

  private def assertCode(result: Either[ModelError, Plan], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
