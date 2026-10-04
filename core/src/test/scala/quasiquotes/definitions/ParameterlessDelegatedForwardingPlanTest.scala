package quasiquotes.definitions

import quasiquotes.definitions.DelegatedForwardingMethodPlan.{
  ContextualParameter,
  ContextualReference,
  MethodIdentity
}
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.*
import quasiquotes.definitions.ScopedType.*

final class ParameterlessDelegatedForwardingPlanTest extends munit.FunSuite:
  test("creates canonical and fully renamed parameterless forwarding graphs"):
    val canonical = createPlan("empty", "A", "inst", "Empty")
    val renamed = createPlan("obtain", "Element", "evidence", "Provider")

    assertPlan(canonical, "empty", "A", "inst", "Empty")
    assertPlan(renamed, "obtain", "Element", "evidence", "Provider")
    assertEquals(canonical.roleSnapshot.topology, renamed.roleSnapshot.topology)
    assertEquals(
      canonical.roleSnapshot.topology,
      RoleTopology(
        evidenceTypeArgumentRole = DeclarationRole.TypeParameter,
        resultTypeRole = DeclarationRole.TypeParameter,
        bodyReceiverRole = DeclarationRole.ContextualParameter,
        selectedMemberRole = SelectedMemberRole.GeneratedMethod
      )
    )

  test("allocates graph-local identities while equal source roles retain equal snapshots"):
    val first = createPlan("empty", "A", "inst", "Empty")
    val second = createPlan("empty", "A", "inst", "Empty")

    assertNotEquals(declarationBinderIds(first), declarationBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

  test("rejects every missing or illegal source role with a stable role-specific code"):
    val valid = Vector("empty", "A", "inst", "Empty")
    val expected = Vector(
      "METHOD_NAME_INVALID",
      "TYPE_PARAMETER_NAME_INVALID",
      "CONTEXTUAL_PARAMETER_NAME_INVALID",
      "CONTEXTUAL_TYPE_CONSTRUCTOR_INVALID"
    )

    valid.indices.foreach { index =>
      List(null, "", "not legal").foreach { invalid =>
        assertCode(createFrom(valid.updated(index, invalid)), expected(index))
      }
    }

  test("rejects the compiler-proven constructor and non-higher-kinded Type-parameter collapse"):
    assertCode(
      ParameterlessDelegatedForwardingPlan.create("empty", "A", "inst", "A"),
      "CONTEXTUAL_TYPE_CONSTRUCTOR_ROLE_COLLAPSE"
    )

  test("admits compiler-proven safe same-spelling cross-namespace controls"):
    val plans = List(
      createPlan("same", "A", "same", "Evidence"),
      createPlan("get", "A", "A", "Evidence"),
      createPlan("A", "A", "inst", "Evidence"),
      createPlan("Same", "A", "inst", "Same"),
      createPlan("get", "A", "Evidence", "Evidence")
    )

    assertEquals(plans.map(_.roleSnapshot.methodSourceName), List("same", "get", "A", "Same", "get"))

  test("Core accepts legal backticked roles for the neutral fresh-name boundary"):
    val plan = createPlan("`def`", "`type`", "`given`", "`class`")

    assertEquals(plan.roleSnapshot.methodSourceName, "`def`")
    assertEquals(plan.roleSnapshot.contextualTypeConstructorSourceName, "`class`")

  test("complete validation rejects every fabricated graph-edge corruption"):
    val valid = createPlan("empty", "A", "inst", "Empty")
    assertEquals(ParameterlessDelegatedForwardingPlan.validate(valid), Right(valid.roleSnapshot))
    assertEquals(
      ParameterlessDelegatedForwardingPlan.validate(null).left.toOption.map(_.code),
      Some("PLAN_MISSING")
    )

    val corruptions = List(
      new Plan(
        null,
        valid.typeParameter,
        valid.contextualParameter,
        valid.evidenceTypeArgument,
        valid.resultType,
        valid.body,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        null,
        valid.contextualParameter,
        valid.evidenceTypeArgument,
        valid.resultType,
        valid.body,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        valid.typeParameter,
        null,
        valid.evidenceTypeArgument,
        valid.resultType,
        valid.body,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        valid.typeParameter,
        valid.contextualParameter,
        null,
        valid.resultType,
        valid.body,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        valid.typeParameter,
        valid.contextualParameter,
        valid.evidenceTypeArgument,
        null,
        valid.body,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        valid.typeParameter,
        valid.contextualParameter,
        valid.evidenceTypeArgument,
        valid.resultType,
        null,
        valid.roleSnapshot
      ),
      new Plan(
        valid.methodIdentity,
        valid.typeParameter,
        valid.contextualParameter,
        valid.evidenceTypeArgument,
        valid.resultType,
        valid.body,
        null
      ),
      copyPlan(
        valid,
        contextualParameter = valid.contextualParameter.copy(binderId = valid.typeParameter.binderId)
      ),
      copyPlan(
        valid,
        evidenceTypeArgument = TypeParameterReference(
          valid.typeParameter.binderId,
          valid.typeParameter.displayName
        )
      ),
      copyPlan(
        valid,
        evidenceTypeArgument = TypeParameterReference(
          valid.contextualParameter.binderId,
          valid.typeParameter.displayName
        )
      ),
      copyPlan(
        valid,
        resultType = TypeParameterReference(
          valid.contextualParameter.binderId,
          valid.typeParameter.displayName
        )
      ),
      copyPlan(
        valid,
        body = valid.body.copy(receiver = ContextualReference(valid.typeParameter.binderId))
      ),
      copyPlan(
        valid,
        body = valid.body.copy(selectedMethodIdentity = new MethodIdentity("empty"))
      ),
      copyPlan(
        valid,
        roleSnapshot = valid.roleSnapshot.copy(methodSourceName = "other")
      )
    )

    corruptions.foreach { plan =>
      assertCode(ParameterlessDelegatedForwardingPlan.validate(plan).map(_ => plan), "PLAN_GRAPH_INVALID")
    }

  private def copyPlan(
      plan: Plan,
      contextualParameter: ContextualParameter = null,
      evidenceTypeArgument: TypeParameterReference = null,
      resultType: TypeParameterReference = null,
      body: ValidatedBody = null,
      roleSnapshot: RoleSnapshot = null
  ): Plan =
    new Plan(
      plan.methodIdentity,
      plan.typeParameter,
      Option(contextualParameter).getOrElse(plan.contextualParameter),
      Option(evidenceTypeArgument).getOrElse(plan.evidenceTypeArgument),
      Option(resultType).getOrElse(plan.resultType),
      Option(body).getOrElse(plan.body),
      Option(roleSnapshot).getOrElse(plan.roleSnapshot)
    )

  private def createFrom(values: Vector[String]) =
    ParameterlessDelegatedForwardingPlan.create(values(0), values(1), values(2), values(3))

  private def createPlan(
      method: String,
      typeParameter: String,
      contextualParameter: String,
      contextualConstructor: String
  ): Plan =
    ParameterlessDelegatedForwardingPlan
      .create(method, typeParameter, contextualParameter, contextualConstructor)
      .fold(problem => fail(problem.message), identity)

  private def assertPlan(
      plan: Plan,
      method: String,
      typeParameter: String,
      contextualParameter: String,
      contextualConstructor: String
  ): Unit =
    assertEquals(plan.methodIdentity.sourceName, method)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.contextualParameter.displayName, contextualParameter)
    assertNotEquals(plan.typeParameter.binderId, plan.contextualParameter.binderId)
    assertEquals(
      plan.contextualParameter.parameterType,
      Applied(SourceName(contextualConstructor), Vector(plan.evidenceTypeArgument))
    )
    assertEquals(plan.evidenceTypeArgument.binderId, plan.typeParameter.binderId)
    assertEquals(plan.resultType.binderId, plan.typeParameter.binderId)
    assertEquals(plan.body.receiver.binderId, plan.contextualParameter.binderId)
    assert(plan.body.selectedMethodIdentity eq plan.methodIdentity)

  private def declarationBinderIds(plan: Plan) =
    Vector(plan.typeParameter.binderId, plan.contextualParameter.binderId)

  private def assertCode(result: Either[ModelError, Plan], expected: String): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))
