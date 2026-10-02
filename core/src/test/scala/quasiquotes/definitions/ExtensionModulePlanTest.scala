package quasiquotes.definitions

import quasiquotes.definitions.ExtensionModulePlan.*

final class ExtensionModulePlanTest extends munit.FunSuite:
  test("creates canonical and fully renamed seven-role plans with the same topology"):
    val canonical = createPlan("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val renamed = createPlan(
      "operations",
      "Element",
      "left",
      "merge",
      "right",
      "evidence",
      "Combine"
    )

    assertNames(
      canonical,
      "syntax",
      "A",
      "a",
      "combine",
      "a1",
      "inst",
      "Monoid"
    )
    assertNames(
      renamed,
      "operations",
      "Element",
      "left",
      "merge",
      "right",
      "evidence",
      "Combine"
    )
    assertEquals(canonical.roleSnapshot.topology, renamed.roleSnapshot.topology)
    assertEquals(
      canonical.roleSnapshot.topology,
      RoleTopology(
        receiverTypeRole = DeclarationRole.TypeParameter,
        ordinaryTypeRole = DeclarationRole.TypeParameter,
        contextualConstructorRole = ExternalTypeRole.EvidenceConstructor,
        contextualArgumentRole = DeclarationRole.TypeParameter,
        resultTypeRole = DeclarationRole.TypeParameter,
        bodyReceiverRole = DeclarationRole.ContextualEvidence,
        selectedMemberRole = SelectedMemberRole.GeneratedMethod,
        bodyArgumentRoles = Vector(
          DeclarationRole.ExtensionReceiver,
          DeclarationRole.OrdinaryArgument
        )
      )
    )

  test("factory allocation is graph-local while role snapshots ignore raw BinderIds"):
    val first = createPlan("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val second = createPlan("syntax", "A", "a", "combine", "a1", "inst", "Monoid")

    assertNotEquals(declarationBinderIds(first), declarationBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

  test("rejects every missing or illegal source-name role with a stable code"):
    val valid = Vector("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
    val expectedCodes = Vector(
      "MODULE_NAME_INVALID",
      "TYPE_PARAMETER_NAME_INVALID",
      "RECEIVER_NAME_INVALID",
      "METHOD_NAME_INVALID",
      "ORDINARY_ARGUMENT_NAME_INVALID",
      "CONTEXTUAL_NAME_INVALID",
      "EVIDENCE_TYPE_CONSTRUCTOR_NAME_INVALID"
    )

    valid.indices.foreach { index =>
      assertCode(createFrom(valid.updated(index, "")), expectedCodes(index))
      assertCode(createFrom(valid.updated(index, null)), expectedCodes(index))
      assertCode(createFrom(valid.updated(index, "not legal")), expectedCodes(index))
    }

  test("rejects duplicate Term declarations and evidence-constructor lexical collapse"):
    assertCode(
      ExtensionModulePlan.create("syntax", "A", "same", "combine", "same", "inst", "Monoid"),
      "TERM_ROLE_NAMES_MUST_BE_DISTINCT"
    )
    assertCode(
      ExtensionModulePlan.create("syntax", "A", "same", "combine", "a1", "same", "Monoid"),
      "TERM_ROLE_NAMES_MUST_BE_DISTINCT"
    )
    assertCode(
      ExtensionModulePlan.create("syntax", "A", "a", "combine", "same", "same", "Monoid"),
      "TERM_ROLE_NAMES_MUST_BE_DISTINCT"
    )
    assertCode(
      ExtensionModulePlan.create("syntax", "A", "a", "combine", "a1", "inst", "A"),
      "EVIDENCE_TYPE_CONSTRUCTOR_ROLE_COLLAPSE"
    )

  test("admits proven cross-namespace and method-Term same-spelling controls"):
    val plan = createPlan("A", "A", "A", "A", "a1", "inst", "Monoid")

    assertEquals(plan.moduleDisplayName, "A")
    assertEquals(plan.typeParameter.displayName, "A")
    assertEquals(plan.receiverParameter.displayName, "A")
    assertEquals(plan.methodIdentity.sourceName, "A")

  test("Core accepts legal backticked source roles for neutral authoring to classify later"):
    val plan = createPlan(
      "`object`",
      "`type`",
      "`val`",
      "`def`",
      "`var`",
      "`given`",
      "`class`"
    )

    assertEquals(plan.roleSnapshot.moduleSourceName, "`object`")
    assertEquals(plan.roleSnapshot.evidenceConstructorSourceName, "`class`")

  private def createFrom(values: Vector[String]) =
    ExtensionModulePlan.create(
      values(0),
      values(1),
      values(2),
      values(3),
      values(4),
      values(5),
      values(6)
    )

  private def createPlan(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      ordinary: String,
      contextual: String,
      evidenceConstructor: String
  ): Plan =
    ExtensionModulePlan
      .create(
        module,
        typeParameter,
        receiver,
        method,
        ordinary,
        contextual,
        evidenceConstructor
      )
      .fold(problem => fail(problem.message), identity)

  private def assertNames(
      plan: Plan,
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      ordinary: String,
      contextual: String,
      evidenceConstructor: String
  ): Unit =
    assertEquals(plan.moduleDisplayName, module)
    assertEquals(plan.typeParameter.displayName, typeParameter)
    assertEquals(plan.receiverParameter.displayName, receiver)
    assertEquals(plan.methodIdentity.sourceName, method)
    assertEquals(plan.ordinaryArgument.displayName, ordinary)
    assertEquals(plan.contextualParameter.displayName, contextual)
    assertEquals(plan.roleSnapshot.evidenceConstructorSourceName, evidenceConstructor)

  private def declarationBinderIds(plan: Plan) =
    Vector(
      plan.typeParameter.binderId,
      plan.receiverParameter.binderId,
      plan.ordinaryArgument.binderId,
      plan.contextualParameter.binderId
    )

  private def assertCode(
      result: Either[ModelError, Plan],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(_) => fail(s"expected $expected")
