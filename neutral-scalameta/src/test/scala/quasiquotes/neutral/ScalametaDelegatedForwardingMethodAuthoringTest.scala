package quasiquotes.neutral

import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan
import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaDelegatedForwardingMethodAuthoringTest extends munit.FunSuite:
  test("authors canonical and arbitrary-binder plans through the exact three-role round trip"):
    val rows = List(
      validPlan(),
      validPlan(
        ids = Vector(9, 2, 17),
        methodName = "render",
        typeName = "Element",
        ordinaryName = "value",
        contextualName = "evidence",
        contextualConstructorName = "Display",
        resultName = "Text"
      ),
      validPlan(
        ids = Vector(17, 9, 2),
        methodName = "combine",
        typeName = "Item",
        ordinaryName = "input",
        contextualName = "service",
        contextualConstructorName = "Combiner",
        resultName = "Output"
      )
    )

    rows.foreach { expected =>
      val authored = author(expected)
      assert(allTrees(authored).forall(_.pos == Position.None), clues(expected))
      val projected = project(authored)
      assertEquals(projected.sourceSpan, None)
      assertRoleEquivalent(projected.plan, expected)
    }

  test("preserves safe repeated spellings across distinct source roles"):
    val rows = List(
      validPlan(methodName = "value", ordinaryName = "value"),
      validPlan(methodName = "ctx", contextualName = "ctx"),
      validPlan(typeName = "Shared", ordinaryName = "Shared"),
      validPlan(typeName = "Shared", contextualName = "Shared")
    )

    rows.foreach(expected => assertRoleEquivalent(project(author(expected)).plan, expected))

  test("reports the stable missing category for a null root"):
    assertErrorCode(
      ScalametaDelegatedForwardingMethodAuthoring.author(null),
      "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_MISSING"
    )

  test("fails closed when source spelling collapses distinct lexical roles"):
    val rows = List(
      validPlan(ordinaryName = "same", contextualName = "same"),
      validPlan(typeName = "Same", contextualConstructorName = "Same"),
      validPlan(typeName = "Same", resultName = "Same")
    )

    rows.foreach { plan =>
      assertErrorCode(
        ScalametaDelegatedForwardingMethodAuthoring.author(plan),
        "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_LEXICAL_ROLE_UNSUPPORTED"
      )
    }

  test("fails closed for Core-valid backticked spellings that the 043 value projector decodes"):
    val rows = List(
      validPlan(methodName = "`type`"),
      validPlan(typeName = "`type`"),
      validPlan(ordinaryName = "`val`"),
      validPlan(contextualName = "`given`"),
      validPlan(contextualConstructorName = "`enum`"),
      validPlan(resultName = "`match`")
    )

    rows.foreach { plan =>
      assertErrorCode(
        ScalametaDelegatedForwardingMethodAuthoring.author(plan),
        "NEUTRAL_DELEGATED_FORWARDING_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  test("direct tree corruptions retain the existing 043 projector as final topology oracle"):
    val authored = author(validPlan())
    val group = authored.paramClauseGroups.head
    val List(ordinaryClause, contextualClause) = group.paramClauses: @unchecked
    val ordinaryParameter = ordinaryClause.values.head
    val contextualParameter = contextualClause.values.head

    val wrongOrder = withParameterGroups(
      authored,
      List(group.copy(paramClauses = List(contextualClause, ordinaryClause)))
    )
    val contextualNotUsing = withParameterGroups(
      authored,
      List(group.copy(paramClauses = List(ordinaryClause, contextualClause.copy(mod = None))))
    )
    val wrongOrdinaryType = withParameterGroups(
      authored,
      List(
        group.copy(
          paramClauses = List(
            ordinaryClause.copy(values = List(ordinaryParameter.copy(decltpe = Some(Type.Name("Other"))))),
            contextualClause
          )
        )
      )
    )
    val wrongContextualArgument = withParameterGroups(
      authored,
      List(
        group.copy(
          paramClauses = List(
            ordinaryClause,
            contextualClause.copy(
              values = List(
                contextualParameter.copy(
                  decltpe = Some(
                    Type.Apply(Type.Name("Context"), Type.ArgClause(List(Type.Name("Other"))))
                  )
                )
              )
            )
          )
        )
      )
    )
    val wrongReceiver = withBody(
      authored,
      Term.Apply(
        Term.Select(Term.Name("other"), Term.Name("forward")),
        Term.ArgClause(List(Term.Name("value")))
      )
    )
    val wrongSelected = withBody(
      authored,
      Term.Apply(
        Term.Select(Term.Name("ctx"), Term.Name("other")),
        Term.ArgClause(List(Term.Name("value")))
      )
    )
    val wrongArgument = withBody(
      authored,
      Term.Apply(
        Term.Select(Term.Name("ctx"), Term.Name("forward")),
        Term.ArgClause(List(Term.Name("other")))
      )
    )
    val wrongArity = withBody(
      authored,
      Term.Apply(
        Term.Select(Term.Name("ctx"), Term.Name("forward")),
        Term.ArgClause(List(Term.Name("value"), Term.Name("value")))
      )
    )
    val wrongTopology = withBody(authored, Term.Name("ctx"))

    List(
      wrongOrder -> "ORDINARY_CLAUSE_UNSUPPORTED",
      contextualNotUsing -> "CONTEXTUAL_CLAUSE_UNSUPPORTED",
      wrongOrdinaryType -> "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH",
      wrongContextualArgument -> "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
      wrongReceiver -> "BODY_RECEIVER_BINDER_MISMATCH",
      wrongSelected -> "BODY_SELECTED_METHOD_MISMATCH",
      wrongArgument -> "BODY_ARGUMENT_BINDER_MISMATCH",
      wrongArity -> "BODY_ARGUMENT_TOPOLOGY_UNSUPPORTED",
      wrongTopology -> "BODY_APPLICATION_UNSUPPORTED"
    ).foreach { case (definition, code) => assertProjectionCode(definition, code) }

  test("a parsed control proves that positioned source is not None provenance"):
    val positioned = Scala3(
      "def forward[A](value: A)(using ctx: Context[A]): Result = ctx.forward(value)"
    ).parse[Stat].get.asInstanceOf[Defn.Def]

    assert(project(positioned).sourceSpan.nonEmpty)

  private def validPlan(
      ids: Vector[Int] = Vector(0, 1, 2),
      methodName: String = "forward",
      typeName: String = "A",
      ordinaryName: String = "value",
      contextualName: String = "ctx",
      contextualConstructorName: String = "Context",
      resultName: String = "Result"
  ): Plan =
    val binders = ids.map(BinderId(_))
    val typeReference = TypeParameterReference(binders(0), typeName)
    DelegatedForwardingMethodPlan
      .create(
        methodName,
        TypeParameter(binders(0), typeName),
        OrdinaryParameter(binders(1), ordinaryName, typeReference),
        ContextualParameter(
          binders(2),
          contextualName,
          Applied(SourceName(contextualConstructorName), Vector(typeReference))
        ),
        SourceName(resultName),
        ForwardingBody(
          ContextualReference(binders(2)),
          methodName,
          OrdinaryReference(binders(1))
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Def =
    ScalametaDelegatedForwardingMethodAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def project(definition: Defn.Def): ProjectedDelegatedForwardingMethod =
    ScalametaDelegatedForwardingMethodProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertRoleEquivalent(actual: Plan, expected: Plan): Unit =
    assertEquals(roleSnapshot(actual), roleSnapshot(expected))

  private def roleSnapshot(plan: Plan): Vector[Any] =
    val declarations = Vector(
      plan.typeParameter.binderId,
      plan.ordinaryParameter.binderId,
      plan.contextualParameter.binderId
    )
    def roleOf(binder: BinderId): Int = declarations.indexOf(binder)
    val Applied(SourceName(constructor), Vector(contextualReference: TypeParameterReference)) =
      plan.contextualParameter.parameterType: @unchecked
    Vector(
      plan.methodIdentity.sourceName,
      plan.typeParameter.displayName,
      plan.ordinaryParameter.displayName,
      plan.contextualParameter.displayName,
      constructor,
      plan.resultType.value,
      roleOf(plan.ordinaryParameter.parameterType.binderId),
      roleOf(contextualReference.binderId),
      roleOf(plan.body.receiver.binderId),
      plan.body.selectedMethodIdentity.sourceName,
      roleOf(plan.body.argument.binderId)
    )

  private def withParameterGroups(
      definition: Defn.Def,
      groups: List[Member.ParamClauseGroup]
  ): Defn.Def =
    Defn.Def(
      definition.mods,
      definition.name,
      groups,
      definition.decltpe,
      definition.body
    )

  private def withBody(definition: Defn.Def, body: Term): Defn.Def =
    Defn.Def(
      definition.mods,
      definition.name,
      definition.paramClauseGroups,
      definition.decltpe,
      body
    )

  private def assertProjectionCode(definition: Defn.Def, expected: String): Unit =
    assertEquals(
      ScalametaDelegatedForwardingMethodProjection.project(definition).left.toOption.map(_.code),
      Some(expected)
    )

  private def assertErrorCode[A](
      result: Either[ScalametaDelegatedForwardingMethodAuthoring.Error, A],
      expected: String
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
