package quasiquotes.neutral

import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaScopedContextualMethodAuthoringTest extends munit.FunSuite:
  test("authors canonical and arbitrary-binder plans through the complete three-role round trip"):
    val rows = List(
      validPlan(),
      validPlan(
        ids = Vector(9, 2, 17),
        methodName = "derive",
        firstTypeName = "Left",
        secondTypeName = "Right",
        upperBoundName = "Domain",
        contextualName = "evidence",
        constructorName = "Combine",
        memberName = "Result"
      ),
      validPlan(
        ids = Vector(17, 9, 2),
        methodName = "resolve",
        firstTypeName = "Input",
        secondTypeName = "Output",
        upperBoundName = "Element",
        contextualName = "service",
        constructorName = "Resolver",
        memberName = "Resolved"
      )
    )

    rows.foreach { expected =>
      val authored = author(expected)
      assert(allTrees(authored).forall(_.pos == Position.None), clues(expected.alphaKey))
      val projected = project(authored)
      assertEquals(projected.sourceSpan, None)
      assertRoleEquivalent(projected.plan, expected)
      ScalametaContextualMethodDispatch.project(authored) match
        case Right(ProjectedContextualMethodRoute.Scoped037(value)) =>
          assertEquals(value.sourceSpan, None)
          assertRoleEquivalent(value.plan, expected)
        case other => fail(s"expected Scoped037 dispatch, found $other")
    }

  test("preserves safe repeated spellings across distinguishable Term and Type roles"):
    val rows = List(
      validPlan(methodName = "ctx", contextualName = "ctx"),
      validPlan(firstTypeName = "Shared", contextualName = "Shared"),
      validPlan(firstTypeName = "Shared", memberName = "Shared")
    )

    rows.foreach(expected => assertRoleEquivalent(project(author(expected)).plan, expected))

  test("reports the stable missing category for a null root"):
    assertErrorCode(
      ScalametaScopedContextualMethodAuthoring.author(null),
      "NEUTRAL_SCOPED037_AUTHORING_MISSING"
    )

  test("rejects the Core-valid unequal-upper-bound shape outside the projector intersection"):
    assertErrorCode(
      ScalametaScopedContextualMethodAuthoring.author(
        validPlan(upperBoundName = "FirstBound", secondUpperBoundName = "SecondBound")
      ),
      "NEUTRAL_SCOPED037_AUTHORING_STRUCTURE_UNSUPPORTED"
    )

  test("fails closed when external Type spellings collapse local Type-parameter roles"):
    val rows = List(
      validPlan(firstTypeName = "A", upperBoundName = "A"),
      validPlan(secondTypeName = "B", upperBoundName = "B"),
      validPlan(firstTypeName = "A", constructorName = "A"),
      validPlan(secondTypeName = "B", constructorName = "B")
    )

    rows.foreach { plan =>
      assertErrorCode(
        ScalametaScopedContextualMethodAuthoring.author(plan),
        "NEUTRAL_SCOPED037_AUTHORING_LEXICAL_ROLE_UNSUPPORTED"
      )
    }

  test("fails closed for Core-valid backticked spellings decoded by fresh Scalameta names"):
    val rows = List(
      validPlan(methodName = "`type`"),
      validPlan(firstTypeName = "`type`"),
      validPlan(secondTypeName = "`match`"),
      validPlan(upperBoundName = "`given`"),
      validPlan(contextualName = "`using`"),
      validPlan(constructorName = "`enum`"),
      validPlan(memberName = "`export`")
    )

    rows.foreach { plan =>
      assertErrorCode(
        ScalametaScopedContextualMethodAuthoring.author(plan),
        "NEUTRAL_SCOPED037_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  test("direct tree corruptions retain the scoped projector as the topology oracle"):
    val authored = author(validPlan())
    val group = authored.paramClauseGroups.head
    val List(first, second) = group.tparamClause.values: @unchecked
    val contextualClause = group.paramClauses.head
    val contextualParameter = contextualClause.values.head
    val Type.Refine(Some(base: Type.Apply), List(member: Defn.Type)) =
      authored.decltpe.get: @unchecked

    val oneTypeParameter = withParameterGroups(
      authored,
      List(group.copy(tparamClause = Type.ParamClause(List(first))))
    )
    val reversedTypeParameters = withParameterGroups(
      authored,
      List(group.copy(tparamClause = Type.ParamClause(List(second, first))))
    )
    val contextualNotUsing = withParameterGroups(
      authored,
      List(group.copy(paramClauses = List(contextualClause.copy(mod = None))))
    )
    val reversedContextualArguments = withParameterGroups(
      authored,
      List(
        group.copy(
          paramClauses = List(
            contextualClause.copy(
              values = List(
                contextualParameter.copy(
                  decltpe = Some(
                    Type.Apply(
                      Type.Name("Context"),
                      Type.ArgClause(List(Type.Name("B"), Type.Name("A")))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    val wrongRefinementBase = withResult(
      authored,
      Type.Refine(
        Some(
          Type.Apply(
            Type.Name("Other"),
            Type.ArgClause(List(Type.Name("A"), Type.Name("B")))
          )
        ),
        Stat.Block(List(member))
      )
    )
    val noRefinementMembers = withResult(
      authored,
      Type.Refine(Some(base), Stat.Block(Nil))
    )
    val wrongMemberTopology = withResult(
      authored,
      Type.Refine(
        Some(base),
        Stat.Block(
          List(
            member.copy(
              body = Type.Name("String")
            )
          )
        )
      )
    )
    val wrongSelectedPrefix = withResult(
      authored,
      Type.Refine(
        Some(base),
        Stat.Block(
          List(
            member.copy(
              body = Type.Select(Term.Name("other"), Type.Name("Out"))
            )
          )
        )
      )
    )
    val wrongSelectedMember = withResult(
      authored,
      Type.Refine(
        Some(base),
        Stat.Block(
          List(
            member.copy(
              body = Type.Select(Term.Name("ctx"), Type.Name("Result"))
            )
          )
        )
      )
    )
    val wrongBodyName = withBody(authored, Term.Name("other"))
    val wrongBodyTopology = withBody(
      authored,
      Term.Select(Term.Name("ctx"), Term.Name("value"))
    )

    List(
      oneTypeParameter -> "NEUTRAL_SCOPED037_TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
      reversedTypeParameters -> "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH",
      contextualNotUsing -> "NEUTRAL_SCOPED037_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      reversedContextualArguments -> "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH",
      wrongRefinementBase -> "NEUTRAL_SCOPED037_REFINEMENT_BASE_MISMATCH",
      noRefinementMembers -> "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      wrongMemberTopology -> "NEUTRAL_SCOPED037_REFINEMENT_RHS_UNSUPPORTED",
      wrongSelectedPrefix -> "NEUTRAL_SCOPED037_SELECTED_PREFIX_UNBOUND",
      wrongSelectedMember -> "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_NAME_MISMATCH",
      wrongBodyName -> "NEUTRAL_SCOPED037_BODY_BINDER_MISMATCH",
      wrongBodyTopology -> "NEUTRAL_SCOPED037_BODY_UNSUPPORTED"
    ).foreach { case (definition, code) => assertProjectionCode(definition, code) }

  test("a parsed control proves positioned provenance remains distinct from fresh success"):
    val positioned = Scala3(
      "def scoped[A <: Bound, B <: Bound](using ctx: Context[A, B]): Context[A, B] { type Out = ctx.Out } = ctx"
    ).parse[Stat].get.asInstanceOf[Defn.Def]

    assert(project(positioned).sourceSpan.nonEmpty)

  private def validPlan(
      ids: Vector[Int] = Vector(0, 1, 2),
      methodName: String = "scoped",
      firstTypeName: String = "A",
      secondTypeName: String = "B",
      upperBoundName: String = "Bound",
      secondUpperBoundName: String | Null = null,
      contextualName: String = "ctx",
      constructorName: String = "Context",
      memberName: String = "Out"
  ): ScopedContextualMethodPlan =
    val binders = ids.map(BinderId(_))
    val first = ScopedTypeParameter(binders(0), firstTypeName, SourceName(upperBoundName))
    val second = ScopedTypeParameter(
      binders(1),
      secondTypeName,
      SourceName(Option(secondUpperBoundName).getOrElse(upperBoundName))
    )
    val applied = Applied(
      SourceName(constructorName),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(second.binderId, second.displayName)
      )
    )
    val result = Refinement(
      applied,
      Vector(
        ScopedTypeAlias(
          memberName,
          DirectStableSelected(binders(2), memberName)
        )
      )
    )
    ScopedContextualMethodPlan
      .create(
        methodName,
        Vector(first, second),
        binders(2),
        contextualName,
        applied,
        result,
        binders(2)
      )
      .fold(problem => fail(problem.message), identity)

  private def author(plan: ScopedContextualMethodPlan): Defn.Def =
    ScalametaScopedContextualMethodAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def project(definition: Defn.Def): ProjectedScopedContextualMethod =
    ScalametaScopedContextualMethodProjection
      .project(definition)
      .fold(problem => fail(problem.message), identity)

  private def assertRoleEquivalent(
      actual: ScopedContextualMethodPlan,
      expected: ScopedContextualMethodPlan
  ): Unit =
    assertEquals(roleSnapshot(actual), roleSnapshot(expected))

  private def roleSnapshot(plan: ScopedContextualMethodPlan): Vector[Any] =
    val declarations = Vector(
      plan.typeParameters(0).binderId,
      plan.typeParameters(1).binderId,
      plan.contextualTermBinderId
    )
    def roleOf(binder: BinderId): Int = declarations.indexOf(binder)
    val Applied(SourceName(contextualConstructor), contextualArguments) =
      plan.contextualType: @unchecked
    val Applied(SourceName(refinementConstructor), refinementArguments) =
      plan.resultType.base: @unchecked
    Vector(
      plan.methodDisplayName,
      plan.typeParameters(0).displayName,
      plan.typeParameters(1).displayName,
      plan.typeParameters(0).upperBound.asInstanceOf[SourceName].value,
      plan.typeParameters(1).upperBound.asInstanceOf[SourceName].value,
      plan.contextualDisplayName,
      contextualConstructor,
      contextualArguments.map {
        case TypeParameterReference(binder, _) => roleOf(binder)
        case _ => -1
      },
      refinementConstructor,
      refinementArguments.map {
        case TypeParameterReference(binder, _) => roleOf(binder)
        case _ => -1
      },
      plan.refinementMember.memberName,
      roleOf(plan.selectedResult.prefixTermBinderId),
      plan.selectedResult.memberExpectation,
      roleOf(plan.bodyTermBinderId)
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

  private def withResult(definition: Defn.Def, result: Type): Defn.Def =
    Defn.Def(
      definition.mods,
      definition.name,
      definition.paramClauseGroups,
      Some(result),
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
      ScalametaScopedContextualMethodProjection.project(definition).left.toOption.map(_.code),
      Some(expected),
      clues(definition)
    )

  private def assertErrorCode[A](
      result: Either[ScalametaScopedContextualMethodAuthoring.Error, A],
      expected: String
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(result))

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
