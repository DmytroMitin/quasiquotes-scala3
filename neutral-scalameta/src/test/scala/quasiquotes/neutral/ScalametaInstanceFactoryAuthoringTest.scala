package quasiquotes.neutral

import quasiquotes.definitions.InstanceFactoryPlan
import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaInstanceFactoryAuthoringTest extends munit.FunSuite:
  test("authors canonical and arbitrary-binder plans through the exact N017 round trip"):
    val rows = List(
      validPlan(),
      validPlan(
        ids = Vector(41, 73, 107, 211, 509),
        factoryName = "make",
        typeName = "Element",
        emptyCarrierName = "fallbackValue",
        functionCarrierName = "selection",
        targetName = "Choice",
        emptyMemberName = "fallback",
        combineMemberName = "select",
        firstNestedName = "left",
        secondNestedName = "right"
      ),
      validPlan(
        ids = Vector(900, 700, 500, 300, 100),
        factoryName = "build",
        typeName = "Value",
        emptyCarrierName = "zero",
        functionCarrierName = "append",
        targetName = "Reducer",
        emptyMemberName = "identity",
        combineMemberName = "reduce",
        firstNestedName = "head",
        secondNestedName = "tail"
      )
    )

    rows.foreach { expected =>
      val authored = author(expected)
      assert(allTrees(authored).forall(_.pos == Position.None))
      val projected = project(authored)
      assertEquals(projected.sourceSpan, None)
      assertRoleEquivalent(projected.plan, expected)
    }

  test("authors legal repeated spellings across non-conflicting namespaces and nested scopes"):
    val expected = validPlan(
      ids = Vector(11, 22, 33, 44, 55),
      factoryName = "shared",
      typeName = "Element",
      emptyCarrierName = "emptyInput",
      functionCarrierName = "combineInput",
      targetName = "shared",
      emptyMemberName = "shared",
      combineMemberName = "shared",
      firstNestedName = "emptyInput",
      secondNestedName = "right"
    )

    assertRoleEquivalent(project(author(expected)).plan, expected)

  test("fails closed for Core-valid names outside the fresh N017 spelling intersection"):
    val keywordRows = List(
      validPlan(factoryName = "`type`") -> "factory",
      validPlan(typeName = "`type`") -> "Type parameter",
      validPlan(emptyCarrierName = "`match`") -> "empty carrier",
      validPlan(functionCarrierName = "`given`") -> "function carrier",
      validPlan(targetName = "`enum`") -> "target",
      validPlan(emptyMemberName = "`override`") -> "empty member",
      validPlan(combineMemberName = "`case`") -> "combine member",
      validPlan(firstNestedName = "`val`") -> "first nested",
      validPlan(secondNestedName = "`var`") -> "second nested"
    )
    keywordRows.foreach { case (plan, clue) =>
      assertErrorCode(
        ScalametaInstanceFactoryAuthoring.author(plan),
        "NEUTRAL_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED",
        clue
      )
    }

  test("fails closed for Core-valid declaration spellings that N017 cannot resolve to five roles"):
    val rows = List(
      validPlan(typeName = "Same", targetName = "Same"),
      validPlan(emptyCarrierName = "same", functionCarrierName = "same"),
      validPlan(firstNestedName = "same", secondNestedName = "same"),
      validPlan(functionCarrierName = "shadowed", firstNestedName = "shadowed"),
      validPlan(emptyCarrierName = "shadowed", emptyMemberName = "shadowed"),
      validPlan(functionCarrierName = "shadowed", emptyMemberName = "shadowed"),
      validPlan(emptyCarrierName = "shadowed", combineMemberName = "shadowed"),
      validPlan(functionCarrierName = "shadowed", combineMemberName = "shadowed")
    )
    rows.foreach { plan =>
      assertErrorCode(
        ScalametaInstanceFactoryAuthoring.author(plan),
        "NEUTRAL_INSTANCE_FACTORY_AUTHORING_NAME_UNSUPPORTED"
      )
    }

  test("reports a stable missing category for a null root"):
    assertErrorCode(
      ScalametaInstanceFactoryAuthoring.author(null),
      "NEUTRAL_INSTANCE_FACTORY_AUTHORING_MISSING"
    )

  test("direct corruption controls retain N017 as the final structural and lexical oracle"):
    val authored = author(validPlan())
    val group = authored.paramClauseGroups.head
    val clause = group.paramClauses.head
    val List(emptyParameter, combineParameter) = clause.values: @unchecked
    val anonymous = authored.body.asInstanceOf[Term.NewAnonymous]
    val List(emptyMember: Defn.Def, combineMember: Defn.Def) = anonymous.templ.stats: @unchecked

    val wrongOuterOrder = withParameterGroups(
      authored,
      List(
        group.copy(paramClauses = List(clause.copy(values = List(combineParameter, emptyParameter))))
      )
    )
    val wrongMode = withParameterGroups(
      authored,
      List(
        group.copy(
          paramClauses = List(
            clause.copy(values = emptyParameter.copy(decltpe = Some(Type.Name("A"))) :: combineParameter :: Nil)
          )
        )
      )
    )
    val wrongTargetRole = authored.copy(
      decltpe = Some(Type.Apply(Type.Name("Target"), List(Type.Name("String"))))
    )
    val wrongOverrideOrder = withMembers(authored, List(combineMember, emptyMember))
    val wrongEmptyBody = withMembers(
      authored,
      List(emptyMember.copy(body = Term.Name("combineFunction")), combineMember)
    )
    val wrongCombineCallee = withMembers(
      authored,
      List(
        emptyMember,
        combineMember.copy(
          body = Term.Apply(
            Term.Name("emptyValue"),
            Term.ArgClause(List(Term.Name("x"), Term.Name("y")))
          )
        )
      )
    )
    val wrongArgumentOrder = withMembers(
      authored,
      List(
        emptyMember,
        combineMember.copy(
          body = Term.Apply(
            Term.Name("combineFunction"),
            Term.ArgClause(List(Term.Name("y"), Term.Name("x")))
          )
        )
      )
    )

    List(
      wrongOuterOrder -> "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
      wrongMode -> "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
      wrongTargetRole -> "TARGET_TYPE_ROLE_MISMATCH",
      wrongOverrideOrder -> "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
      wrongEmptyBody -> "EMPTY_BODY_ROLE_MISMATCH",
      wrongCombineCallee -> "COMBINE_CALLEE_ROLE_MISMATCH",
      wrongArgumentOrder -> "COMBINE_ARGUMENT_ROLE_MISMATCH"
    ).foreach { case (definition, code) =>
      assertProjectionCode(definition, code)
    }

  private def validPlan(
      ids: Vector[Int] = Vector(0, 1, 2, 3, 4),
      factoryName: String = "instance",
      typeName: String = "A",
      emptyCarrierName: String = "emptyValue",
      functionCarrierName: String = "combineFunction",
      targetName: String = "Target",
      emptyMemberName: String = "emptyValueMember",
      combineMemberName: String = "combineValues",
      firstNestedName: String = "x",
      secondNestedName: String = "y"
  ): Plan =
    val binders = ids.map(BinderId(_))
    val typeReference = TypeParameterReference(binders(0), typeName)
    InstanceFactoryPlan
      .create(
        factoryName,
        TypeParameter(binders(0), typeName),
        ByNameCarrier(
          binders(1),
          emptyCarrierName,
          ParameterMode.ByName,
          ValueType(typeReference)
        ),
        BinaryFunctionCarrier(
          binders(2),
          functionCarrierName,
          ParameterMode.ByValue,
          BinaryFunctionType(typeReference, typeReference, typeReference)
        ),
        Applied(SourceName(targetName), Vector(typeReference)),
        EmptyOverride(emptyMemberName, TermReference(binders(1))),
        CombineOverride(
          combineMemberName,
          NestedParameter(binders(3), firstNestedName, typeReference),
          NestedParameter(binders(4), secondNestedName, typeReference),
          typeReference,
          CombineBody(
            TermReference(binders(2)),
            Vector(TermReference(binders(3)), TermReference(binders(4)))
          )
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def author(plan: Plan): Defn.Def =
    ScalametaInstanceFactoryAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def project(definition: Defn.Def): ProjectedInstanceFactory =
    ScalametaInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(problem.message), identity)

  private def assertRoleEquivalent(actual: Plan, expected: Plan): Unit =
    assertEquals(actual.factoryDisplayName, expected.factoryDisplayName)
    assertEquals(actual.typeParameter.displayName, expected.typeParameter.displayName)
    assertEquals(actual.emptyValue.displayName, expected.emptyValue.displayName)
    assertEquals(actual.emptyValue.mode, expected.emptyValue.mode)
    assertEquals(actual.combineFunction.displayName, expected.combineFunction.displayName)
    assertEquals(actual.combineFunction.mode, expected.combineFunction.mode)
    assertEquals(actual.targetType.constructor, expected.targetType.constructor)
    assertEquals(actual.emptyOverride.memberDisplayName, expected.emptyOverride.memberDisplayName)
    assertEquals(actual.combineOverride.memberDisplayName, expected.combineOverride.memberDisplayName)
    assertEquals(
      actual.combineOverride.firstParameter.displayName,
      expected.combineOverride.firstParameter.displayName
    )
    assertEquals(
      actual.combineOverride.secondParameter.displayName,
      expected.combineOverride.secondParameter.displayName
    )
    assertEquals(
      Vector(
        actual.typeParameter.binderId,
        actual.emptyValue.binderId,
        actual.combineFunction.binderId,
        actual.combineOverride.firstParameter.binderId,
        actual.combineOverride.secondParameter.binderId
      ),
      Vector.tabulate(5)(BinderId(_))
    )
    assertEquals(actual.emptyValue.valueType.reference.binderId, BinderId(0))
    assertEquals(
      Vector(
        actual.combineFunction.functionType.firstArgument.binderId,
        actual.combineFunction.functionType.secondArgument.binderId,
        actual.combineFunction.functionType.result.binderId,
        actual.targetType.arguments.head.asInstanceOf[TypeParameterReference].binderId,
        actual.combineOverride.firstParameter.parameterType.binderId,
        actual.combineOverride.secondParameter.parameterType.binderId,
        actual.combineOverride.resultType.binderId
      ),
      Vector.fill(7)(BinderId(0))
    )
    assertEquals(actual.emptyOverride.body.binderId, BinderId(1))
    assertEquals(actual.combineOverride.body.callee.binderId, BinderId(2))
    assertEquals(
      actual.combineOverride.body.arguments.map(_.binderId),
      Vector(BinderId(3), BinderId(4))
    )

  private def withMembers(definition: Defn.Def, members: List[Stat]): Defn.Def =
    val anonymous = definition.body.asInstanceOf[Term.NewAnonymous]
    definition.copy(
      body = anonymous.copy(templ = anonymous.templ.copy(stats = members))
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

  private def assertProjectionCode(definition: Defn.Def, expected: String): Unit =
    assertEquals(
      ScalametaInstanceFactoryProjection.project(definition).left.toOption.map(_.code),
      Some(expected)
    )

  private def assertErrorCode[A](
      result: Either[ScalametaInstanceFactoryAuthoring.Error, A],
      expected: String,
      clue: String = ""
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(clue, result))

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
