package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.ExtensionModulePlan
import quasiquotes.definitions.ExtensionModulePlan.Plan as CorePlan
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.Plan as BoundedPlan

class ExtensionModulePlanU024AdapterTest extends munit.FunSuite:
  private final case class Names(
      module: String,
      typeParameter: String,
      receiver: String,
      method: String,
      argument: String,
      evidence: String,
      evidenceType: String
  ):
    val source =
      s"""object $module:
         |  extension [$typeParameter]($receiver: $typeParameter)
         |    def $method($argument: $typeParameter)(using $evidence: $evidenceType[$typeParameter]): $typeParameter =
         |      $evidence.$method($receiver, $argument)
         |""".stripMargin

  private val canonical =
    Names("syntax", "A", "a", "combine", "a1", "inst", "Monoid")
  private val renamed =
    Names("operations", "Element", "left", "merge", "right", "evidence", "Combine")

  test("mechanically adapts canonical and renamed Core plans without rebuilding semantic values") {
    Vector(canonical, renamed).foreach { names =>
      val core = corePlan(names)
      val before = coreSnapshot(core)
      val bounded = adapt(core)

      assertEquals(bounded.moduleDisplayName, core.moduleDisplayName)
      assertEquals(bounded.methodDisplayName, core.methodIdentity.sourceName)
      assertEquals(bounded.typeParameter.binderId, core.typeParameter.binderId)
      assertEquals(bounded.receiverParameter.binderId, core.receiverParameter.binderId)
      assertEquals(bounded.ordinaryArgument.binderId, core.ordinaryArgument.binderId)
      assertEquals(bounded.contextualParameter.binderId, core.contextualParameter.binderId)
      assert(bounded.receiverParameter.parameterType eq core.receiverParameter.parameterType)
      assert(bounded.ordinaryArgument.parameterType eq core.ordinaryArgument.parameterType)
      assert(bounded.contextualParameter.parameterType eq core.contextualParameter.parameterType)
      assert(bounded.resultType eq core.resultType)
      assertEquals(bounded.body.receiver.binderId, core.body.receiver.binderId)
      assertEquals(
        bounded.body.selectedMethodDisplayName,
        core.body.selectedMethodIdentity.sourceName
      )
      assertEquals(
        bounded.body.arguments.map(_.binderId),
        core.body.arguments.map(_.binderId)
      )
      assertEquals(coreSnapshot(core), before)
    }
  }

  test("accepts repeated alpha-equivalent Core plans without assuming BinderId ordinals") {
    val first = corePlan(canonical)
    val second = corePlan(canonical)
    assertNotEquals(coreBinderIds(first), coreBinderIds(second))
    assertEquals(first.roleSnapshot, second.roleSnapshot)

    val firstBounded = adapt(first)
    val secondBounded = adapt(second)
    assertEquals(firstBounded.methodDisplayName, secondBounded.methodDisplayName)
    assertEquals(coreBinderIds(first), boundedBinderIds(firstBounded))
    assertEquals(coreBinderIds(second), boundedBinderIds(secondBounded))

    withContext {
      val firstResult = lower(first, "<quasiquotes-generated:u043-repeat-1>")
      val secondResult = lower(second, "<quasiquotes-generated:u043-repeat-2>")
      assertEquals(firstResult.generatedSource, secondResult.generatedSource)
      assertEquals(structure(firstResult.tree), structure(secondResult.tree))
    }
  }

  test("matches direct U024 plan and generated-origin lowering for canonical and renamed plans") {
    withContext {
      Vector(canonical, renamed).zipWithIndex.foreach { case (names, index) =>
        val core = corePlan(names)
        val adapted = adapt(core)
        val direct = directPlan(core)
        assertEquals(boundedValues(adapted), boundedValues(direct))

        val virtualName = s"<quasiquotes-generated:u043-$index>"
        val throughAdapter = lower(core, virtualName)
        val directResult = BoundedExtensionModuleGeneratedOriginAdapter
          .lower(direct, virtualName)
          .fold(problem => fail(problem.message), identity)

        assertEquals(throughAdapter.generatedSource, names.source)
        assertEquals(throughAdapter.generatedSource, directResult.generatedSource)
        assertEquals(throughAdapter.virtualSourceName, directResult.virtualSourceName)
        assertEquals(
          throughAdapter.sourceFile.content.mkString,
          directResult.sourceFile.content.mkString
        )
        assertEquals(structure(throughAdapter.tree), structure(directResult.tree))

        val positioned = BoundedExtensionModuleUntypedLowerer.allTrees(throughAdapter.tree)
        assertEquals(positioned.size, 21)
        positioned.foreach { tree =>
          assert(tree.source.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.source.path, virtualName)
          assertEquals(tree.source.content.mkString, names.source)
          assert(tree.span.exists, clues(tree.getClass.getSimpleName))
          assert(tree.span.start >= throughAdapter.tree.span.start)
          assert(tree.span.end <= throughAdapter.tree.span.end)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }

        val sourceFree = BoundedExtensionModuleUntypedLowerer
          .lower(adapted)
          .fold(problem => fail(problem.message), identity)
        val sourceFreeTrees = BoundedExtensionModuleUntypedLowerer.allTrees(sourceFree)
        assertEquals(sourceFreeTrees.size, 21)
        assert(sourceFreeTrees.forall(tree => !tree.source.exists && !tree.span.exists))
        assert(sourceFreeTrees.forall(_.symbol == NoSymbol))
        assert(sourceFreeTrees.forall(!_.isInstanceOf[untpd.TypedSplice]))
      }
    }
  }

  test("contains null malformed Core bounded-validation and generated-origin failures") {
    assertError(ExtensionModulePlanU024Adapter.adapt(null), "PLAN_REQUIRED")

    val valid = corePlan(canonical)
    val missingMethod = new CorePlan(
      valid.moduleDisplayName,
      null,
      valid.typeParameter,
      valid.receiverParameter,
      valid.ordinaryArgument,
      valid.contextualParameter,
      valid.resultType,
      valid.body,
      valid.roleSnapshot
    )
    assertError(
      ExtensionModulePlanU024Adapter.adapt(missingMethod),
      "CORE_PLAN_MALFORMED"
    )

    val collidingOrdinary = new CorePlan(
      valid.moduleDisplayName,
      valid.methodIdentity,
      valid.typeParameter,
      valid.receiverParameter,
      valid.ordinaryArgument.copy(binderId = valid.receiverParameter.binderId),
      valid.contextualParameter,
      valid.resultType,
      valid.body,
      valid.roleSnapshot
    )
    assertError(
      ExtensionModulePlanU024Adapter.adapt(collidingOrdinary),
      "BOUNDED_PLAN_ADAPTATION_FAILED"
    )

    val illegalCore = ExtensionModulePlan.create(
      canonical.module,
      canonical.typeParameter,
      canonical.receiver,
      canonical.method,
      canonical.receiver,
      canonical.evidence,
      canonical.evidenceType
    )
    assertEquals(
      illegalCore.left.toOption.map(_.code),
      Some("TERM_ROLE_NAMES_MUST_BE_DISTINCT")
    )

    withContext {
      assertError(
        ExtensionModulePlanU024Adapter.lowerGenerated(
          valid,
          " ordinary.scala"
        ),
        "GENERATED_ORIGIN_LOWERING_FAILED"
      )
    }
  }

  test("keeps Core input immutable and returns fresh raw and generated-origin graphs") {
    withContext {
      val core = corePlan(canonical)
      val before = coreSnapshot(core)
      val firstAdapted = adapt(core)
      val secondAdapted = adapt(core)
      assert(!(firstAdapted eq secondAdapted))

      val first = lower(core, "<quasiquotes-generated:u043-fresh>")
      val second = lower(core, "<quasiquotes-generated:u043-fresh>")
      assert(!(first.tree eq second.tree))
      assert(!(first.sourceFile eq second.sourceFile))
      val firstTrees = BoundedExtensionModuleUntypedLowerer.allTrees(first.tree)
      val secondTrees = BoundedExtensionModuleUntypedLowerer.allTrees(second.tree)
      assertEquals(firstTrees.size, secondTrees.size)
      assert(firstTrees.zip(secondTrees).forall { case (left, right) => !(left eq right) })
      assertEquals(first.generatedSource, second.generatedSource)
      assertEquals(structure(first.tree), structure(second.tree))
      assertEquals(coreSnapshot(core), before)
    }
  }

  private def corePlan(names: Names): CorePlan =
    ExtensionModulePlan
      .create(
        names.module,
        names.typeParameter,
        names.receiver,
        names.method,
        names.argument,
        names.evidence,
        names.evidenceType
      )
      .fold(problem => fail(problem.message), identity)

  private def adapt(plan: CorePlan): BoundedPlan =
    ExtensionModulePlanU024Adapter
      .adapt(plan)
      .fold(problem => fail(problem.message), identity)

  private def lower(
      plan: CorePlan,
      virtualSourceName: String
  )(using Context): GeneratedOriginDefinitionResult =
    ExtensionModulePlanU024Adapter
      .lowerGenerated(plan, virtualSourceName)
      .fold(problem => fail(problem.message), identity)

  private def directPlan(core: CorePlan): BoundedPlan =
    import BoundedExtensionModulePlan.*
    BoundedExtensionModulePlan
      .create(
        core.moduleDisplayName,
        core.methodIdentity.sourceName,
        TypeParameter(core.typeParameter.binderId, core.typeParameter.displayName),
        ReceiverParameter(
          core.receiverParameter.binderId,
          core.receiverParameter.displayName,
          core.receiverParameter.parameterType
        ),
        OrdinaryArgument(
          core.ordinaryArgument.binderId,
          core.ordinaryArgument.displayName,
          core.ordinaryArgument.parameterType
        ),
        ContextualParameter(
          core.contextualParameter.binderId,
          core.contextualParameter.displayName,
          core.contextualParameter.parameterType
        ),
        core.resultType,
        DelegatedBody(
          BodyTermReference(core.body.receiver.binderId),
          core.body.selectedMethodIdentity.sourceName,
          core.body.arguments.map(reference => BodyTermReference(reference.binderId))
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def coreBinderIds(plan: CorePlan) =
    Vector(
      plan.typeParameter.binderId,
      plan.receiverParameter.binderId,
      plan.ordinaryArgument.binderId,
      plan.contextualParameter.binderId
    )

  private def boundedBinderIds(plan: BoundedPlan) =
    Vector(
      plan.typeParameter.binderId,
      plan.receiverParameter.binderId,
      plan.ordinaryArgument.binderId,
      plan.contextualParameter.binderId
    )

  private def coreSnapshot(plan: CorePlan) =
    (
      plan.roleSnapshot,
      coreBinderIds(plan),
      plan.receiverParameter.parameterType,
      plan.ordinaryArgument.parameterType,
      plan.contextualParameter.parameterType,
      plan.resultType,
      plan.body.receiver.binderId,
      plan.body.selectedMethodIdentity.sourceName,
      plan.body.arguments.map(_.binderId)
    )

  private def boundedSnapshot(plan: BoundedPlan) =
    (
      plan.moduleDisplayName,
      plan.methodDisplayName,
      plan.typeParameter.displayName,
      plan.receiverParameter.displayName,
      plan.ordinaryArgument.displayName,
      plan.contextualParameter.displayName,
      plan.receiverParameter.parameterType,
      plan.ordinaryArgument.parameterType,
      plan.contextualParameter.parameterType,
      plan.resultType,
      plan.body.selectedMethodDisplayName
    )

  private def boundedValues(plan: BoundedPlan) =
    (
      boundedSnapshot(plan),
      boundedBinderIds(plan),
      plan.body.receiver,
      plan.body.arguments
    )

  private def assertError[A](
      result: Either[ExtensionModulePlanU024Adapter.Error, A],
      expectedCode: String
  ): Unit =
    result match
      case Left(problem) =>
        assertEquals(problem.code, expectedCode)
        assert(problem.detail.nonEmpty)
        assert(problem.message.nonEmpty)
      case Right(_) => fail(s"expected $expectedCode")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.ModuleDef =>
        s"ModuleDef(${value.name},${value.mods.flags},${structure(value.impl)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${value.derived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.ExtMethods =>
        s"ExtMethods(${value.paramss.map(_.map(structure))},${value.methods.map(structure)})"
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Apply =>
        s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
