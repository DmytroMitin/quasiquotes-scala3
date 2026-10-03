package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*

class ValueInstanceFactoryPlanUntypedLowererTest extends munit.FunSuite:
  private final case class Names(
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String
  ):
    val source =
      s"def $factory[$typeParameter]($carrier: $typeParameter): $target[$typeParameter] = new $target[$typeParameter] { override val $member: $typeParameter = $carrier }"

  private val canonical = Names("instance", "A", "valueValue", "HasValue", "value")
  private val renamed = Names("make", "Element", "elementCarrier", "Container", "element")

  test("lowers canonical and renamed plans to fresh parser-equivalent source-free trees") {
    withContext {
      Vector(canonical, renamed).foreach { names =>
        val plan = validPlan(names)
        val snapshot = plan.roleSnapshot
        val first = lower(plan)
        val second = lower(plan)

        assertEquals(structure(first), structure(parseOne(names.source)))
        assertEquals(structure(second), structure(first))
        assert(!(first eq second))
        assert(
          allTrees(first).zip(allTrees(second)).forall { case (left, right) => !(left eq right) }
        )
        assertEquals(plan.roleSnapshot, snapshot)
        assertSourceFree(first)
        assertEquals(
          ValueInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, first),
          Right(())
        )
      }
    }
  }

  test("reuses validated graph roles and fails closed for missing or corrupt plans") {
    withContext {
      assertCode(ValueInstanceFactoryPlanUntypedLowerer.lower(null), "PLAN_REQUIRED")

      val valid = validPlan(canonical)
      val wrongTypeReference = TypeParameterReference(
        valid.strictCarrier.binderId,
        valid.typeParameter.displayName
      )
      val corruptions = List(
        copyPlan(valid, strictCarrier = valid.strictCarrier.copy(parameterType = wrongTypeReference)),
        copyPlan(valid, resultTarget = Applied(SourceName(canonical.target), Vector(wrongTypeReference))),
        copyPlan(valid, anonymousParentTarget = Applied(SourceName("Other"), valid.resultTarget.arguments)),
        copyPlan(valid, valueOverride = valid.valueOverride.copy(valueType = wrongTypeReference)),
        copyPlan(valid, valueOverride = valid.valueOverride.copy(body = TermReference(valid.typeParameter.binderId))),
        copyPlan(valid, roleSnapshot = valid.roleSnapshot.copy(memberSourceName = "other"))
      )
      corruptions.foreach { corrupt =>
        assertCode(ValueInstanceFactoryPlanUntypedLowerer.lower(corrupt), "PLAN_INVALID")
      }
    }
  }

  test("decodes Core-valid backticked source roles without inventing new identities") {
    withContext {
      val plan = ValueInstanceFactoryPlan
        .create("`def`", "`type`", "`val`", "`class`", "`object`")
        .fold(problem => fail(problem.message), identity)
      val raw = lower(plan)

      assertEquals(raw.name.toString, "def")
      assertEquals(raw.leadingTypeParams.head.name.toString, "type")
      assertEquals(raw.trailingParamss.head.head.name.toString, "val")
      assertEquals(raw.rhs.asInstanceOf[untpd.New].tpt.asInstanceOf[untpd.Template]
        .body.head.asInstanceOf[untpd.ValDef].name.toString, "object")
      assertSourceFree(raw)
    }
  }

  test("candidate validation rejects role substitution and parser provenance contamination") {
    withContext {
      val plan = validPlan(canonical)
      val raw = lower(plan)
      val template = raw.rhs.asInstanceOf[untpd.New].tpt.asInstanceOf[untpd.Template]
      val member = template.body.head.asInstanceOf[untpd.ValDef]
      val swappedMember = untpd.cpy.ValDef(member)(rhs = untpd.Ident(termName(canonical.member)))
      val swappedTemplate = untpd.cpy.Template(template)(body = swappedMember :: Nil)
      val swappedRaw = untpd.cpy.DefDef(raw)(rhs = untpd.New(swappedTemplate))

      assertCode(
        ValueInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, swappedRaw),
        "EXACT_RAW_INVARIANT_FAILED"
      )
      assertCode(
        ValueInstanceFactoryPlanUntypedLowerer.validateCandidate(plan, parseOne(canonical.source)),
        "EXACT_RAW_INVARIANT_FAILED"
      )
    }
  }

  private def validPlan(names: Names): Plan =
    ValueInstanceFactoryPlan
      .create(names.factory, names.typeParameter, names.carrier, names.target, names.member)
      .fold(problem => fail(problem.message), identity)

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

  private def lower(plan: Plan)(using Context): untpd.DefDef =
    ValueInstanceFactoryPlanUntypedLowerer
      .lower(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertSourceFree(root: untpd.DefDef)(using Context): Unit =
    val trees = allTrees(root)
    assertEquals(trees.size, 17)
    trees.foreach { tree =>
      assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
      assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertCode[A](
      result: Either[ValueInstanceFactoryPlanUntypedLoweringError, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail(s"expected $expected")

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
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
      case value: untpd.New => s"New(${structure(value.tpt)})"
      case value: untpd.Template =>
        s"Template(${structure(value.constr)},${value.parentsOrDerived.map(structure)},${structure(value.self)},${value.body.map(structure)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(SourceFile.virtual("U046Expected.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
