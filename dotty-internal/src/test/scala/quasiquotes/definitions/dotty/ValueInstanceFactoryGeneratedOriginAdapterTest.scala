package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

class ValueInstanceFactoryGeneratedOriginAdapterTest extends munit.FunSuite:
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

  test("renders deterministic canonical and renamed complete generated origins") {
    withContext {
      Vector(canonical, renamed).zipWithIndex.foreach { case (names, index) =>
        val plan = validPlan(names)
        val snapshot = plan.roleSnapshot
        val virtualName = s"<quasiquotes-generated:u046-value-instance-factory-$index>"
        val first = lower(plan, virtualName)
        val second = lower(plan, virtualName)
        val sourceFree = ValueInstanceFactoryPlanUntypedLowerer
          .lower(plan)
          .fold(problem => fail(problem.message), identity)

        assertEquals(first.generatedSource, names.source)
        assertEquals(second.generatedSource, names.source)
        assertEquals(first.virtualSourceName, virtualName)
        assertEquals(structure(first.tree), structure(sourceFree))
        assertEquals(structure(second.tree), structure(sourceFree))
        assert(!(first.tree eq second.tree))
        assert(!(first.sourceFile eq second.sourceFile))
        assert(
          allTrees(first.tree).zip(allTrees(second.tree)).forall {
            case (left, right) => !(left eq right)
          }
        )
        assertEquals(plan.roleSnapshot, snapshot)
        assertPositioned(first, names.source, virtualName)
      }
    }
  }

  test("renders Core-valid backticked roles exactly") {
    withContext {
      val plan = ValueInstanceFactoryPlan
        .create("`def`", "`type`", "`val`", "`class`", "`object`")
        .fold(problem => fail(problem.message), identity)
      val result = lower(plan, "<quasiquotes-generated:u046-backticked>")
      assertEquals(
        result.generatedSource,
        "def `def`[`type`](`val`: `type`): `class`[`type`] = new `class`[`type`] { override val `object`: `type` = `val` }"
      )
      assertPositioned(result, result.generatedSource, result.virtualSourceName)
    }
  }

  test("rejects missing inputs and invalid virtual source names") {
    withContext {
      assertCode(
        ValueInstanceFactoryGeneratedOriginAdapter
          .lower(null, "<quasiquotes-generated:u046-null>"),
        "PLAN_REQUIRED"
      )
      val plan = validPlan(canonical)
      Vector(null, "", "bad\nname.scala", "bad\rname.scala", "bad\u0000name.scala")
        .foreach { virtualName =>
          assertCode(
            ValueInstanceFactoryGeneratedOriginAdapter.lower(plan, virtualName),
            "GENERATED_ORIGIN_INVALID"
          )
        }
    }
  }

  private def validPlan(names: Names): Plan =
    ValueInstanceFactoryPlan
      .create(names.factory, names.typeParameter, names.carrier, names.target, names.member)
      .fold(problem => fail(problem.message), identity)

  private def lower(plan: Plan, virtualName: String)(using Context): GeneratedOriginDefinitionResult =
    ValueInstanceFactoryGeneratedOriginAdapter
      .lower(plan, virtualName)
      .fold(problem => fail(problem.message), identity)

  private def assertPositioned(
      result: GeneratedOriginDefinitionResult,
      source: String,
      virtualName: String
  )(using Context): Unit =
    val trees = allTrees(result.tree)
    assertEquals(trees.size, 17)
    assertEquals(result.tree.span.start, 0)
    assertEquals(result.tree.span.end, source.length)
    trees.foreach { tree =>
      assert(tree.source.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.source.path, virtualName)
      assertEquals(tree.source.content.mkString, source)
      assert(tree.span.exists, clues(tree.getClass.getSimpleName))
      assert(tree.span.start >= 0)
      assert(tree.span.start <= tree.span.point)
      assert(tree.span.point <= tree.span.end)
      assert(tree.span.end <= source.length)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
      directChildren(tree).foreach { child =>
        assert(child.span.start >= tree.span.start)
        assert(child.span.end <= tree.span.end)
      }
    }

  private def assertCode[A](
      result: Either[ValueInstanceFactoryGeneratedOriginError, A],
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
    ValueInstanceFactoryPlanUntypedLowerer.directChildren(tree).filterNot(_.isEmpty)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
