package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.*

class SelfAbstractTypeMemberOptionMatrixGeneratedOriginAdapterTest
    extends munit.FunSuite:
  private final case class Names(member: String, selfAlias: String, upperBase: String)

  private final case class Row(
      label: String,
      lowerPresent: Boolean,
      fBoundPresent: Boolean,
      nodeCount: Int
  )

  private val nameSets = Vector(
    Names("Self", "self", "Nat"),
    Names("Element", "owner$2", "Domain")
  )

  private val rows = Vector(
    Row("both", lowerPresent = true, fBoundPresent = true, 9),
    Row("lower", lowerPresent = true, fBoundPresent = false, 5),
    Row("f-bound", lowerPresent = false, fBoundPresent = true, 7),
    Row("neither", lowerPresent = false, fBoundPresent = false, 3)
  )

  test("renders and positions all canonical and renamed rows deterministically") {
    withContext {
      nameSets.zipWithIndex.foreach { case (names, nameIndex) =>
        rows.zipWithIndex.foreach { case (row, rowIndex) =>
          val plan = validPlan(names, row)
          val snapshot = plan.roleSnapshot
          val virtualName = s"<quasiquotes-generated:u049-$nameIndex-$rowIndex>"
          val first = lower(plan, virtualName)
          val second = lower(plan, virtualName)
          val sourceFree = SelfAbstractTypeMemberUntypedLowerer
            .lower(plan)
            .fold(problem => fail(problem.message), identity)
          val expected = render(names, row)

          assertEquals(first.generatedSource, expected)
          assertEquals(second.generatedSource, expected)
          assertEquals(first.virtualSourceName, virtualName)
          assertEquals(structure(first.tree), structure(sourceFree))
          assertEquals(structure(second.tree), structure(sourceFree))
          assert(!(first.tree eq second.tree))
          assert(!(first.sourceFile eq second.sourceFile))
          assert(
            nonEmptyTrees(first.tree).zip(nonEmptyTrees(second.tree)).forall {
              case (left, right) => !(left eq right)
            }
          )
          assertEquals(plan.roleSnapshot, snapshot)
          assertPositioned(first, expected, virtualName, row.nodeCount)
        }
      }
    }
  }

  test("preserves exact historical both-present generated sources") {
    withContext {
      val canonical = lower(validPlan(nameSets(0), rows(0)), "Canonical.scala")
      val renamed = lower(validPlan(nameSets(1), rows(0)), "Renamed.scala")
      assertEquals(
        canonical.generatedSource,
        "type Self >: self.type <: Nat { type Self = self.Self }"
      )
      assertEquals(
        renamed.generatedSource,
        "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }"
      )
      assertEquals(
        spanSnapshot(canonical.tree),
        Vector(
          ("TypeDef", 0, 5, 55),
          ("TypeBoundsTree", 13, 13, 55),
          ("SingletonTypeTree", 13, 13, 22),
          ("Ident", 13, 13, 17),
          ("RefinedTypeTree", 26, 26, 55),
          ("Ident", 26, 26, 29),
          ("TypeDef", 32, 37, 53),
          ("Select", 44, 49, 53),
          ("Ident", 44, 44, 48)
        )
      )
      assertEquals(
        spanSnapshot(renamed.tree),
        Vector(
          ("TypeDef", 0, 5, 73),
          ("TypeBoundsTree", 16, 16, 73),
          ("SingletonTypeTree", 16, 16, 28),
          ("Ident", 16, 16, 23),
          ("RefinedTypeTree", 32, 32, 73),
          ("Ident", 32, 32, 38),
          ("TypeDef", 41, 46, 71),
          ("Select", 56, 64, 71),
          ("Ident", 56, 56, 63)
        )
      )
    }
  }

  test("validates the Core plan before rendering and rejects invalid inputs") {
    withContext {
      assertCode(
        SelfAbstractTypeMemberGeneratedOriginAdapter.lower(null, "Generated.scala"),
        "INTERNAL_INVARIANT_FAILED"
      )
      val plan = validPlan(nameSets.head, rows.last)
      Vector(null, "", "bad\nname.scala", "bad\rname.scala", "bad\u0000name.scala")
        .foreach { virtualName =>
          assertCode(
            SelfAbstractTypeMemberGeneratedOriginAdapter.lower(plan, virtualName),
            "INVALID_VIRTUAL_SOURCE_NAME"
          )
        }
    }
  }

  private def render(names: Names, row: Row): String =
    val lower = Option.when(row.lowerPresent)(s" >: ${names.selfAlias}.type").getOrElse("")
    val refinement =
      Option
        .when(row.fBoundPresent)(
          s" { type ${names.member} = ${names.selfAlias}.${names.member} }"
        )
        .getOrElse("")
    s"type ${names.member}$lower <: ${names.upperBase}$refinement"

  private def validPlan(names: Names, row: Row): SelfAbstractTypeMemberPlan =
    SelfAbstractTypeMemberPlan
      .createMatrix(
        ObservedSelfAbstractTypeMemberMatrix(
          names.member,
          Option.when(row.lowerPresent)(names.selfAlias),
          names.upperBase,
          Option.when(row.fBoundPresent)(
            ObservedSelfAbstractTypeMemberRefinement(
              names.member,
              names.selfAlias,
              names.member
            )
          )
        ),
        SelfAbstractTypeMemberExpectation(
          names.member,
          names.selfAlias,
          names.upperBase
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def lower(
      plan: SelfAbstractTypeMemberPlan,
      virtualName: String
  )(using Context): GeneratedOriginDefinitionResult =
    SelfAbstractTypeMemberGeneratedOriginAdapter
      .lower(plan, virtualName)
      .fold(problem => fail(problem.message), identity)

  private def assertPositioned(
      result: GeneratedOriginDefinitionResult,
      source: String,
      virtualName: String,
      count: Int
  )(using Context): Unit =
    val trees = nonEmptyTrees(result.tree)
    assertEquals(trees.size, count)
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
      val children = directChildren(tree)
      children.foreach { child =>
        assert(child.span.start >= tree.span.start)
        assert(child.span.end <= tree.span.end)
      }
      children.zip(children.drop(1)).foreach { case (left, right) =>
        assert(left.span.end <= right.span.start)
      }
    }

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.SingletonTypeTree => s"Singleton(${structure(value.ref)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def assertCode[A](
      result: Either[SelfAbstractTypeMemberGeneratedOriginError, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail(s"expected $expected")

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    SelfAbstractTypeMemberUntypedLowerer.allTrees(tree)

  private def spanSnapshot(
      tree: untpd.Tree
  )(using Context): Vector[(String, Int, Int, Int)] =
    nonEmptyTrees(tree).map(current =>
      (
        current.getClass.getSimpleName,
        current.span.start,
        current.span.point,
        current.span.end
      )
    )

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    SelfAbstractTypeMemberUntypedLowerer.directChildren(tree)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
