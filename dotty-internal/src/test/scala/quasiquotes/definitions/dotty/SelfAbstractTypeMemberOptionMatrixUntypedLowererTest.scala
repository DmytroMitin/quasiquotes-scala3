package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.*

class SelfAbstractTypeMemberOptionMatrixUntypedLowererTest extends munit.FunSuite:
  private final case class Names(member: String, selfAlias: String, upperBase: String)

  private final case class Row(
      lowerPresent: Boolean,
      fBoundPresent: Boolean,
      nodeCount: Int
  )

  private val nameSets = Vector(
    Names("Self", "self", "Nat"),
    Names("Element", "owner$2", "Domain")
  )

  private val rows = Vector(
    Row(lowerPresent = true, fBoundPresent = true, 9),
    Row(lowerPresent = true, fBoundPresent = false, 5),
    Row(lowerPresent = false, fBoundPresent = true, 7),
    Row(lowerPresent = false, fBoundPresent = false, 3)
  )

  test("lowers all canonical and renamed option rows to fresh exact source-free trees") {
    withContext {
      nameSets.foreach { names =>
        rows.foreach { row =>
          val plan = validPlan(names, row)
          val snapshot = plan.roleSnapshot
          val first = lower(plan)
          val second = lower(plan)

          assertRow(first, names, row)
          assertEquals(structure(first), expectedStructure(names, row))
          assertEquals(structure(second), structure(first))
          assert(!(first eq second))
          assert(
            nonEmptyTrees(first).zip(nonEmptyTrees(second)).forall {
              case (left, right) => !(left eq right)
            }
          )
          assertEquals(plan.roleSnapshot, snapshot)
          assertSourceFree(first, row.nodeCount)
          assertEquals(
            SelfAbstractTypeMemberUntypedLowerer.validateCandidate(plan, first),
            Right(())
          )
        }
      }
    }
  }

  test("rejects row substitutions and synthetic absent-edge representations") {
    withContext {
      val names = nameSets.head
      val both = rows(0)
      val lowerOnly = rows(1)
      val fOnly = rows(2)
      val neither = rows(3)
      val bothRaw = lower(validPlan(names, both))
      val lowerRaw = lower(validPlan(names, lowerOnly))
      val fRaw = lower(validPlan(names, fOnly))
      val neitherRaw = lower(validPlan(names, neither))

      assertRejected(validPlan(names, lowerOnly), bothRaw)
      assertRejected(validPlan(names, fOnly), bothRaw)
      assertRejected(validPlan(names, neither), bothRaw)
      assertRejected(validPlan(names, both), lowerRaw)
      assertRejected(validPlan(names, both), fRaw)
      assertRejected(validPlan(names, both), neitherRaw)

      val explicitNothing = untpd.cpy.TypeDef(neitherRaw)(
        rhs = untpd.TypeBoundsTree(
          untpd.Ident(typeName("Nothing")),
          untpd.Ident(typeName(names.upperBase))
        )
      )
      val emptyRefinement = untpd.cpy.TypeDef(neitherRaw)(
        rhs = untpd.TypeBoundsTree(
          untpd.EmptyTree,
          untpd.RefinedTypeTree(untpd.Ident(typeName(names.upperBase)), Nil)
        )
      )
      assertRejected(validPlan(names, neither), explicitNothing)
      assertRejected(validPlan(names, neither), emptyRefinement)
    }
  }

  test("rejects null plans and raw contamination after direct Core-plan validation") {
    withContext {
      assertCode(SelfAbstractTypeMemberUntypedLowerer.lower(null), "RAW_LOWERING_FAILED")

      val names = nameSets(1)
      val plan = validPlan(names, rows.head)
      val raw = lower(plan)
      val wrongSelected = raw.rhs match
        case bounds: untpd.TypeBoundsTree =>
          val wrongUpper = bounds.hi match
            case refinement: untpd.RefinedTypeTree =>
              val member = refinement.refinements.head.asInstanceOf[untpd.TypeDef]
              val wrongMember = untpd.cpy.TypeDef(member)(
                rhs = untpd.Select(
                  untpd.Ident(termName("other")),
                  typeName(names.member)
                )
              )
              untpd.cpy.RefinedTypeTree(refinement)(
                tpt = refinement.tpt,
                refinements = wrongMember :: Nil
              )
            case other => fail(s"expected refinement, found $other")
          untpd.cpy.TypeDef(raw)(
            rhs = untpd.TypeBoundsTree(bounds.lo, wrongUpper)
          )
        case other => fail(s"expected bounds, found $other")

      assertRejected(plan, wrongSelected)
    }
  }

  private def validPlan(names: Names, row: Row): SelfAbstractTypeMemberPlan =
    val refinement = Option.when(row.fBoundPresent)(
      ObservedSelfAbstractTypeMemberRefinement(
        names.member,
        names.selfAlias,
        names.member
      )
    )
    SelfAbstractTypeMemberPlan
      .createMatrix(
        ObservedSelfAbstractTypeMemberMatrix(
          names.member,
          Option.when(row.lowerPresent)(names.selfAlias),
          names.upperBase,
          refinement
        ),
        SelfAbstractTypeMemberExpectation(
          names.member,
          names.selfAlias,
          names.upperBase
        )
      )
      .fold(problem => fail(problem.message), identity)

  private def lower(plan: SelfAbstractTypeMemberPlan)(using Context): untpd.TypeDef =
    SelfAbstractTypeMemberUntypedLowerer
      .lower(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertRow(definition: untpd.TypeDef, names: Names, row: Row): Unit =
    assertEquals(definition.name.toString, names.member)
    assert(!definition.mods.hasFlags)
    definition.rhs match
      case bounds: untpd.TypeBoundsTree =>
        assert(bounds.alias.isEmpty)
        if row.lowerPresent then
          bounds.lo match
            case untpd.SingletonTypeTree(untpd.Ident(alias)) =>
              assertEquals(alias.toString, names.selfAlias)
            case other => fail(s"expected singleton lower, found $other")
        else assert(bounds.lo.isEmpty, clues(bounds.lo))

        if row.fBoundPresent then
          bounds.hi match
            case untpd.RefinedTypeTree(
                  untpd.Ident(base),
                  List(member: untpd.TypeDef)
                ) =>
              assertEquals(base.toString, names.upperBase)
              assertEquals(member.name.toString, names.member)
              member.rhs match
                case untpd.Select(untpd.Ident(prefix), selected) =>
                  assertEquals(prefix.toString, names.selfAlias)
                  assertEquals(selected.toString, names.member)
                case other => fail(s"expected selected RHS, found $other")
            case other => fail(s"expected refinement, found $other")
        else
          bounds.hi match
            case untpd.Ident(base) => assertEquals(base.toString, names.upperBase)
            case other => fail(s"expected direct upper Ident, found $other")
      case other => fail(s"expected TypeBoundsTree, found $other")

  private def expectedStructure(names: Names, row: Row): String =
    val lower =
      if row.lowerPresent then s"Singleton(Ident(${names.selfAlias}))"
      else "Empty"
    val upper =
      if row.fBoundPresent then
        s"Refined(Ident(${names.upperBase}),List(TypeDef(${names.member},0,Select(Ident(${names.selfAlias}),${names.member}))))"
      else s"Ident(${names.upperBase})"
    s"TypeDef(${names.member},0,TypeBounds($lower,$upper,Empty))"

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

  private def assertSourceFree(root: untpd.TypeDef, count: Int)(using Context): Unit =
    val trees = nonEmptyTrees(root)
    assertEquals(trees.size, count)
    trees.foreach { tree =>
      assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
      assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertRejected(
      plan: SelfAbstractTypeMemberPlan,
      raw: untpd.TypeDef
  )(using Context): Unit =
    assertCode(
      SelfAbstractTypeMemberUntypedLowerer.validateCandidate(plan, raw),
      "RAW_LOWERING_FAILED"
    )

  private def assertCode[A](
      result: Either[SelfAbstractTypeMemberRawError, A],
      expected: String
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expected, clues(problem.detail))
      case Right(_) => fail(s"expected $expected")

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.SingletonTypeTree => Vector(value.ref)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
