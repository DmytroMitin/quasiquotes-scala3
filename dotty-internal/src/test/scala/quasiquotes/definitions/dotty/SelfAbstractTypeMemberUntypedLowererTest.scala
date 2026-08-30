package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.*

class SelfAbstractTypeMemberUntypedLowererTest extends munit.FunSuite:
  private val Canonical =
    "type Self >: self.type <: Nat { type Self = self.Self }"
  private val Renamed =
    "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }"

  test("the parser exposes the exact canonical nine-node TypeDef tree and spans") {
    withContext {
      parseOne(Canonical) { parsed =>
        assertExactShape(parsed, "Self", "self", "Nat")
        assertEquals(structure(parsed), expectedStructure("Self", "self", "Nat"))
        assertEquals(
          nonEmptyTrees(parsed).map(tree =>
            (tree.getClass.getSimpleName, tree.span.start, tree.span.point, tree.span.end)
          ),
          Vector(
            ("TypeDef", 0, 5, 55),
            ("TypeBoundsTree", 10, 10, 55),
            ("SingletonTypeTree", 13, 13, 22),
            ("Ident", 13, 13, 17),
            ("RefinedTypeTree", 26, 26, 55),
            ("Ident", 26, 26, 29),
            ("TypeDef", 32, 37, 53),
            ("Select", 44, 49, 53),
            ("Ident", 44, 44, 48)
          )
        )
        assert(!parsed.source.exists)
      }
    }
  }

  test("a source-free constructor reproduces canonical and renamed parser structures") {
    withContext {
      List(
        (Canonical, "Self", "self", "Nat"),
        (Renamed, "Element", "owner$2", "Domain")
      ).foreach { case (source, member, selfAlias, upperBase) =>
        val parsedStructure = parseOne(source)(structure)
        val constructed = lowerRaw(member, selfAlias, upperBase)

        assertExactShape(constructed, member, selfAlias, upperBase)
        assertEquals(structure(constructed), parsedStructure)
        val nodes = nonEmptyTrees(constructed)
        assertEquals(nodes.size, 9)
        nodes.foreach { tree =>
          assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
          assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree.getClass.getSimpleName))
        }
      }
    }
  }

  test("deterministic generated source can recursively position all nine nodes") {
    withContext {
      List(("Self", "self", "Nat"), ("Element", "owner$2", "Domain")).foreach {
        case (member, selfAlias, upperBase) =>
          val plan = validPlan(member, selfAlias, upperBase)
          val raw = lowerRaw(member, selfAlias, upperBase)
          val lowered = SelfAbstractTypeMemberGeneratedOriginAdapter
            .lower(plan, "Phase137Auxify046Generated.scala")
            .fold(error => fail(error.message), identity)
          val generated = lowered.generatedSource
          val source = lowered.sourceFile
          val positioned = lowered.tree match
            case value: untpd.TypeDef => value
            case other => fail(s"expected TypeDef, found ${other.getClass.getSimpleName}")

          assertExactShape(positioned, member, selfAlias, upperBase)
          assertEquals(structure(positioned), structure(raw))
          val nodes = nonEmptyTrees(positioned)
          assertEquals(nodes.size, 9)
          nodes.foreach { tree =>
            assert(tree.source.exists, clues(tree.getClass.getSimpleName))
            assertEquals(tree.source.path, source.path)
            assertEquals(tree.source.content.mkString, generated)
            assert(tree.span.exists, clues(tree.getClass.getSimpleName))
            assert(tree.span.start >= 0)
            assert(tree.span.start <= tree.span.point)
            assert(tree.span.point <= tree.span.end)
            assert(tree.span.end <= generated.length)
            assertEquals(tree.symbol, NoSymbol)
            assert(!tree.isInstanceOf[untpd.TypedSplice])
            directChildren(tree).foreach { child =>
              assert(child.span.start >= tree.span.start)
              assert(child.span.end <= tree.span.end)
            }
          }
          nodes.foreach { tree =>
            directChildren(tree).zip(directChildren(tree).drop(1)).foreach {
              case (left, right) => assert(left.span.end <= right.span.start)
            }
          }
      }
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne[A](source: String)(
      run: untpd.TypeDef => A
  )(using outerContext: Context): A =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("SelfAbstractTypeMemberOracle.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    val definition = parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head match
          case value: untpd.TypeDef => value
          case other => fail(s"expected TypeDef, found ${other.getClass.getSimpleName}")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")
    run(definition)

  private def lowerRaw(
      member: String,
      selfAlias: String,
      upperBase: String
  )(using Context): untpd.TypeDef =
    SelfAbstractTypeMemberUntypedLowerer
      .lower(validPlan(member, selfAlias, upperBase))
      .fold(error => fail(error.message), identity)

  private def validPlan(
      member: String,
      selfAlias: String,
      upperBase: String
  ): SelfAbstractTypeMemberPlan =
    val observed = ObservedSelfAbstractTypeMember(
      member,
      selfAlias,
      upperBase,
      member,
      selfAlias,
      member
    )
    val expected = SelfAbstractTypeMemberExpectation(member, selfAlias, upperBase)
    SelfAbstractTypeMemberPlan
      .create(observed, expected)
      .fold(error => fail(error.message), identity)

  private def assertExactShape(
      definition: untpd.TypeDef,
      member: String,
      selfAlias: String,
      upperBase: String
  ): Unit =
    assertEquals(definition.name.toString, member)
    assert(!definition.mods.hasFlags)
    definition.rhs match
      case untpd.TypeBoundsTree(
            untpd.SingletonTypeTree(untpd.Ident(lowerAlias)),
            untpd.RefinedTypeTree(
              untpd.Ident(base),
              List(refinementMember: untpd.TypeDef)
            ),
            alias
          ) =>
        assert(alias.isEmpty)
        assertEquals(lowerAlias.toString, selfAlias)
        assertEquals(base.toString, upperBase)
        assertEquals(refinementMember.name.toString, member)
        assert(!refinementMember.mods.hasFlags)
        refinementMember.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, selfAlias)
            assertEquals(selected.toString, member)
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected exact two-bound refined TypeDef, found $other")

  private def expectedStructure(member: String, selfAlias: String, base: String): String =
    s"TypeDef($member,0,TypeBounds(Singleton(Ident($selfAlias)),Refined(Ident($base),List(TypeDef($member,0,Select(Ident($selfAlias),$member)))),Empty))"

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.TypeBoundsTree =>
        s"TypeBounds(${structure(value.lo)},${structure(value.hi)},${structure(value.alias)})"
      case value: untpd.SingletonTypeTree =>
        s"Singleton(${structure(value.ref)})"
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.Select =>
        s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

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
