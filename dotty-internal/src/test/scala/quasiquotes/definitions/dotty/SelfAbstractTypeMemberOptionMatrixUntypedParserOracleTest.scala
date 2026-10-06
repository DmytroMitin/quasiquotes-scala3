package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class SelfAbstractTypeMemberOptionMatrixUntypedParserOracleTest extends munit.FunSuite:
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
    Row("both-present", lowerPresent = true, fBoundPresent = true, 9),
    Row("lower-only", lowerPresent = true, fBoundPresent = false, 5),
    Row("f-bound-only", lowerPresent = false, fBoundPresent = true, 7),
    Row("neither", lowerPresent = false, fBoundPresent = false, 3)
  )

  nameSets.foreach { names =>
    rows.foreach { row =>
      test(s"pins ${row.label} raw topology for ${names.member}") {
        withContext {
          val source = render(names, row)
          val parsed = parseOne(source)

          assertRow(parsed, names, row)
          val trees = nonEmptyTrees(parsed)
          assertEquals(trees.size, row.nodeCount)
          assert(trees.forall(!_.source.exists))
          assert(trees.forall(_.span.exists))
          assert(trees.forall(_.symbol == NoSymbol))
          assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
          trees.foreach { tree =>
            assert(tree.span.start >= parsed.span.start)
            assert(tree.span.end <= parsed.span.end)
            assert(tree.span.start <= tree.span.point)
            assert(tree.span.point <= tree.span.end)
          }
        }
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

  private def assertRow(
      definition: untpd.TypeDef,
      names: Names,
      row: Row
  ): Unit =
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
              assert(!member.mods.hasFlags)
              member.rhs match
                case untpd.Select(untpd.Ident(prefix), selected) =>
                  assertEquals(prefix.toString, names.selfAlias)
                  assertEquals(selected.toString, names.member)
                case other => fail(s"expected selected refinement RHS, found $other")
            case other => fail(s"expected one-alias refinement, found $other")
        else
          bounds.hi match
            case untpd.Ident(base) => assertEquals(base.toString, names.upperBase)
            case other => fail(s"expected direct upper-base Ident, found $other")
      case other => fail(s"expected TypeBoundsTree, found $other")

  private def parseOne(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("U049SelfOptionMatrixOracle.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.TypeDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

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
