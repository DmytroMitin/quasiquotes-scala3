package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** U047 parser oracle for canonical and fully renamed Type-member factories. */
class TypeMemberInstanceFactoryUntypedParserOracleTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      factory: String,
      firstTypeParameter: String,
      secondTypeParameter: String,
      target: String,
      member: String
  )

  private val fixtures = Vector(
    Fixture(
      "def instance[A, Out0]: HasOut[A] { type Out = Out0 } = new HasOut[A] { type Out = Out0 }",
      "instance",
      "A",
      "Out0",
      "HasOut",
      "Out"
    ),
    Fixture(
      "def make[Element, Result0]: Container[Element] { type Result = Result0 } = new Container[Element] { type Result = Result0 }",
      "make",
      "Element",
      "Result0",
      "Container",
      "Result"
    )
  )

  fixtures.foreach { fixture =>
    test(s"pins exact Type-member factory raw topology for ${fixture.factory}") {
      withContext {
        val root = parseOne(fixture.source)
        assertFactory(root, fixture)
        val trees = allTrees(root)
        assertEquals(trees.size, 19)
        assert(trees.forall(!_.source.exists))
        assert(trees.forall(_.span.exists))
        assert(trees.forall(_.symbol == NoSymbol))
        assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
        trees.foreach { tree =>
          assert(tree.span.start >= root.span.start)
          assert(tree.span.end <= root.span.end)
          assert(tree.span.start <= tree.span.point)
          assert(tree.span.point <= tree.span.end)
        }
      }
    }
  }

  private def assertFactory(root: untpd.DefDef, fixture: Fixture)(using Context): Unit =
    assertEquals(root.name.toString, fixture.factory)
    assertEquals(root.mods.flags, Flags.Method)
    root.paramss match
      case List(List(first: untpd.TypeDef, second: untpd.TypeDef)) =>
        assertTypeParameter(first, fixture.firstTypeParameter)
        assertTypeParameter(second, fixture.secondTypeParameter)
      case other => fail(s"expected one two-Type-parameter clause and no value clauses, found $other")

    root.tpt match
      case untpd.RefinedTypeTree(base, List(alias: untpd.TypeDef)) =>
        assertApplied(base, fixture.target, fixture.firstTypeParameter)
        assertAlias(alias, fixture.member, fixture.secondTypeParameter)
      case other => fail(s"expected one concrete result refinement alias, found $other")

    root.rhs match
      case untpd.New(template: untpd.Template) =>
        assertEquals(template.constr.name.toString, "<init>")
        assertEquals(template.constr.mods.flags, Flags.EmptyFlags)
        assertEquals(template.constr.paramss, Nil)
        assert(template.constr.tpt.isEmpty)
        assert(template.constr.rhs.isEmpty)
        template.parentsOrDerived match
          case List(parent) =>
            assertApplied(parent, fixture.target, fixture.firstTypeParameter)
          case other => fail(s"expected one anonymous parent, found $other")
        assertEquals(template.derived, Nil)
        assert(template.self.isEmpty)
        template.body match
          case List(alias: untpd.TypeDef) =>
            assertAlias(alias, fixture.member, fixture.secondTypeParameter)
          case other => fail(s"expected one anonymous concrete Type alias, found $other")
      case other => fail(s"expected New(Template), found $other")

  private def assertTypeParameter(parameter: untpd.TypeDef, expected: String): Unit =
    assertEquals(parameter.name.toString, expected)
    assertEquals(parameter.mods.flags, Flags.Param)
    parameter.rhs match
      case untpd.TypeBoundsTree(lo, hi, alias) =>
        assert(lo.isEmpty)
        assert(hi.isEmpty)
        assert(alias.isEmpty)
      case other => fail(s"expected unbounded TypeBoundsTree for $expected, found $other")

  private def assertAlias(alias: untpd.TypeDef, member: String, rhs: String): Unit =
    assertEquals(alias.name.toString, member)
    assertEquals(alias.mods.flags, Flags.EmptyFlags)
    alias.rhs match
      case untpd.Ident(name) => assertEquals(name.toString, rhs)
      case other => fail(s"expected direct Type Ident($rhs), found $other")

  private def assertApplied(tree: untpd.Tree, constructor: String, argument: String): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(observedConstructor),
            List(untpd.Ident(observedArgument))
          ) =>
        assertEquals(observedConstructor.toString, constructor)
        assertEquals(observedArgument.toString, argument)
      case other => fail(s"expected $constructor[$argument], found $other")

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual("U047TypeMemberInstanceFactory.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
