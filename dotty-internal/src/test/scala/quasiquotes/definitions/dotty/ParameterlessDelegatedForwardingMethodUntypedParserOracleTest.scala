package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** U048 parser oracle for canonical and fully renamed parameterless forwarders. */
class ParameterlessDelegatedForwardingMethodUntypedParserOracleTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      method: String,
      typeParameter: String,
      contextualParameter: String,
      evidenceConstructor: String
  )

  private val fixtures = Vector(
    Fixture(
      "def empty[A](using inst: Empty[A]): A = inst.empty",
      "empty",
      "A",
      "inst",
      "Empty"
    ),
    Fixture(
      "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain",
      "obtain",
      "Element",
      "evidence",
      "Provider"
    )
  )

  fixtures.foreach { fixture =>
    test(s"pins exact parameterless-forwarder raw topology for ${fixture.method}") {
      withContext {
        val root = parseOne(fixture.source)
        assertForwarder(root, fixture)
        val trees = allTrees(root)
        assertEquals(trees.size, 10)
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

  private def assertForwarder(root: untpd.DefDef, fixture: Fixture)(using Context): Unit =
    assertEquals(root.name.toString, fixture.method)
    assertEquals(root.mods.flags, Flags.Method)
    root.paramss match
      case List(List(typeParameter: untpd.TypeDef), List(contextual: untpd.ValDef)) =>
        assertEquals(typeParameter.name.toString, fixture.typeParameter)
        assertEquals(typeParameter.mods.flags, Flags.Param)
        typeParameter.rhs match
          case untpd.TypeBoundsTree(lo, hi, alias) =>
            assert(lo.isEmpty)
            assert(hi.isEmpty)
            assert(alias.isEmpty)
          case other => fail(s"expected unbounded TypeBoundsTree, found $other")

        assertEquals(contextual.name.toString, fixture.contextualParameter)
        assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
        assert(contextual.rhs.isEmpty)
        contextual.tpt match
          case untpd.AppliedTypeTree(
                untpd.Ident(constructor),
                List(untpd.Ident(argument))
              ) =>
            assertEquals(constructor.toString, fixture.evidenceConstructor)
            assertEquals(argument.toString, fixture.typeParameter)
          case other => fail(s"expected unary contextual AppliedTypeTree, found $other")
      case other =>
        fail(s"expected one Type clause and one contextual clause only, found $other")

    root.tpt match
      case untpd.Ident(name) => assertEquals(name.toString, fixture.typeParameter)
      case other => fail(s"expected direct result Ident(${fixture.typeParameter}), found $other")

    root.rhs match
      case untpd.Select(untpd.Ident(receiver), selected) =>
        assertEquals(receiver.toString, fixture.contextualParameter)
        assertEquals(selected.toString, fixture.method)
      case _: untpd.Apply => fail("parameterless body must not contain Apply")
      case other => fail(s"expected direct stable Select body, found $other")

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
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parsers.Parser(
      SourceFile.virtual("U048ParameterlessDelegatedForwarding.scala", source)
    ).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
