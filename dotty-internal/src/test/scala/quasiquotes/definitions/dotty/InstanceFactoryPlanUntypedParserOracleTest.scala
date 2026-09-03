package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** Retained U017 parser oracle for canonical and fully renamed factories. */
class InstanceFactoryPlanUntypedParserOracleTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      factory: String,
      typeParameter: String,
      emptyCarrier: String,
      functionCarrier: String,
      target: String,
      emptyMember: String,
      combineMember: String,
      firstNested: String,
      secondNested: String
  )

  private val fixtures = Vector(
    Fixture(
      "def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] { override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1) }",
      "instance",
      "A",
      "emptyValue",
      "combineFunction",
      "Monoid",
      "empty",
      "combine",
      "a",
      "a1"
    ),
    Fixture(
      "def make[Element](fallbackValue: => Element, selection: (Element, Element) => Element): Choice[Element] = new Choice[Element] { override def fallback: Element = fallbackValue; override def select(left: Element, right: Element): Element = selection(left, right) }",
      "make",
      "Element",
      "fallbackValue",
      "selection",
      "Choice",
      "fallback",
      "select",
      "left",
      "right"
    )
  )

  fixtures.foreach { fixture =>
    test(s"pins exact raw factory topology for ${fixture.factory}") {
      withContext {
        val root = parseOne(fixture.source)
        assertFactory(root, fixture)
        val trees = allTrees(root)
        assertEquals(trees.size, 33)
        assert(trees.forall(_.span.exists))
        assertEquals(trees.map(_.source.exists).distinct.size, 1)
        assert(trees.forall(_.symbol == NoSymbol))
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
    assertEquals(root.leadingTypeParams.size, 1)
    val typeParameter = root.leadingTypeParams.head
    assertEquals(typeParameter.name.toString, fixture.typeParameter)
    assertEquals(typeParameter.mods.flags, Flags.Param)
    typeParameter.rhs match
      case untpd.TypeBoundsTree(lo, hi, alias) =>
        assert(lo.isEmpty)
        assert(hi.isEmpty)
        assert(alias.isEmpty)
      case other => fail(s"expected unbounded TypeBoundsTree, found $other")

    val outerParameters = root.trailingParamss match
      case List(List(first: untpd.ValDef, second: untpd.ValDef)) => first -> second
      case other => fail(s"expected one two-value outer clause, found $other")
    val (emptyCarrier, functionCarrier) = outerParameters
    assertEquals(emptyCarrier.name.toString, fixture.emptyCarrier)
    assertEquals(functionCarrier.name.toString, fixture.functionCarrier)
    assertEquals(emptyCarrier.mods.flags, Flags.Param)
    assertEquals(functionCarrier.mods.flags, Flags.Param)
    emptyCarrier.tpt match
      case untpd.ByNameTypeTree(untpd.Ident(name)) =>
        assertEquals(name.toString, fixture.typeParameter)
      case other => fail(s"expected exact ByNameTypeTree, found $other")
    functionCarrier.tpt match
      case untpd.Function(
            List(untpd.Ident(first), untpd.Ident(second)),
            untpd.Ident(result)
          ) =>
        assertEquals(
          Vector(first, second, result).map(_.toString),
          Vector.fill(3)(fixture.typeParameter)
        )
      case other => fail(s"expected exact binary Function Type tree, found $other")
    assertApplied(root.tpt, fixture.target, fixture.typeParameter)

    val template = root.rhs match
      case untpd.New(value: untpd.Template) => value
      case other => fail(s"expected New(Template), found $other")
    assertEquals(template.constr.name.toString, "<init>")
    assertEquals(template.constr.paramss, Nil)
    assert(template.constr.tpt.isEmpty)
    assert(template.constr.rhs.isEmpty)
    template.parentsOrDerived match
      case List(parent) => assertApplied(parent, fixture.target, fixture.typeParameter)
      case other => fail(s"expected one anonymous parent, found $other")
    assertEquals(template.derived, Nil)
    assertEquals(template.self.name.toString, "_")
    assert(template.self.tpt.isEmpty)
    assert(template.self.rhs.isEmpty)

    template.body match
      case List(empty: untpd.DefDef, combine: untpd.DefDef) =>
        assertEquals(empty.name.toString, fixture.emptyMember)
        assertEquals(empty.mods.flags, Flags.Method | Flags.Override)
        assertEquals(empty.paramss, Nil)
        assertTypeIdent(empty.tpt, fixture.typeParameter)
        assertTermIdent(empty.rhs, fixture.emptyCarrier)

        assertEquals(combine.name.toString, fixture.combineMember)
        assertEquals(combine.mods.flags, Flags.Method | Flags.Override)
        combine.paramss match
          case List(List(first: untpd.ValDef, second: untpd.ValDef)) =>
            assertEquals(first.name.toString, fixture.firstNested)
            assertEquals(second.name.toString, fixture.secondNested)
            assertEquals(first.mods.flags, Flags.Param)
            assertEquals(second.mods.flags, Flags.Param)
            assertTypeIdent(first.tpt, fixture.typeParameter)
            assertTypeIdent(second.tpt, fixture.typeParameter)
          case other => fail(s"expected two nested parameters, found $other")
        assertTypeIdent(combine.tpt, fixture.typeParameter)
        combine.rhs match
          case untpd.Apply(
                untpd.Ident(callee),
                List(untpd.Ident(first), untpd.Ident(second))
              ) =>
            assertEquals(callee.toString, fixture.functionCarrier)
            assertEquals(first.toString, fixture.firstNested)
            assertEquals(second.toString, fixture.secondNested)
          case other => fail(s"expected direct combine Apply, found $other")
      case other => fail(s"expected two ordered overrides, found $other")

  private def assertApplied(tree: untpd.Tree, constructor: String, argument: String): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(observedConstructor),
            List(untpd.Ident(observedArgument))
          ) =>
        assertEquals(observedConstructor.toString, constructor)
        assertEquals(observedArgument.toString, argument)
      case other => fail(s"expected $constructor[$argument], found $other")

  private def assertTypeIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Type Ident($expected), found $other")

  private def assertTermIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Term Ident($expected), found $other")

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
      case value: untpd.ByNameTypeTree => Vector(value.result)
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual("U017InstanceFactory.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
