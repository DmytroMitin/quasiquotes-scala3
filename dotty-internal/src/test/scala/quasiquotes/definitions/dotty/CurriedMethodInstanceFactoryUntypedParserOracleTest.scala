package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

/** U051 parser oracle for canonical and fully renamed curried-method factories. */
class CurriedMethodInstanceFactoryUntypedParserOracleTest extends munit.FunSuite:
  private final case class Fixture(
      source: String,
      factory: String,
      typeParameter: String,
      carrier: String,
      target: String,
      member: String,
      firstParameter: String,
      secondParameter: String
  )

  private val fixtures = Vector(
    Fixture(
      "def instance[A](combineFunction: A => A => A): Curried[A] = new Curried[A] { override def combine(a: A)(b: A): A = combineFunction(a)(b) }",
      "instance",
      "A",
      "combineFunction",
      "Curried",
      "combine",
      "a",
      "b"
    ),
    Fixture(
      "def make[Element](mergeFunction: Element => Element => Element): Aggregator[Element] = new Aggregator[Element] { override def merge(left: Element)(right: Element): Element = mergeFunction(left)(right) }",
      "make",
      "Element",
      "mergeFunction",
      "Aggregator",
      "merge",
      "left",
      "right"
    )
  )

  fixtures.foreach { fixture =>
    test(s"pins exact curried-method raw topology for ${fixture.factory}") {
      withContext {
        val root = parseOne(fixture.source)
        assertFactory(root, fixture)
        val trees = allTrees(root)
        assertEquals(trees.size, 29)
        assertEquals(categorySignature(trees), expectedCategories)
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

  private val expectedCategories = Vector(
    "DefDef", "TypeDef", "TypeBoundsTree", "ValDef", "Function", "Ident",
    "Function", "Ident", "Ident", "AppliedTypeTree", "Ident", "Ident", "New",
    "Template", "DefDef", "AppliedTypeTree", "Ident", "Ident", "DefDef", "ValDef",
    "Ident", "ValDef", "Ident", "Ident", "Apply", "Apply", "Ident", "Ident", "Ident"
  )

  private def assertFactory(root: untpd.DefDef, fixture: Fixture)(using Context): Unit =
    assertEquals(root.name.toString, fixture.factory)
    assertEquals(root.mods.flags, Flags.Method)
    root.paramss match
      case List(List(typeParameter: untpd.TypeDef), List(carrier: untpd.ValDef)) =>
        assertTypeParameter(typeParameter, fixture.typeParameter)
        assertEquals(carrier.name.toString, fixture.carrier)
        assertEquals(carrier.mods.flags, Flags.Param)
        carrier.tpt match
          case untpd.Function(
                List(untpd.Ident(firstArgument)),
                untpd.Function(List(untpd.Ident(secondArgument)), untpd.Ident(result))
              ) =>
            assertEquals(firstArgument.toString, fixture.typeParameter)
            assertEquals(secondArgument.toString, fixture.typeParameter)
            assertEquals(result.toString, fixture.typeParameter)
          case other => fail(s"expected nested unary carrier Type, found $other")
        assert(carrier.rhs.isEmpty)
      case other => fail(s"expected one Type parameter and one strict carrier, found $other")

    assertApplied(root.tpt, fixture.target, fixture.typeParameter)
    root.rhs match
      case untpd.New(template: untpd.Template) =>
        assertEquals(template.constr.name.toString, "<init>")
        assertEquals(template.constr.mods.flags, Flags.EmptyFlags)
        assertEquals(template.constr.paramss, Nil)
        assert(template.constr.tpt.isEmpty)
        assert(template.constr.rhs.isEmpty)
        template.parentsOrDerived match
          case List(parent) => assertApplied(parent, fixture.target, fixture.typeParameter)
          case other => fail(s"expected one anonymous parent, found $other")
        assertEquals(template.derived, Nil)
        assert(template.self.isEmpty)
        template.body match
          case List(member: untpd.DefDef) => assertMember(member, fixture)
          case other => fail(s"expected one override method, found $other")
      case other => fail(s"expected New(Template), found $other")

  private def assertMember(member: untpd.DefDef, fixture: Fixture)(using Context): Unit =
    assertEquals(member.name.toString, fixture.member)
    assertEquals(member.mods.flags, Flags.Method | Flags.Override)
    member.paramss match
      case List(List(first: untpd.ValDef), List(second: untpd.ValDef)) =>
        assertParameter(first, fixture.firstParameter, fixture.typeParameter)
        assertParameter(second, fixture.secondParameter, fixture.typeParameter)
      case other => fail(s"expected two ordinary unary clauses, found $other")
    assertTypeIdent(member.tpt, fixture.typeParameter)
    member.rhs match
      case untpd.Apply(
            untpd.Apply(untpd.Ident(callee), List(untpd.Ident(firstArgument))),
            List(untpd.Ident(secondArgument))
          ) =>
        assertEquals(callee.toString, fixture.carrier)
        assertEquals(firstArgument.toString, fixture.firstParameter)
        assertEquals(secondArgument.toString, fixture.secondParameter)
      case other => fail(s"expected exact nested Apply body, found $other")

  private def assertTypeParameter(parameter: untpd.TypeDef, expected: String): Unit =
    assertEquals(parameter.name.toString, expected)
    assertEquals(parameter.mods.flags, Flags.Param)
    parameter.rhs match
      case untpd.TypeBoundsTree(lo, hi, alias) =>
        assert(lo.isEmpty)
        assert(hi.isEmpty)
        assert(alias.isEmpty)
      case other => fail(s"expected unbounded TypeBoundsTree, found $other")

  private def assertParameter(parameter: untpd.ValDef, name: String, tpe: String)(using Context): Unit =
    assertEquals(parameter.name.toString, name)
    assertEquals(parameter.mods.flags, Flags.Param)
    assertTypeIdent(parameter.tpt, tpe)
    assert(parameter.rhs.isEmpty)

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

  private def categorySignature(trees: Vector[untpd.Tree]): Vector[String] =
    trees.map(_.getClass.getSimpleName.stripSuffix("$"))

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
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def parseOne(source: String)(using outerContext: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parsers.Parser(SourceFile.virtual("U051CurriedMethodInstanceFactory.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
