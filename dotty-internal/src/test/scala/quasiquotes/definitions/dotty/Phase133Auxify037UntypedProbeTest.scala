package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class Phase133Auxify037UntypedProbeTest extends munit.FunSuite:
  private val Source =
    """def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] {
      |  type Out = inst.Out
      |} = inst""".stripMargin

  test("the parser exposes the exact two-bound applied and one-refinement raw tree") {
    withContext {
      parseOne { parsed =>
        assertExactShape(parsed)
        assertEquals(parsed.span.start, 0)
        assertEquals(parsed.span.end, Source.length)
        assert(!parsed.source.exists)
        nonEmptyTrees(parsed).foreach { tree =>
          assert(tree.span.exists, clues(tree.getClass.getSimpleName))
          assert(tree.span.start >= 0, clues(tree.getClass.getSimpleName))
          assert(tree.span.end <= Source.length, clues(tree.getClass.getSimpleName))
          assert(tree.span.start <= tree.span.point, clues(tree.getClass.getSimpleName))
          assert(tree.span.point <= tree.span.end, clues(tree.getClass.getSimpleName))
        }
      }
    }
  }

  test("a parser-free constructor can reproduce the same exact raw structure") {
    withContext {
      val parsedStructure = parseOne { parsed =>
        assertExactShape(parsed)
        structure(parsed)
      }
      val constructed = constructEquivalent()

      assertExactShape(constructed)
      assertEquals(structure(constructed), parsedStructure)
      nonEmptyTrees(constructed).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
      }
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne[A](run: untpd.DefDef => A)(using outerContext: Context): A =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("Phase133Auxify037Probe.scala", Source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    val definition = parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")
    run(definition)

  private def constructEquivalent()(using Context): untpd.DefDef =
    given SourceFile = NoSource
    val first =
      untpd
        .TypeDef(
          typeName("N"),
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.Ident(typeName("Nat")))
        )
        .withMods(untpd.Modifiers(Flags.Param))
    val second =
      untpd
        .TypeDef(
          typeName("M"),
          untpd.TypeBoundsTree(untpd.EmptyTree, untpd.Ident(typeName("Nat")))
        )
        .withMods(untpd.Modifiers(Flags.Param))
    val applied = addOfNAndM()
    val contextual =
      untpd
        .ValDef(termName("inst"), applied, untpd.EmptyTree)
        .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
    val selected =
      untpd.Select(untpd.Ident(termName("inst")), typeName("Out"))
    val refinementMember = untpd.TypeDef(typeName("Out"), selected)
    val result = untpd.RefinedTypeTree(addOfNAndM(), refinementMember :: Nil)
    untpd
      .DefDef(
        termName("apply"),
        List(List(first, second), List(contextual)),
        result,
        untpd.Ident(termName("inst"))
      )
      .withMods(untpd.Modifiers(Flags.Method))

  private def addOfNAndM()(using SourceFile): untpd.Tree =
    untpd.AppliedTypeTree(
      untpd.Ident(typeName("Add")),
      List(untpd.Ident(typeName("N")), untpd.Ident(typeName("M")))
    )

  private def assertExactShape(definition: untpd.DefDef)(using Context): Unit =
    assertEquals(definition.name.toString, "apply")
    assertEquals(definition.mods.flags, Flags.Method)
    val typeParameters = definition.leadingTypeParams
    assertEquals(typeParameters.map(_.name.toString), List("N", "M"))
    typeParameters.foreach { parameter =>
      assertEquals(parameter.mods.flags, Flags.Param)
      parameter.rhs match
        case untpd.TypeBoundsTree(lo, untpd.Ident(hi), alias) =>
          assert(lo.isEmpty)
          assertEquals(hi.toString, "Nat")
          assert(alias.isEmpty)
        case other => fail(s"expected upper-only TypeBoundsTree, found $other")
    }
    val contextual = definition.trailingParamss match
      case List(List(value: untpd.ValDef)) => value
      case other => fail(s"expected one contextual parameter, found $other")
    assertEquals(contextual.name.toString, "inst")
    assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
    assertAppliedAdd(contextual.tpt)
    definition.tpt match
      case untpd.RefinedTypeTree(base, List(member: untpd.TypeDef)) =>
        assertAppliedAdd(base)
        assertEquals(member.name.toString, "Out")
        assert(!member.mods.hasFlags)
        member.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, "inst")
            assertEquals(selected.toString, "Out")
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected one-member RefinedTypeTree, found $other")
    definition.rhs match
      case untpd.Ident(name) => assertEquals(name.toString, "inst")
      case other => fail(s"expected stable identifier body, found $other")

  private def assertAppliedAdd(tree: untpd.Tree): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(constructor),
            List(untpd.Ident(first), untpd.Ident(second))
          ) =>
        assertEquals(constructor.toString, "Add")
        assertEquals(first.toString, "N")
        assertEquals(second.toString, "M")
      case other => fail(s"expected AppliedTypeTree(Add, N, M), found $other")

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
      case value: untpd.RefinedTypeTree =>
        s"Refined(${structure(value.tpt)},${value.refinements.map(structure)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
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
      case value: untpd.DefDef => value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty
