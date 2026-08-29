package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.*
import quasiquotes.parser.BinderId

class Phase134Auxify037InternalLoweringTest extends munit.FunSuite:
  import ScopedType.*

  private val CanonicalSource =
    "def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst"

  test("production source-free lowering matches the parser-characterized raw structure") {
    withContext {
      val parsedStructure = parseOne(CanonicalSource)(structure)
      val raw = ScopedContextualMethodUntypedLowerer
        .lower(validPlan())
        .fold(error => fail(error.message), identity)

      assertExactShape(raw, "apply", "N", "M", "Nat", "Add", "inst", "Out")
      assertEquals(structure(raw), parsedStructure)
      nonEmptyTrees(raw).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree.getClass.getSimpleName))
      }
    }
  }

  test("generated origin positions the complete exact tree on one virtual source") {
    withContext {
      val result = ScopedContextualMethodGeneratedOriginAdapter
        .lower(validPlan(), "generated/Phase134Auxify037.scala")
        .fold(error => fail(error.message), identity)
      val raw = result.tree.asInstanceOf[untpd.DefDef]

      assertEquals(result.generatedSource, CanonicalSource)
      assertEquals(result.virtualSourceName, "generated/Phase134Auxify037.scala")
      assertExactShape(raw, "apply", "N", "M", "Nat", "Add", "inst", "Out")
      nonEmptyTrees(raw).foreach { tree =>
        assert(tree.source.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.source.path, result.virtualSourceName)
        assert(tree.span.exists, clues(tree.getClass.getSimpleName))
        assert(tree.span.start >= 0, clues(tree.getClass.getSimpleName))
        assert(tree.span.start <= tree.span.point, clues(tree.getClass.getSimpleName))
        assert(tree.span.point <= tree.span.end, clues(tree.getClass.getSimpleName))
        assert(tree.span.end <= CanonicalSource.length, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree.getClass.getSimpleName))
        directChildren(tree).foreach { child =>
          assert(child.span.start >= tree.span.start)
          assert(child.span.end <= tree.span.end)
        }
      }
    }
  }

  test("generated origin preserves dynamic legal source names deterministically") {
    withContext {
      val plan = validPlan(
        methodName = "derive",
        firstName = "Left",
        secondName = "Right",
        upperBound = "Natural",
        constructor = "Combine",
        contextualName = "evidence",
        memberName = "Result"
      )
      val first = ScopedContextualMethodGeneratedOriginAdapter
        .lower(plan, "generated/DynamicAuxify037.scala")
        .fold(error => fail(error.message), identity)
      val second = ScopedContextualMethodGeneratedOriginAdapter
        .lower(plan, "generated/DynamicAuxify037.scala")
        .fold(error => fail(error.message), identity)

      assertEquals(
        first.generatedSource,
        "def derive[Left <: Natural, Right <: Natural](using evidence: Combine[Left, Right]): Combine[Left, Right] { type Result = evidence.Result } = evidence"
      )
      assertEquals(first.generatedSource, second.generatedSource)
      assertEquals(structure(first.tree), structure(second.tree))
    }
  }

  test("raw and generated-origin failures retain separate deterministic categories") {
    withContext {
      assertEquals(
        ScopedContextualMethodUntypedLowerer
          .lower(null)
          .left
          .toOption
          .map(_.code),
        Some("RAW_LOWERING_FAILED")
      )
      assertEquals(
        ScopedContextualMethodGeneratedOriginAdapter
          .lower(validPlan(), "bad\nsource.scala")
          .left
          .toOption
          .map(_.code),
        Some("GENERATED_ORIGIN_FAILED")
      )
    }
  }

  private def validPlan(
      methodName: String = "apply",
      firstName: String = "N",
      secondName: String = "M",
      upperBound: String = "Nat",
      constructor: String = "Add",
      contextualName: String = "inst",
      memberName: String = "Out"
  ): ScopedContextualMethodPlan =
    val first = ScopedTypeParameter(BinderId(1), firstName, SourceName(upperBound))
    val second = ScopedTypeParameter(BinderId(2), secondName, SourceName(upperBound))
    val contextualBinder = BinderId(3)
    val applied = Applied(
      SourceName(constructor),
      Vector(
        TypeParameterReference(first.binderId, first.displayName),
        TypeParameterReference(second.binderId, second.displayName)
      )
    )
    ScopedContextualMethodPlan
      .create(
        methodName,
        Vector(first, second),
        contextualBinder,
        contextualName,
        applied,
        Refinement(
          applied,
          Vector(
            ScopedTypeAlias(
              memberName,
              DirectStableSelected(contextualBinder, memberName)
            )
          )
        ),
        contextualBinder
      )
      .fold(error => fail(error.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne[A](source: String)(
      run: untpd.DefDef => A
  )(using outerContext: Context): A =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parser(SourceFile.virtual("Phase134Parser.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        run(packageDef.stats.head.asInstanceOf[untpd.DefDef])
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def assertExactShape(
      definition: untpd.DefDef,
      methodName: String,
      firstName: String,
      secondName: String,
      upperBound: String,
      constructorName: String,
      contextualName: String,
      memberName: String
  )(using Context): Unit =
    assertEquals(definition.name.toString, methodName)
    assertEquals(definition.mods.flags, Flags.Method)
    val typeParameters = definition.leadingTypeParams
    assertEquals(typeParameters.map(_.name.toString), List(firstName, secondName))
    typeParameters.foreach { parameter =>
      assertEquals(parameter.mods.flags, Flags.Param)
      parameter.rhs match
        case untpd.TypeBoundsTree(lo, untpd.Ident(hi), alias) =>
          assert(lo.isEmpty)
          assertEquals(hi.toString, upperBound)
          assert(alias.isEmpty)
        case other => fail(s"expected upper-only TypeBoundsTree, found $other")
    }
    val contextual = definition.trailingParamss match
      case List(List(value: untpd.ValDef)) => value
      case other => fail(s"expected one contextual parameter, found $other")
    assertEquals(contextual.name.toString, contextualName)
    assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
    assertApplied(contextual.tpt, constructorName, firstName, secondName)
    definition.tpt match
      case untpd.RefinedTypeTree(base, List(member: untpd.TypeDef)) =>
        assertApplied(base, constructorName, firstName, secondName)
        assertEquals(member.name.toString, memberName)
        member.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, contextualName)
            assertEquals(selected.toString, memberName)
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected one-member RefinedTypeTree, found $other")
    definition.rhs match
      case untpd.Ident(name) => assertEquals(name.toString, contextualName)
      case other => fail(s"expected stable identifier body, found $other")

  private def assertApplied(
      tree: untpd.Tree,
      constructorName: String,
      firstName: String,
      secondName: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(constructor),
            List(untpd.Ident(first), untpd.Ident(second))
          ) =>
        assertEquals(constructor.toString, constructorName)
        assertEquals(first.toString, firstName)
        assertEquals(second.toString, secondName)
      case other => fail(s"expected exact two-argument AppliedTypeTree, found $other")

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
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty
