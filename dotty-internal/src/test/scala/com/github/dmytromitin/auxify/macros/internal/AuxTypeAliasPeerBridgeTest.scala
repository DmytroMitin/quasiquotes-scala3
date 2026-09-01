package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.Param
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.AuxTypeAliasPeerBridge

import scala.meta.*
import scala.meta.dialects.Scala3

class AuxTypeAliasPeerBridgeTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("foreign AUXify package receives the canonical generated TypeDef") {
    withContext {
      val lowered = lower(
        parseAlias(Canonical),
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "generated/Auxify039Aux.scala"
      )

      assertEquals(lowered.generatedSource, Canonical)
      assertEquals(lowered.virtualSourceName, "generated/Auxify039Aux.scala")
      assertAlias(lowered.tree, "Aux", "N", "M", "Out0", "Nat", "Add", "Out")
      assertGeneratedOrigin(lowered)
    }
  }

  test("foreign AUXify package receives fully renamed legal generated output") {
    withContext {
      val source =
        "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
      val lowered = lower(
        parseAlias(source),
        "Evidence", "Left", "Domain", "Right", "Domain", "Result0", "Domain", "Combine", "Result",
        "generated/Auxify039Evidence.scala"
      )

      assertEquals(lowered.generatedSource, source)
      assertAlias(
        lowered.tree,
        "Evidence", "Left", "Right", "Result0", "Domain", "Combine", "Result"
      )
      assertGeneratedOrigin(lowered)
    }
  }

  test("N projection owns malformed topology and expectation mismatches") {
    withContext {
      val canonical = parseAlias(Canonical)
      val malformed = parseAlias(
        "type Aux[N <: Nat, M <: Nat] = Add[N, M] { type Out = N }"
      )
      assertFailure(
        malformed,
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_TYPE_PARAMETER_ARITY_UNSUPPORTED"
      )
      assertFailure(
        canonical,
        "Other", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_ALIAS_NAME_MISMATCH"
      )
      assertFailure(
        canonical,
        "Aux", "Left", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_TYPE_PARAMETER_NAME_MISMATCH"
      )
      assertFailure(
        canonical,
        "Aux", "N", "Domain", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_MISMATCH"
      )
      assertFailure(
        canonical,
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Combine", "Out",
        "NEUTRAL_AUX_TARGET_NAME_MISMATCH"
      )
      assertFailure(
        canonical,
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Result",
        "NEUTRAL_AUX_REFINEMENT_MEMBER_NAME_MISMATCH"
      )
    }
  }

  test("missing input fields and virtual source names fail at controlled boundaries") {
    withContext {
      val canonical = parseAlias(Canonical)
      assertFailure(
        null,
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_ALIAS_MISSING"
      )
      val firstBoundary = AuxTypeAliasPeerBridge.lower(
        null,
        "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "bad\nname.scala"
      )
      assertEquals(
        firstBoundary.left.toOption.map(_.code),
        Some("NEUTRAL_AUX_ALIAS_MISSING")
      )
      assertFailure(
        canonical,
        null, "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
        "NEUTRAL_AUX_EXPECTATION_INVALID"
      )
      Vector(null, "", "bad\nname.scala", "bad\rname.scala", "bad\u0000name.scala")
        .foreach { virtualSourceName =>
          val result = AuxTypeAliasPeerBridge.lower(
            canonical,
            "Aux", "N", "Nat", "M", "Nat", "Out0", "Nat", "Add", "Out",
            virtualSourceName
          )
          assertEquals(
            result.left.toOption.map(_.code),
            Some("INVALID_VIRTUAL_SOURCE_NAME"),
            clues(virtualSourceName, result)
          )
        }
    }
  }

  private def lower(
      definition: Defn.Type,
      alias: String,
      first: String,
      firstBound: String,
      second: String,
      secondBound: String,
      output: String,
      outputBound: String,
      target: String,
      member: String,
      virtualSourceName: String
  )(using Context): AuxTypeAliasPeerBridge.Lowered =
    AuxTypeAliasPeerBridge
      .lower(
        definition,
        alias,
        first,
        firstBound,
        second,
        secondBound,
        output,
        outputBound,
        target,
        member,
        virtualSourceName
      )
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertFailure(
      definition: Defn.Type,
      alias: String,
      first: String,
      firstBound: String,
      second: String,
      secondBound: String,
      output: String,
      outputBound: String,
      target: String,
      member: String,
      expectedCode: String
  )(using Context): Unit =
    val result = AuxTypeAliasPeerBridge.lower(
      definition,
      alias,
      first,
      firstBound,
      second,
      secondBound,
      output,
      outputBound,
      target,
      member,
      "generated/Auxify039Failure.scala"
    )
    assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))
    assert(result.left.toOption.exists(_.detail.nonEmpty), clues(result))

  private def assertAlias(
      tree: untpd.TypeDef,
      alias: String,
      first: String,
      second: String,
      output: String,
      bound: String,
      target: String,
      member: String
  )(using Context): Unit =
    assertEquals(tree.name.toString, alias)
    tree.rhs match
      case untpd.LambdaTypeTree(parameters, untpd.RefinedTypeTree(base, List(refinement: untpd.TypeDef))) =>
        assertEquals(parameters.map(_.name.toString), List(first, second, output))
        parameters.foreach { parameter =>
          assertEquals(parameter.mods.flags, Param)
          parameter.rhs match
            case untpd.TypeBoundsTree(lo, untpd.Ident(upper), aliasTree) =>
              assert(lo.isEmpty)
              assertEquals(upper.toString, bound)
              assert(aliasTree.isEmpty)
            case other => fail(s"expected upper-only TypeBoundsTree, found $other")
        }
        base match
          case untpd.AppliedTypeTree(untpd.Ident(constructor), List(untpd.Ident(left), untpd.Ident(right))) =>
            assertEquals(constructor.toString, target)
            assertEquals(left.toString, first)
            assertEquals(right.toString, second)
          case other => fail(s"expected two-argument applied target, found $other")
        assertEquals(refinement.name.toString, member)
        refinement.rhs match
          case untpd.Ident(name) => assertEquals(name.toString, output)
          case other => fail(s"expected direct output binder reference, found $other")
      case other => fail(s"expected bounded alias TypeDef, found $other")

  private def assertGeneratedOrigin(
      lowered: AuxTypeAliasPeerBridge.Lowered
  )(using Context): Unit =
    val trees = allTrees(lowered.tree)
    assertEquals(trees.size, 18)
    trees.foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assertEquals(tree.source.path, lowered.virtualSourceName, clues(tree))
      assertEquals(tree.source.content.mkString, lowered.generatedSource, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0, clues(tree))
      assert(tree.span.start <= tree.span.point, clues(tree))
      assert(tree.span.point <= tree.span.end, clues(tree))
      assert(tree.span.end <= lowered.generatedSource.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol, clues(tree))
      assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def parseAlias(source: String): Defn.Type =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Type]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
