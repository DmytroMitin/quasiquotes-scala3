package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.ContextualMethodPeerBridge

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class ContextualMethodPeerBridgeTest extends munit.FunSuite:
  private val CanonicalSource =
    "def apply[A](using inst: Show[A]): Show[A] = inst"

  test("foreign package receives the exact insertion-ready DefDef and provenance") {
    withContext {
      val definition =
        q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

      val lowered: ContextualMethodPeerBridge.Lowered =
        ContextualMethodPeerBridge
          .lower(definition, "AuxifyGeneratedShowApply.scala")
          .fold(failure => fail(s"${failure.code}: ${failure.detail}"), identity)
      val method: untpd.DefDef = lowered.tree

      assertEquals(lowered.generatedSource, CanonicalSource)
      assertEquals(lowered.virtualSourceName, "AuxifyGeneratedShowApply.scala")
      assertEquals(method.source.path, "AuxifyGeneratedShowApply.scala")
      assertEquals(method.name.toString, "apply")
      assertEquals(method.mods.flags, Flags.Method)
      assertEquals(method.leadingTypeParams.map(_.name.toString), List("A"))
      assertEquals(method.leadingTypeParams.head.mods.flags, Flags.Param)
      val contextual = method.trailingParamss.head.head.asInstanceOf[untpd.ValDef]
      assertEquals(contextual.name.toString, "inst")
      assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
      assertApplied(contextual.tpt, "Show", "A")
      assertApplied(method.tpt, "Show", "A")
      method.rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "inst")
        case other => fail(s"expected body Ident(inst), found $other")
      nonEmptyTrees(method).foreach { tree =>
        assert(tree.source.exists, clues(tree))
        assertEquals(tree.source.path, "AuxifyGeneratedShowApply.scala", clues(tree))
        assert(tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol, clues(tree))
      }
    }
  }

  test("foreign package receives deterministic compact failure classifications") {
    withContext {
      val ordinaryClause =
        q"def apply[A](inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]
      val invalidValidatedName = syntheticApply("bad-name", "inst")
      val canonical = syntheticApply("apply", "inst")

      assertFailure(null, "Generated.scala", "INVALID_SCALAMETA_DEFINITION")
      assertFailure(
        ordinaryClause,
        "Generated.scala",
        "UNSUPPORTED_SCALAMETA_CONTEXTUAL_METHOD"
      )
      assertFailure(
        invalidValidatedName,
        "Generated.scala",
        "NEUTRAL_PROJECTION_FAILED"
      )
      assertFailure(canonical, "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME")
      assertFailure(canonical, null, "INVALID_VIRTUAL_SOURCE_NAME")
    }
  }

  private def assertFailure(
      definition: Defn.Def,
      virtualSourceName: String,
      expectedCode: String
  )(using Context): Unit =
    val result = ContextualMethodPeerBridge.lower(definition, virtualSourceName)
    assertEquals(result.left.toOption.map(_.code), Some(expectedCode), clues(result))
    assert(result.left.toOption.exists(_.detail.nonEmpty), clues(result))

  private def syntheticApply(methodName: String, parameterName: String): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Type.Bounds(None, None, Nil, Nil)
    )
    val showOfA =
      Type.Apply(Type.Name("Show"), Type.ArgClause(List(Type.Name("A"))))
    val contextualParameter =
      Term.Param(Nil, Term.Name(parameterName), Some(showOfA), None)
    val parameterGroup = Member.ParamClauseGroup(
      Type.ParamClause(List(typeParameter)),
      List(Term.ParamClause(List(contextualParameter), Some(Mod.Using())))
    )
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(parameterGroup),
      Some(showOfA),
      Term.Name(parameterName)
    )

  private def assertApplied(
      tree: untpd.Tree,
      constructor: String,
      argument: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(actualConstructor),
            List(untpd.Ident(actualArgument))
          ) =>
        assertEquals(actualConstructor.toString, constructor)
        assertEquals(actualArgument.toString, argument)
      case other => fail(s"expected AppliedTypeTree($constructor, $argument), found $other")

  private def nonEmptyTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
