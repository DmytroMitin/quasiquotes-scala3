package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.DelegatedForwardingMethodPeerBridge

import scala.meta.*
import scala.meta.dialects.Scala3

class DelegatedForwardingMethodPeerBridgeTest extends munit.FunSuite:
  test("foreign AUXify package receives canonical and renamed insertion-ready DefDefs") {
    withContext {
      val rows = List(
        (
          "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)",
          "show",
          "A",
          "a",
          "inst",
          "Show",
          "String",
          "AuxifyGeneratedShow.scala"
        ),
        (
          "def render[Element](value: Element)(using evidence: Display[Element]): Text = evidence.render(value)",
          "render",
          "Element",
          "value",
          "evidence",
          "Display",
          "Text",
          "AuxifyGeneratedRender.scala"
        )
      )

      rows.foreach {
        case (
              source,
              method,
              typeParameter,
              ordinary,
              contextual,
              constructor,
              result,
              virtualSource
            ) =>
          val lowered: DelegatedForwardingMethodPeerBridge.Lowered =
            DelegatedForwardingMethodPeerBridge
              .lower(parse(source), virtualSource)
              .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

          assertEquals(lowered.generatedSource, source)
          assertEquals(lowered.virtualSourceName, virtualSource)
          assertExactShape(
            lowered.tree,
            method,
            typeParameter,
            ordinary,
            contextual,
            constructor,
            result
          )
          val trees = nonEmptyTrees(lowered.tree)
          assertEquals(trees.size, 14)
          trees.foreach { tree =>
            assert(tree.source.exists, clues(tree))
            assertEquals(tree.source.path, virtualSource, clues(tree))
            assertEquals(tree.source.content.mkString, source, clues(tree))
            assert(tree.span.exists, clues(tree))
            assert(tree.span.start >= 0, clues(tree))
            assert(tree.span.start <= tree.span.point, clues(tree))
            assert(tree.span.point <= tree.span.end, clues(tree))
            assert(tree.span.end <= source.length, clues(tree))
            assertEquals(tree.symbol, NoSymbol, clues(tree))
            assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
            directChildren(tree).foreach { child =>
              assert(child.span.start >= tree.span.start, clues(child))
              assert(child.span.end <= tree.span.end, clues(child))
            }
          }
      }
    }
  }

  test("foreign boundary preserves representative 043 failure categories") {
    withContext {
      assertFailure(null, "Generated.scala", "DEFINITION_TOPOLOGY_UNSUPPORTED")
      assertFailure(
        parse("def show[A, B](a: A)(using inst: Show[A]): String = inst.show(a)"),
        "Generated.scala",
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"
      )
      assertFailure(
        parse("def show[A](a: A): String = a.toString"),
        "Generated.scala",
        "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED"
      )
      assertFailure(
        parse("def show[A](a: A)(using inst: Show[A]): String = inst.render(a)"),
        "Generated.scala",
        "BODY_SELECTED_METHOD_MISMATCH"
      )
      assertFailure(
        parse("def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"),
        "Bad\nName.scala",
        "GENERATED_ORIGIN_INVALID"
      )
    }
  }

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def assertFailure(
      definition: Defn.Def,
      virtualSource: String,
      expectedCode: String
  )(using Context): Unit =
    val failure = DelegatedForwardingMethodPeerBridge
      .lower(definition, virtualSource)
      .left
      .toOption
      .getOrElse(fail("malformed 043 definition unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.nonEmpty, clues(failure))

  private def assertExactShape(
      method: untpd.DefDef,
      methodName: String,
      typeParameterName: String,
      ordinaryName: String,
      contextualName: String,
      constructorName: String,
      resultTypeName: String
  )(using Context): Unit =
    assertEquals(method.name.toString, methodName)
    assertEquals(method.mods.flags, Flags.Method)
    val typeParameter = method.leadingTypeParams match
      case value :: Nil => value
      case other => fail(s"expected one Type parameter, found $other")
    assertEquals(typeParameter.name.toString, typeParameterName)
    assertEquals(typeParameter.mods.flags, Flags.Param)
    typeParameter.rhs match
      case untpd.TypeBoundsTree(lo, hi, alias) =>
        assert(lo.isEmpty)
        assert(hi.isEmpty)
        assert(alias.isEmpty)
      case other => fail(s"expected unbounded TypeBoundsTree, found $other")
    method.trailingParamss match
      case List(List(ordinary: untpd.ValDef), List(contextual: untpd.ValDef)) =>
        assertEquals(ordinary.name.toString, ordinaryName)
        assertEquals(ordinary.mods.flags, Flags.Param)
        assertIdent(ordinary.tpt, typeParameterName)
        assertEquals(contextual.name.toString, contextualName)
        assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
        contextual.tpt match
          case untpd.AppliedTypeTree(
                untpd.Ident(constructor),
                List(untpd.Ident(argument))
              ) =>
            assertEquals(constructor.toString, constructorName)
            assertEquals(argument.toString, typeParameterName)
          case other => fail(s"expected unary contextual AppliedTypeTree, found $other")
      case other => fail(s"expected ordinary and contextual clauses, found $other")
    assertIdent(method.tpt, resultTypeName)
    method.rhs match
      case untpd.Apply(
            untpd.Select(untpd.Ident(receiver), selected),
            List(untpd.Ident(argument))
          ) =>
        assertEquals(receiver.toString, contextualName)
        assertEquals(selected.toString, methodName)
        assertEquals(argument.toString, ordinaryName)
      case other => fail(s"expected selected one-argument application, found $other")

  private def assertIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case untpd.Ident(name) => assertEquals(name.toString, expected)
      case other => fail(s"expected Ident($expected), found $other")

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
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
