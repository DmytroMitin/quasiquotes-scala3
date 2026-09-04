package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.InstanceFactoryPeerBridge

import scala.meta.*
import scala.meta.dialects.Scala3

class InstanceFactoryPeerBridgeTest extends munit.FunSuite:
  private val Canonical =
    "def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] { override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1) }"
  private val Renamed =
    "def make[Element](fallbackValue: => Element, selection: (Element, Element) => Element): Choice[Element] = new Choice[Element] { override def fallback: Element = fallbackValue; override def select(left: Element, right: Element): Element = selection(left, right) }"

  test("foreign package receives canonical and renamed insertion-ready factories") {
    withContext {
      List(
        (Canonical, "instance", "A", "emptyValue", "combineFunction", "Monoid", "empty", "combine", "a", "a1"),
        (Renamed, "make", "Element", "fallbackValue", "selection", "Choice", "fallback", "select", "left", "right")
      ).zipWithIndex.foreach {
        case ((source, factory, tparam, emptyCarrier, functionCarrier, target, emptyMember, combineMember, first, second), index) =>
          val virtualSource = s"AuxifyGeneratedInstanceFactory$index.scala"
          val lowered: InstanceFactoryPeerBridge.Lowered =
            InstanceFactoryPeerBridge
              .lower(parse(source), virtualSource)
              .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

          assertEquals(lowered.generatedSource, source)
          assertEquals(lowered.virtualSourceName, virtualSource)
          assertExactShape(
            lowered.tree,
            factory,
            tparam,
            emptyCarrier,
            functionCarrier,
            target,
            emptyMember,
            combineMember,
            first,
            second
          )
          val trees = nonEmptyTrees(lowered.tree)
          assertEquals(trees.size, 33)
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
          }
        }
    }
  }

  test("foreign package receives stable bounded projection diagnostics") {
    withContext {
      assertFailure(null, "Generated.scala", "INVALID_SCALAMETA_DEFINITION", "DEFINITION_MISSING")
      assertFailure(
        Canonical.replace("emptyValue: => A", "emptyValue: A"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "EMPTY_VALUE_TYPE_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("(A, A) => A", "A => A"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("new Monoid[A]", "new Other[A]"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "PARENT_TARGET_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace(
          "override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1)",
          "override def combine(a: A, a1: A): A = combineFunction(a, a1); override def empty: A = emptyValue"
        ),
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED"
      )
      assertFailure(
        Canonical.replace("override def empty: A = emptyValue", "override def empty: A = combineFunction"),
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "EMPTY_BODY_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("combineFunction(a, a1)", "emptyValue(a, a1)"),
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "COMBINE_CALLEE_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("combine(a: A, a1: A)", "combine(combineFunction: A, a1: A)"),
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "COMBINE_CALLEE_ROLE_MISMATCH"
      )
    }
  }

  test("foreign package rejects malformed provenance input before returning a tree") {
    withContext {
      assertFailure(parse(Canonical), null, "INVALID_VIRTUAL_SOURCE_NAME", "virtual source name")
      assertFailure(parse(Canonical), "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME", "LF")
    }
  }

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def assertFailure(
      source: String,
      expectedCode: String,
      expectedDetail: String
  )(using Context): Unit =
    assertFailure(parse(source), "Generated.scala", expectedCode, expectedDetail)

  private def assertFailure(
      definition: Defn.Def,
      virtualSource: String,
      expectedCode: String,
      expectedDetail: String
  )(using Context): Unit =
    val failure = InstanceFactoryPeerBridge
      .lower(definition, virtualSource)
      .left
      .toOption
      .getOrElse(fail("malformed instance factory unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.contains(expectedDetail), clues(failure))

  private def assertExactShape(
      method: untpd.DefDef,
      factoryName: String,
      typeParameterName: String,
      emptyCarrierName: String,
      functionCarrierName: String,
      targetName: String,
      emptyMemberName: String,
      combineMemberName: String,
      firstNestedName: String,
      secondNestedName: String
  )(using Context): Unit =
    assertEquals(method.name.toString, factoryName)
    assertEquals(method.mods.flags, Flags.Method)
    method.paramss match
      case List(
            List(typeParameter: untpd.TypeDef),
            List(emptyCarrier: untpd.ValDef, functionCarrier: untpd.ValDef)
          ) =>
        assertEquals(typeParameter.name.toString, typeParameterName)
        assertEquals(typeParameter.mods.flags, Flags.Param)
        assertEquals(emptyCarrier.name.toString, emptyCarrierName)
        assertEquals(emptyCarrier.mods.flags, Flags.Param)
        emptyCarrier.tpt match
          case untpd.ByNameTypeTree(untpd.Ident(name)) =>
            assertEquals(name.toString, typeParameterName)
          case other => fail(s"expected by-name carrier Type, found $other")
        assertEquals(functionCarrier.name.toString, functionCarrierName)
        assertEquals(functionCarrier.mods.flags, Flags.Param)
        functionCarrier.tpt match
          case untpd.Function(
                List(untpd.Ident(first), untpd.Ident(second)),
                untpd.Ident(result)
              ) =>
            assertEquals(first.toString, typeParameterName)
            assertEquals(second.toString, typeParameterName)
            assertEquals(result.toString, typeParameterName)
          case other => fail(s"expected binary function Type, found $other")
      case other => fail(s"expected exact factory parameter topology, found $other")
    assertApplied(method.tpt, targetName, typeParameterName)
    method.rhs match
      case untpd.New(template: untpd.Template) =>
        template.parentsOrDerived match
          case parent :: Nil => assertApplied(parent, targetName, typeParameterName)
          case other => fail(s"expected one target parent, found $other")
        template.body match
          case List(emptyMember: untpd.DefDef, combineMember: untpd.DefDef) =>
            assertEquals(emptyMember.name.toString, emptyMemberName)
            emptyMember.rhs match
              case untpd.Ident(name) => assertEquals(name.toString, emptyCarrierName)
              case other => fail(s"expected empty carrier reference, found $other")
            assertEquals(combineMember.name.toString, combineMemberName)
            combineMember.trailingParamss match
              case List(List(first: untpd.ValDef, second: untpd.ValDef)) =>
                assertEquals(first.name.toString, firstNestedName)
                assertEquals(second.name.toString, secondNestedName)
              case other => fail(s"expected two combine parameters, found $other")
            combineMember.rhs match
              case untpd.Apply(
                    untpd.Ident(callee),
                    List(untpd.Ident(first), untpd.Ident(second))
                  ) =>
                assertEquals(callee.toString, functionCarrierName)
                assertEquals(first.toString, firstNestedName)
                assertEquals(second.toString, secondNestedName)
              case other => fail(s"expected exact combine application, found $other")
          case other => fail(s"expected two override members, found $other")
      case other => fail(s"expected anonymous implementation, found $other")

  private def assertApplied(
      tree: untpd.Tree,
      constructorName: String,
      argumentName: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(constructor),
            List(untpd.Ident(argument))
          ) =>
        assertEquals(constructor.toString, constructorName)
        assertEquals(argument.toString, argumentName)
      case other => fail(s"expected $constructorName[$argumentName], found $other")

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
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
