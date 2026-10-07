package com.github.dmytromitin.auxify.macros.internal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.dotty.InstanceFactoryPeerBridge

import scala.meta.*
import scala.meta.dialects.Scala3

class InstanceFactoryPeerBridgeCurriedTest extends munit.FunSuite:
  private val Canonical =
    "def instance[A](combineFunction: A => A => A): Curried[A] = new Curried[A] { override def combine(a: A)(b: A): A = combineFunction(a)(b) }"
  private val Renamed =
    "def make[Element](mergeFunction: Element => Element => Element): Aggregator[Element] = new Aggregator[Element] { override def merge(left: Element)(right: Element): Element = mergeFunction(left)(right) }"

  test("public bridge lowers canonical and renamed curried factories as fresh exact 29-node graphs") {
    withContext {
      Vector(
        (Canonical, "instance", "A", "combineFunction", "Curried", "combine", "a", "b"),
        (Renamed, "make", "Element", "mergeFunction", "Aggregator", "merge", "left", "right")
      ).zipWithIndex.foreach {
        case ((source, factory, tparam, carrier, target, member, firstName, secondName), index) =>
          val definition = parse(source)
          val before = definition.structure
          val virtualSource = s"AuxifyGeneratedCurriedFactory$index.scala"
          val first = lower(definition, virtualSource)
          val second = lower(definition, virtualSource)

          assertEquals(definition.structure, before)
          assertEquals(first.generatedSource, source)
          assertEquals(second.generatedSource, source)
          assertEquals(first.virtualSourceName, virtualSource)
          assertCurriedShape(first.tree, factory, tparam, carrier, target, member, firstName, secondName)
          assertPositioned(first, source, 29)
          assertPositioned(second, source, 29)
          assertFreshAndDeterministic(first.tree, second.tree)
        }
    }
  }

  test("selected malformed curried candidates map through stable public failure categories") {
    withContext {
      assertFailure(
        Canonical.replace("combineFunction: A => A => A", "combine: A => A => A")
          .replace("combineFunction(a)(b)", "combine(a)(b)"),
        "INVALID_INSTANCE_FACTORY_NAME",
        "NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
      )
      assertFailure(
        s"private $Canonical",
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "NEUTRAL_CURRIED_METHOD_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED"
      )
      assertFailure(
        Canonical.replace("A => A => A", "(A, A) => A"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_TYPE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("new Curried[A]", "new Other[A]"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "NEUTRAL_CURRIED_METHOD_FACTORY_PARENT_TARGET_MISMATCH"
      )
      assertFailure(
        Canonical.replace("(b: A): A", "(b: String): A"),
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("combineFunction(a)(b)", "combineFunction(b)(a)"),
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH"
      )
      assertFailure(
        Canonical.replace("combineFunction(a)(b)", "combineFunction(a, b)"),
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_UNSUPPORTED"
      )
      assertFailure(
        parse(Canonical),
        "Bad\nName.scala",
        "INVALID_VIRTUAL_SOURCE_NAME",
        "LF"
      )
    }
  }

  test("non-curried adjacent method factories preserve the historical public failure") {
    withContext {
      val expected = InstanceFactoryPeerBridge.Failure(
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED: the ordinary outer clause must contain exactly two parameters in role order."
      )
      Vector(
        "def instance[A](combineFunction: A => A => A): Curried[A] = new Curried[A] { override def combine(a: A): A = combineFunction(a)(a) }",
        "def instance[A](combineFunction: A => A => A): Curried[A] = new Curried[A] { override def combine(a: A)(b: A)(c: A): A = combineFunction(a)(b) }",
        "def instance[A](combineFunction: A => A => A): Curried[A] = new Curried[A] { override def combine(a: A)(using b: A): A = combineFunction(a)(b) }",
        "def unrelated[A](value: A): Box[A] = new Box[A] { def transform(a: A)(b: A): A = a }"
      ).foreach { source =>
        val actual = InstanceFactoryPeerBridge
          .lower(parse(source), "Generated.scala")
          .left
          .toOption
          .getOrElse(fail("adjacent non-curried factory unexpectedly lowered"))
        assertEquals(actual, expected, clues(source))
      }
    }
  }

  private def assertCurriedShape(
      method: untpd.DefDef,
      factoryName: String,
      typeParameterName: String,
      carrierName: String,
      targetName: String,
      memberName: String,
      firstParameterName: String,
      secondParameterName: String
  )(using Context): Unit =
    assertEquals(method.name.toString, factoryName)
    method.paramss match
      case List(List(typeParameter: untpd.TypeDef), List(carrier: untpd.ValDef)) =>
        assertEquals(typeParameter.name.toString, typeParameterName)
        assertEquals(carrier.name.toString, carrierName)
        carrier.tpt match
          case untpd.Function(
                List(untpd.Ident(first)),
                untpd.Function(List(untpd.Ident(second)), untpd.Ident(result))
              ) =>
            assertEquals(first.toString, typeParameterName)
            assertEquals(second.toString, typeParameterName)
            assertEquals(result.toString, typeParameterName)
          case other => fail(s"expected nested unary carrier Type, found $other")
      case other => fail(s"expected one Type and one carrier parameter, found $other")
    assertApplied(method.tpt, targetName, typeParameterName)
    method.rhs match
      case untpd.New(template: untpd.Template) =>
        template.parentsOrDerived match
          case parent :: Nil => assertApplied(parent, targetName, typeParameterName)
          case other => fail(s"expected one target parent, found $other")
        template.body match
          case List(member: untpd.DefDef) =>
            assertEquals(member.name.toString, memberName)
            assertEquals(member.mods.flags, Flags.Override | Flags.Method)
            member.trailingParamss match
              case List(List(first: untpd.ValDef), List(second: untpd.ValDef)) =>
                assertEquals(first.name.toString, firstParameterName)
                assertEquals(second.name.toString, secondParameterName)
              case other => fail(s"expected two unary member clauses, found $other")
            member.rhs match
              case untpd.Apply(
                    untpd.Apply(untpd.Ident(callee), List(untpd.Ident(first))),
                    List(untpd.Ident(second))
                  ) =>
                assertEquals(callee.toString, carrierName)
                assertEquals(first.toString, firstParameterName)
                assertEquals(second.toString, secondParameterName)
              case other => fail(s"expected nested application body, found $other")
          case other => fail(s"expected one override method, found $other")
      case other => fail(s"expected anonymous implementation, found $other")

  private def assertPositioned(
      lowered: InstanceFactoryPeerBridge.Lowered,
      source: String,
      expectedNodes: Int
  )(using Context): Unit =
    val trees = nonEmptyTrees(lowered.tree)
    assertEquals(trees.size, expectedNodes)
    assertEquals(lowered.tree.span.start, 0)
    assertEquals(lowered.tree.span.end, source.length)
    trees.foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assertEquals(tree.source.path, lowered.virtualSourceName, clues(tree))
      assertEquals(tree.source.content.mkString, source, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0, clues(tree))
      assert(tree.span.start <= tree.span.point, clues(tree))
      assert(tree.span.point <= tree.span.end, clues(tree))
      assert(tree.span.end <= source.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol, clues(tree))
      assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
    }

  private def assertFreshAndDeterministic(
      first: untpd.DefDef,
      second: untpd.DefDef
  )(using Context): Unit =
    val firstTrees = nonEmptyTrees(first)
    val secondTrees = nonEmptyTrees(second)
    assertEquals(firstTrees.size, secondTrees.size)
    assert(!(first.source eq second.source))
    assertEquals(
      firstTrees.map(tree => (tree.span.start, tree.span.end, tree.span.point)),
      secondTrees.map(tree => (tree.span.start, tree.span.end, tree.span.point))
    )
    firstTrees.zip(secondTrees).foreach { case (left, right) =>
      assert(!(left eq right), clues(left, right))
    }

  private def assertApplied(
      tree: untpd.Tree,
      constructorName: String,
      argumentName: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(untpd.Ident(constructor), List(untpd.Ident(argument))) =>
        assertEquals(constructor.toString, constructorName)
        assertEquals(argument.toString, argumentName)
      case other => fail(s"expected $constructorName[$argumentName], found $other")

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
      .getOrElse(fail("malformed curried factory unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assert(failure.detail.contains(expectedDetail), clues(failure))

  private def lower(
      definition: Defn.Def,
      virtualSource: String
  )(using Context): InstanceFactoryPeerBridge.Lowered =
    InstanceFactoryPeerBridge
      .lower(definition, virtualSource)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def nonEmptyTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
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
