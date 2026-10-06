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
  private val ValueCanonical =
    "def instance[A](valueValue: A): HasValue[A] = new HasValue[A] { override val value: A = valueValue }"
  private val ValueRenamed =
    "def make[Element](elementCarrier: Element): Container[Element] = new Container[Element] { override val element: Element = elementCarrier }"
  private val TypeMemberCanonical =
    "def instance[A, Out0]: HasOut[A] { type Out = Out0 } = new HasOut[A] { type Out = Out0 }"
  private val TypeMemberRenamed =
    "def make[Element, Result0]: Container[Element] { type Result = Result0 } = new Container[Element] { type Result = Result0 }"


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
  test("foreign package receives canonical and renamed one-value factories through the same bridge") {
    withContext {
      List(
        (ValueCanonical, "instance", "A", "valueValue", "HasValue", "value"),
        (ValueRenamed, "make", "Element", "elementCarrier", "Container", "element")
      ).zipWithIndex.foreach {
        case ((source, factory, tparam, carrier, target, member), index) =>
          val definition = parse(source)
          val before = definition.structure
          val first = lower(definition, s"AuxifyGeneratedValueFactory$index.scala")
          val second = lower(definition, s"AuxifyGeneratedValueFactory$index.scala")

          assertEquals(definition.structure, before)
          assertEquals(first.generatedSource, source)
          assertEquals(second.generatedSource, source)
          assertValueShape(first.tree, factory, tparam, carrier, target, member)
          assertPositioned(first, source, 17)
          assertFresh(first.tree, second.tree)
      }
    }
  }

  test("foreign package receives canonical and renamed Type-member factories through the same bridge") {
    withContext {
      List(
        (TypeMemberCanonical, "instance", "A", "Out0", "HasOut", "Out"),
        (TypeMemberRenamed, "make", "Element", "Result0", "Container", "Result")
      ).zipWithIndex.foreach {
        case ((source, factory, firstType, secondType, target, member), index) =>
          val definition = parse(source)
          val before = definition.structure
          val first = lower(definition, s"AuxifyGeneratedTypeMemberFactory$index.scala")
          val second = lower(definition, s"AuxifyGeneratedTypeMemberFactory$index.scala")

          assertEquals(definition.structure, before)
          assertEquals(first.generatedSource, source)
          assertEquals(second.generatedSource, source)
          assertTypeMemberShape(
            first.tree,
            factory,
            firstType,
            secondType,
            target,
            member
          )
          assertPositioned(first, source, 19)
          assertFresh(first.tree, second.tree)
      }
    }
  }

  test("selected sibling families map private failures into stable public categories") {
    withContext {
      assertFailure(
        "def instance[A](value: A): HasValue[A] = new HasValue[A] { override val value: A = value }",
        "INVALID_INSTANCE_FACTORY_NAME",
        "NEUTRAL_VALUE_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
      )
      assertFailure(
        "def instance[A](valueValue: A): HasValue[B] = new HasValue[A] { override val value: A = valueValue }",
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH"
      )
      assertFailure(
        "def instance[A](valueValue: A): HasValue[A] = new HasValue[A] { override val value: A = other }",
        "INVALID_INSTANCE_FACTORY_TERM_ROLE",
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH"
      )
      assertFailure(
        "def instance[A](valueValue: A): HasValue[A] = new HasValue[A] { val value: A = valueValue }",
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED"
      )
      assertFailure(
        "private def instance[A, Out0]: HasOut[A] { type Out = Out0 } = new HasOut[A] { type Out = Out0 }",
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED"
      )
      assertFailure(
        "def instance[A, Out0]: HasOut[A] { type Out = A } = new HasOut[A] { type Out = Out0 }",
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_RHS_MISMATCH"
      )
      assertFailure(
        "def instance[A, Out0]: HasOut[A] { type Out0 = Out0 } = new HasOut[A] { type Out0 = Out0 }",
        "INVALID_INSTANCE_FACTORY_NAME",
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED"
      )
      assertFailure(parse(ValueCanonical), null, "INVALID_VIRTUAL_SOURCE_NAME", "virtual source name")
      assertFailure(parse(TypeMemberCanonical), "Bad\nName.scala", "INVALID_VIRTUAL_SOURCE_NAME", "LF")
    }
  }

  test("adjacent shapes outside both new envelopes preserve the exact old failure") {
    withContext {
      val typeParameterFailure = InstanceFactoryPeerBridge.Failure(
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED: the bounded factory requires one unmodified, unbounded, non-higher-kinded Type parameter."
      )
      val outerClauseFailure = InstanceFactoryPeerBridge.Failure(
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED: the ordinary outer clause must contain exactly two parameters in role order."
      )
      List(
        "def instance[A, B](valueValue: A): HasValue[A] = new HasValue[A] { override val value: A = valueValue }" -> typeParameterFailure,
        "def instance[A](valueValue: A): HasValue[A] = new HasValue[A] { override def value: A = valueValue }" -> outerClauseFailure,
        "def instance[A](valueValue: A): HasValue[A] = new HasValue[A] { type value = A }" -> outerClauseFailure,
        "def instance[A, Out0](valueValue: A): HasOut[A] { type Out = Out0 } = new HasOut[A] { type Out = Out0 }" -> typeParameterFailure,
        "def instance[A, Out0]: HasOut[A] { type Out = Out0 } = new HasOut[A] { val Out: Out0 = ??? }" -> typeParameterFailure,
        "def unrelated[A](value: A): A = value" -> outerClauseFailure,
        "def odd[A](value: A): Box[A] = new Box[A] { val first: A = value; val second: A = value }" -> outerClauseFailure,
        "def instance[A](Out: A): HasOut[A] = new HasOut[A] { def value: A = Out }" -> outerClauseFailure
      ).foreach { case (source, expected) =>
        val actual = InstanceFactoryPeerBridge
          .lower(parse(source), "Generated.scala")
          .left
          .toOption
          .getOrElse(fail("adjacent malformed factory unexpectedly lowered"))
        assertEquals(actual, expected, clues(source))
      }
    }
  }
  test("existing malformed two-method cases preserve exact public failures") {
    withContext {
      val cases = Vector(
        (
          null,
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_SCALAMETA_DEFINITION",
            "DEFINITION_MISSING: the Scalameta Defn.Def must be present."
          )
        ),
        (
          parse(Canonical.replace("emptyValue: => A", "emptyValue: A")),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
            "EMPTY_VALUE_TYPE_ROLE_MISMATCH: the first outer parameter Type must be one Type.ByName over the factory Type binder."
          )
        ),
        (
          parse(Canonical.replace("(A, A) => A", "A => A")),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
            "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH: the binary function Type must contain two argument and one result references to the factory Type binder."
          )
        ),
        (
          parse(Canonical.replace("new Monoid[A]", "new Other[A]")),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TYPE_ROLE",
            "PARENT_TARGET_ROLE_MISMATCH: the anonymous parent must match the outer result constructor and Type binder."
          )
        ),
        (
          parse(Canonical.replace(
            "override def empty: A = emptyValue; override def combine(a: A, a1: A): A = combineFunction(a, a1)",
            "override def combine(a: A, a1: A): A = combineFunction(a, a1); override def empty: A = emptyValue"
          )),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY",
            "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED: the first member must be an override with no Type or value parameters."
          )
        ),
        (
          parse(Canonical.replace(
            "override def empty: A = emptyValue",
            "override def empty: A = combineFunction"
          )),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TERM_ROLE",
            "EMPTY_BODY_ROLE_MISMATCH: the empty body name must resolve to the exact outer by-name carrier binder."
          )
        ),
        (
          parse(Canonical.replace("combineFunction(a, a1)", "emptyValue(a, a1)")),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TERM_ROLE",
            "COMBINE_CALLEE_ROLE_MISMATCH: the combine callee must resolve to the exact outer binary-function carrier binder."
          )
        ),
        (
          parse(Canonical.replace(
            "combine(a: A, a1: A)",
            "combine(combineFunction: A, a1: A)"
          )),
          "Generated.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_INSTANCE_FACTORY_TERM_ROLE",
            "COMBINE_CALLEE_ROLE_MISMATCH: the combine callee must resolve to the exact outer binary-function carrier binder."
          )
        ),
        (
          parse(Canonical),
          null,
          InstanceFactoryPeerBridge.Failure(
            "INVALID_VIRTUAL_SOURCE_NAME",
            "the virtual source name must be present."
          )
        ),
        (
          parse(Canonical),
          "Bad\nName.scala",
          InstanceFactoryPeerBridge.Failure(
            "INVALID_VIRTUAL_SOURCE_NAME",
            "Invalid generated-origin virtual source name: NUL, CR, and LF are not permitted."
          )
        )
      )

      cases.foreach { case (definition, virtualSource, expected) =>
        val actual = InstanceFactoryPeerBridge
          .lower(definition, virtualSource)
          .left
          .toOption
          .getOrElse(fail("malformed two-method factory unexpectedly lowered"))
        assertEquals(actual, expected)
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
  private def lower(
      definition: Defn.Def,
      virtualSource: String
  )(using Context): InstanceFactoryPeerBridge.Lowered =
    InstanceFactoryPeerBridge
      .lower(definition, virtualSource)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)


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

  private def assertValueShape(
      method: untpd.DefDef,
      factoryName: String,
      typeParameterName: String,
      carrierName: String,
      targetName: String,
      memberName: String
  )(using Context): Unit =
    assertEquals(method.name.toString, factoryName)
    method.paramss match
      case List(
            List(typeParameter: untpd.TypeDef),
            List(carrier: untpd.ValDef)
          ) =>
        assertEquals(typeParameter.name.toString, typeParameterName)
        assertEquals(carrier.name.toString, carrierName)
        carrier.tpt match
          case untpd.Ident(name) => assertEquals(name.toString, typeParameterName)
          case other => fail(s"expected direct carrier Type, found $other")
      case other => fail(s"expected exact one-value parameter topology, found $other")
    assertApplied(method.tpt, targetName, typeParameterName)
    method.rhs match
      case untpd.New(template: untpd.Template) =>
        template.parentsOrDerived match
          case parent :: Nil => assertApplied(parent, targetName, typeParameterName)
          case other => fail(s"expected one one-value target parent, found $other")
        template.body match
          case List(member: untpd.ValDef) =>
            assertEquals(member.name.toString, memberName)
            assertEquals(member.mods.flags, Flags.Override)
            member.tpt match
              case untpd.Ident(name) => assertEquals(name.toString, typeParameterName)
              case other => fail(s"expected direct member Type, found $other")
            member.rhs match
              case untpd.Ident(name) => assertEquals(name.toString, carrierName)
              case other => fail(s"expected outer carrier reference, found $other")
          case other => fail(s"expected one override-val member, found $other")
      case other => fail(s"expected one-value anonymous implementation, found $other")

  private def assertTypeMemberShape(
      method: untpd.DefDef,
      factoryName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      targetName: String,
      memberName: String
  )(using Context): Unit =
    assertEquals(method.name.toString, factoryName)
    method.paramss match
      case List(List(first: untpd.TypeDef, second: untpd.TypeDef)) =>
        assertEquals(first.name.toString, firstTypeParameterName)
        assertEquals(second.name.toString, secondTypeParameterName)
      case other => fail(s"expected exact Type-member parameter topology, found $other")
    method.tpt match
      case refinement: untpd.RefinedTypeTree =>
        assertApplied(refinement.tpt, targetName, firstTypeParameterName)
        refinement.refinements match
          case List(alias: untpd.TypeDef) =>
            assertEquals(alias.name.toString, memberName)
            alias.rhs match
              case untpd.Ident(name) => assertEquals(name.toString, secondTypeParameterName)
              case other => fail(s"expected result alias RHS, found $other")
          case other => fail(s"expected one result refinement alias, found $other")
      case other => fail(s"expected refined result Type, found $other")
    method.rhs match
      case untpd.New(template: untpd.Template) =>
        template.parentsOrDerived match
          case parent :: Nil => assertApplied(parent, targetName, firstTypeParameterName)
          case other => fail(s"expected one Type-member target parent, found $other")
        template.body match
          case List(alias: untpd.TypeDef) =>
            assertEquals(alias.name.toString, memberName)
            alias.rhs match
              case untpd.Ident(name) => assertEquals(name.toString, secondTypeParameterName)
              case other => fail(s"expected anonymous alias RHS, found $other")
          case other => fail(s"expected one anonymous concrete alias, found $other")
      case other => fail(s"expected Type-member anonymous implementation, found $other")

  private def assertPositioned(
      lowered: InstanceFactoryPeerBridge.Lowered,
      source: String,
      expectedNodes: Int
  )(using Context): Unit =
    val trees = nonEmptyTrees(lowered.tree)
    assertEquals(trees.size, expectedNodes)
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

  private def assertFresh(
      first: untpd.DefDef,
      second: untpd.DefDef
  )(using Context): Unit =
    val firstTrees = nonEmptyTrees(first)
    val secondTrees = nonEmptyTrees(second)
    assertEquals(firstTrees.size, secondTrees.size)
    firstTrees.zip(secondTrees).foreach { case (left, right) =>
      assert(!(left eq right), clues(left, right))
    }

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
      case value: untpd.RefinedTypeTree =>
        value.tpt +: value.refinements.toVector
      case value: untpd.Template =>
        (Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body).filterNot(_.isEmpty)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
