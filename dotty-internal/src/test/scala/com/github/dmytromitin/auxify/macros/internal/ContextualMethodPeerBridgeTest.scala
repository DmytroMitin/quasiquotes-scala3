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

  test("foreign AUXify package receives the exact bounded Add.Out bridge result") {
    withContext {
      assertScoped037(
        targetDefinition("apply", "N", "M", "Nat", "Add", "inst", "Out"),
        "AuxifyGeneratedAddApply.scala",
        "apply",
        "N",
        "M",
        "Nat",
        "Add",
        "inst",
        "Out"
      )
    }
  }

  test("foreign AUXify package can construct and lower renamed legal 037 spellings") {
    withContext {
      assertScoped037(
        targetDefinition(
          "derive",
          "Left",
          "Right",
          "Natural",
          "Combine",
          "evidence",
          "Result"
        ),
        "AuxifyGeneratedCombineApply.scala",
        "derive",
        "Left",
        "Right",
        "Natural",
        "Combine",
        "evidence",
        "Result"
      )
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

  test("foreign package receives scoped 037 failures without legacy fallback") {
    withContext {
      assertFailureDetail(
        parseDefinition(
          "def apply[N <: Nat, M <: Other](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst"
        ),
        "NEUTRAL_PROJECTION_FAILED",
        "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_MISMATCH"
      )
      assertFailureDetail(
        parseDefinition(
          "def apply[N <: Nat, M <: Nat](using inst: Add[M, N]): Add[M, N] { type Out = inst.Out } = inst"
        ),
        "NEUTRAL_PROJECTION_FAILED",
        "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH"
      )
      assertFailureDetail(
        parseDefinition(
          "inline def apply[N <: Nat, M <: Nat](using inst: Add[N, M]): Add[N, M] { type Out = inst.Out } = inst"
        ),
        "UNSUPPORTED_SCALAMETA_CONTEXTUAL_METHOD",
        "NEUTRAL_SCOPED037_DEFINITION_MODIFIERS_UNSUPPORTED"
      )
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

  private def assertFailureDetail(
      definition: Defn.Def,
      expectedCode: String,
      expectedNeutralCode: String
  )(using Context): Unit =
    val result = ContextualMethodPeerBridge.lower(definition, "Generated.scala")
    val failure = result.left.toOption.getOrElse(fail("malformed 037 unexpectedly lowered"))
    assertEquals(failure.code, expectedCode)
    assert(failure.detail.startsWith(s"$expectedNeutralCode:"), clues(failure))

  private def parseDefinition(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

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

  private def targetDefinition(
      methodName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      upperBoundName: String,
      constructorName: String,
      contextualParameterName: String,
      memberName: String
  ): Defn.Def =
    val method = Term.Name(methodName)
    val firstName = Type.Name(firstTypeParameterName)
    val secondName = Type.Name(secondTypeParameterName)
    val upperBound = Type.Name(upperBoundName)
    val constructor = Type.Name(constructorName)
    val contextualName = Term.Name(contextualParameterName)
    val selectedMember = Type.Name(memberName)
    val first: Type.Param = tparam"$firstName <: $upperBound"
    val second: Type.Param = tparam"$secondName <: $upperBound"
    val applied: Type = t"$constructor[..${List(firstName, secondName)}]"
    val selected: Type = t"$contextualName.$selectedMember"
    val refined: Type = t"$applied { type $selectedMember = $selected }"
    val definition: Defn.Def =
      q"def $method[..${List(first, second)}](using $contextualName: $applied): $refined = $contextualName"
    definition

  private def assertScoped037(
      definition: Defn.Def,
      virtualSourceName: String,
      methodName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      upperBoundName: String,
      constructorName: String,
      contextualParameterName: String,
      memberName: String
  )(using Context): Unit =
    val lowered = ContextualMethodPeerBridge
      .lower(definition, virtualSourceName)
      .fold(failure => fail(s"${failure.code}: ${failure.detail}"), identity)
    val method = lowered.tree

    assertEquals(lowered.virtualSourceName, virtualSourceName)
    assertEquals(method.source.path, virtualSourceName)
    assertEquals(method.name.toString, methodName)
    assertEquals(method.mods.flags, Flags.Method)
    assertEquals(
      method.leadingTypeParams.map(_.name.toString),
      List(firstTypeParameterName, secondTypeParameterName)
    )
    method.leadingTypeParams.foreach { parameter =>
      assertEquals(parameter.mods.flags, Flags.Param)
      parameter.rhs match
        case untpd.TypeBoundsTree(lo, untpd.Ident(hi), alias) =>
          assert(lo.isEmpty)
          assertEquals(hi.toString, upperBoundName)
          assert(alias.isEmpty)
        case other => fail(s"expected upper-only TypeBoundsTree, found $other")
    }
    val contextual = method.trailingParamss match
      case List(List(value: untpd.ValDef)) => value
      case other => fail(s"expected one contextual parameter, found $other")
    assertEquals(contextual.name.toString, contextualParameterName)
    assertEquals(contextual.mods.flags, Flags.Param | Flags.Given)
    assertApplied(
      contextual.tpt,
      constructorName,
      firstTypeParameterName,
      secondTypeParameterName
    )
    method.tpt match
      case untpd.RefinedTypeTree(base, List(member: untpd.TypeDef)) =>
        assertApplied(
          base,
          constructorName,
          firstTypeParameterName,
          secondTypeParameterName
        )
        assertEquals(member.name.toString, memberName)
        member.rhs match
          case untpd.Select(untpd.Ident(prefix), selected) =>
            assertEquals(prefix.toString, contextualParameterName)
            assertEquals(selected.toString, memberName)
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected one-member RefinedTypeTree, found $other")
    method.rhs match
      case untpd.Ident(name) => assertEquals(name.toString, contextualParameterName)
      case other => fail(s"expected body Ident($contextualParameterName), found $other")
    nonEmptyTrees(method).foreach { tree =>
      assert(tree.source.exists, clues(tree))
      assertEquals(tree.source.path, virtualSourceName, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0, clues(tree))
      assert(tree.span.start <= tree.span.point, clues(tree))
      assert(tree.span.point <= tree.span.end, clues(tree))
      assert(tree.span.end <= lowered.generatedSource.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol, clues(tree))
      assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
      directChildren(tree).foreach { child =>
        assert(child.span.start >= tree.span.start, clues(child))
        assert(child.span.end <= tree.span.end, clues(child))
      }
    }

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

  private def assertApplied(
      tree: untpd.Tree,
      constructor: String,
      firstArgument: String,
      secondArgument: String
  ): Unit =
    tree match
      case untpd.AppliedTypeTree(
            untpd.Ident(actualConstructor),
            List(untpd.Ident(actualFirst), untpd.Ident(actualSecond))
          ) =>
        assertEquals(actualConstructor.toString, constructor)
        assertEquals(actualFirst.toString, firstArgument)
        assertEquals(actualSecond.toString, secondArgument)
      case other =>
        fail(
          s"expected AppliedTypeTree($constructor, $firstArgument, $secondArgument), found $other"
        )

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
        .filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree =>
        value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
