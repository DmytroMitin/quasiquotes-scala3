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

  test("foreign AUXify package receives canonical and renamed parameterless DefDefs") {
    withContext {
      val rows = List(
        (
          "def empty[A](using inst: Empty[A]): A = inst.empty",
          "empty",
          "A",
          "inst",
          "Empty",
          "AuxifyGeneratedEmpty.scala"
        ),
        (
          "def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain",
          "obtain",
          "Element",
          "evidence",
          "Provider",
          "AuxifyGeneratedObtain.scala"
        )
      )

      rows.foreach {
        case (source, method, typeParameter, contextual, constructor, virtualSource) =>
          val definition = parse(source)
          val originalStructure = definition.structure
          val lowered = DelegatedForwardingMethodPeerBridge
            .lower(definition, virtualSource)
            .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)
          val repeated = DelegatedForwardingMethodPeerBridge
            .lower(definition, virtualSource)
            .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

          assertEquals(lowered.generatedSource, source)
          assertEquals(lowered.virtualSourceName, virtualSource)
          assertParameterlessShape(
            lowered.tree,
            method,
            typeParameter,
            contextual,
            constructor
          )
          val trees = nonEmptyTrees(lowered.tree)
          val repeatedTrees = nonEmptyTrees(repeated.tree)
          assertEquals(trees.size, 10)
          assertEquals(repeatedTrees.size, 10)
          assert(lowered.tree ne repeated.tree)
          assert(lowered.tree.source ne repeated.tree.source)
          trees.zip(repeatedTrees).foreach { case (first, second) =>
            assert(first ne second, clues(first, second))
          }
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
          assertEquals(definition.structure, originalStructure)
          assertEquals(definition.syntax, source)
      }
    }
  }

  test("selected parameterless failures use the delegated-forwarding vocabulary") {
    withContext {
      val rows = List(
        "private def empty[A](using inst: Empty[A]): A = inst.empty" ->
          "DEFINITION_TOPOLOGY_UNSUPPORTED",
        "def empty[A, B](using inst: Empty[A]): A = inst.empty" ->
          "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "def empty[A <: Any](using inst: Empty[A]): A = inst.empty" ->
          "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "def empty[A](using first: Empty[A], second: Empty[A]): A = first.empty" ->
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "def empty[A](using inst: Empty[A] = ???): A = inst.empty" ->
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "def empty[A](using inst: pkg.Empty[A]): A = inst.empty" ->
          "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
        "def empty[A](using inst: Empty[String]): A = inst.empty" ->
          "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
        "def empty[A](using inst: Empty[A]) = inst.empty" ->
          "RESULT_TYPE_UNSUPPORTED",
        "def empty[A](using inst: Empty[A]): String = inst.empty" ->
          "RESULT_TYPE_UNSUPPORTED",
        "def empty[A](using inst: Empty[A]): A = other.empty" ->
          "BODY_RECEIVER_BINDER_MISMATCH",
        "def empty[A](using inst: Empty[A]): A = (inst: Empty[A]).empty" ->
          "BODY_SELECTION_UNSUPPORTED",
        "def empty[A](using inst: Empty[A]): A = inst.other" ->
          "BODY_SELECTED_METHOD_MISMATCH",
        "def empty[A](using inst: A[A]): A = inst.empty" ->
          "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
        "def `def`[A](using inst: Empty[A]): A = inst.`def`" ->
          "NAME_UNSUPPORTED"
      )

      rows.foreach { case (source, expectedCode) =>
        assertFailure(parse(source), "Generated.scala", expectedCode)
      }

      val base = parse("def empty[A](using inst: Empty[A]): A = inst.empty")
      val group = base.paramClauseGroups.head
      val clause = group.paramClauses.head
      val parameter = clause.values.head
      List(
        clause.copy(values = Nil),
        clause.copy(values = List(parameter.copy(mods = Nil))),
        clause.copy(values = List(parameter.copy(mods = List(Mod.Using(), Mod.Inline()))))
      ).foreach { changed =>
        assertFailure(
          regroup(
            base,
            List(group.copy(paramClauses = List(changed)))
          ),
          "Generated.scala",
          "CONTEXTUAL_PARAMETER_UNSUPPORTED"
        )
      }

      assertFailure(
        parse("def empty[A](using inst: Empty[A]): A = inst.empty"),
        "Bad\nName.scala",
        "GENERATED_ORIGIN_INVALID"
      )
    }
  }

  test("nonselected adjacent shapes retain the original 043 failure") {
    withContext {
      val rows = List(
        (
          "def empty[A]()(using inst: Empty[A]): A = inst.empty",
          "ORDINARY_PARAMETER_UNSUPPORTED",
          "the first clause must contain one unmodified, non-defaulted parameter."
        ),
        (
          "def empty[A](using inst: Empty[A]): A = inst.empty()",
          "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
          "the exact 043 method requires one ordinary clause followed by one final using clause."
        ),
        (
          "def empty[A](using one: Empty[A])(using two: Empty[A]): A = one.empty",
          "ORDINARY_CLAUSE_UNSUPPORTED",
          "the first value clause must be ordinary."
        ),
        (
          "def empty[A](inst: Empty[A]): A = inst.empty",
          "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
          "the exact 043 method requires one ordinary clause followed by one final using clause."
        )
      )

      rows.foreach { case (source, expectedCode, expectedDetail) =>
        assertFailureExact(parse(source), "Generated.scala", expectedCode, expectedDetail)
      }
    }
  }

  test("foreign boundary preserves every historical 043 code and detail") {
    withContext {
      val definitionDetail = "the exact 043 method has no definition modifiers."
      val typeParameterDetail =
        "the exact 043 method requires one unmodified, unbounded, unnested Type parameter."
      val valueClauseDetail =
        "the exact 043 method requires one ordinary clause followed by one final using clause."
      val rows = List(
        (
          "private def show[A](a: A)(using inst: Show[A]): String = inst.show(a)",
          "DEFINITION_TOPOLOGY_UNSUPPORTED",
          definitionDetail
        ),
        (
          "def show(a: A)(using inst: Show[A]): String = inst.show(a)",
          "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
          typeParameterDetail
        ),
        (
          "def show[A, B](a: A)(using inst: Show[A]): String = inst.show(a)",
          "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
          typeParameterDetail
        ),
        (
          "def show[A](a: A): String = a.toString",
          "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
          valueClauseDetail
        ),
        (
          "def show[A](using inst: Show[A])(a: A): String = inst.show(a)",
          "ORDINARY_CLAUSE_UNSUPPORTED",
          "the first value clause must be ordinary."
        ),
        (
          "def show[A](a: A, b: A)(using inst: Show[A]): String = inst.show(a)",
          "ORDINARY_PARAMETER_UNSUPPORTED",
          "the first clause must contain one unmodified, non-defaulted parameter."
        ),
        (
          "def show[A](a: A = ???)(using inst: Show[A]): String = inst.show(a)",
          "ORDINARY_PARAMETER_UNSUPPORTED",
          "the first clause must contain one unmodified, non-defaulted parameter."
        ),
        (
          "def show[A](a: => A)(using inst: Show[A]): String = inst.show(a)",
          "ORDINARY_PARAMETER_UNSUPPORTED",
          "the first clause must contain one unmodified, non-defaulted parameter."
        ),
        (
          "def show[A](a: A*)(using inst: Show[A]): String = inst.show(a)",
          "ORDINARY_PARAMETER_UNSUPPORTED",
          "the first clause must contain one unmodified, non-defaulted parameter."
        ),
        (
          "def show[A](a: String)(using inst: Show[A]): String = inst.show(a)",
          "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH",
          "the ordinary parameter Type must reference the declared method Type binder."
        ),
        (
          "def show[A](a: A)(inst: Show[A]): String = inst.show(a)",
          "CONTEXTUAL_CLAUSE_UNSUPPORTED",
          "the second and final value clause must be a using clause."
        ),
        (
          "def show[A](a: A)(using first: Show[A], second: Show[A]): String = first.show(a)",
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
          "the final using clause must contain one non-defaulted contextual parameter."
        ),
        (
          "def show[A](a: A)(using inst: Show[A] = ???): String = inst.show(a)",
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
          "the final using clause must contain one non-defaulted contextual parameter."
        ),
        (
          "def show[A](a: A)(using inst: pkg.Show[A]): String = inst.show(a)",
          "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
          "the contextual Type constructor must be one direct source name."
        ),
        (
          "def show[A](a: A)(using inst: Show[String]): String = inst.show(a)",
          "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
          "the contextual Type argument must reference the declared method Type binder."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): Box[String] = inst.show(a)",
          "RESULT_TYPE_UNSUPPORTED",
          "the exact 043 result Type must be one direct source name."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = inst",
          "BODY_APPLICATION_UNSUPPORTED",
          "the exact 043 body must be one application."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = show(a)",
          "BODY_SELECTION_UNSUPPORTED",
          "the applied function must be one direct selection."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = other.show(a)",
          "BODY_RECEIVER_BINDER_MISMATCH",
          "the selected receiver must be the exact contextual parameter."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = inst.render(a)",
          "BODY_SELECTED_METHOD_MISMATCH",
          "the selected member must equal the generated method name."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = inst.show(a, a)",
          "BODY_ARGUMENT_TOPOLOGY_UNSUPPORTED",
          "the selected method must receive exactly one direct Term-name argument."
        ),
        (
          "def show[A](a: A)(using inst: Show[A]): String = inst.show(other)",
          "BODY_ARGUMENT_BINDER_MISMATCH",
          "the application argument must be the exact ordinary parameter."
        ),
        (
          "def show[A <: Bound](a: A)(using inst: Show[A]): String = inst.show(a)",
          "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
          typeParameterDetail
        )
      )

      assertFailureExact(
        null,
        "Generated.scala",
        "DEFINITION_TOPOLOGY_UNSUPPORTED",
        "the Scalameta Defn.Def must be present."
      )
      rows.foreach { case (source, code, detail) =>
        assertFailureExact(parse(source), "Generated.scala", code, detail)
      }
      assertFailureExact(
        parse("def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"),
        "Bad\nName.scala",
        "GENERATED_ORIGIN_INVALID",
        "Invalid generated-origin virtual source name: NUL, CR, and LF are not permitted."
      )
    }
  }

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def regroup(
      definition: Defn.Def,
      groups: List[Member.ParamClauseGroup]
  ): Defn.Def =
    Defn.Def(definition.mods, definition.name, groups, definition.decltpe, definition.body)

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

  private def assertFailureExact(
      definition: Defn.Def,
      virtualSource: String,
      expectedCode: String,
      expectedDetail: String
  )(using Context): Unit =
    val failure = DelegatedForwardingMethodPeerBridge
      .lower(definition, virtualSource)
      .left
      .toOption
      .getOrElse(fail("malformed definition unexpectedly lowered"))
    assertEquals(failure.code, expectedCode, clues(failure))
    assertEquals(failure.detail, expectedDetail, clues(failure))

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

  private def assertParameterlessShape(
      method: untpd.DefDef,
      methodName: String,
      typeParameterName: String,
      contextualName: String,
      constructorName: String
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
      case List(List(contextual: untpd.ValDef)) =>
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
      case other => fail(s"expected only one contextual clause, found $other")
    assertIdent(method.tpt, typeParameterName)
    method.rhs match
      case untpd.Select(untpd.Ident(receiver), selected) =>
        assertEquals(receiver.toString, contextualName)
        assertEquals(selected.toString, methodName)
      case other => fail(s"expected direct parameterless selection, found $other")

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
