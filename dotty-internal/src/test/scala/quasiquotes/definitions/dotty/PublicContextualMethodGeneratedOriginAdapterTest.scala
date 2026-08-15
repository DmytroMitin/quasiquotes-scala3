package quasiquotes.definitions.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.publicapi.{
  CompletedTerm,
  CompletedType,
  DefinitionConstruction,
  DefinitionResultView
}

class PublicContextualMethodGeneratedOriginAdapterTest extends munit.FunSuite:
  private val CanonicalSource =
    "def apply[A](using instance: Show[A]): Show[A] = instance"

  test("generates the exact bounded contextual method from public projections") {
    withContext {
      val result = lower(canonicalMethod(), "GeneratedShowCompanion.scala")

      assertEquals(result.generatedSource, CanonicalSource)
      assertEquals(result.generatedSource.length, 57)
      assertEquals(result.virtualSourceName, "GeneratedShowCompanion.scala")
      assertEquals(result.sourceFile.content.mkString, CanonicalSource)
      assertEquals(result.tree.getClass.getSimpleName, "DefDef")
    }
  }

  test("assigns the parser-observed complete structural span map") {
    withContext {
      val result = lower(canonicalMethod(), "GeneratedShowCompanion.scala")
      val method = result.tree.asInstanceOf[untpd.DefDef]
      val typeParameter = method.leadingTypeParams.head
      val bounds = typeParameter.rhs
      val parameter = method.trailingParamss.head.head.asInstanceOf[untpd.ValDef]
      val contextualType = parameter.tpt.asInstanceOf[untpd.AppliedTypeTree]
      val resultType = method.tpt.asInstanceOf[untpd.AppliedTypeTree]

      assertSpan(method, 0, 57, 4)
      assertSpan(typeParameter, 10, 11, 10)
      assertSpan(bounds, 10, 10, 10)
      assertSpan(parameter, 19, 36, 19)
      assertSpan(contextualType, 29, 36, 29)
      assertSpan(contextualType.tpt, 29, 33, 29)
      assertSpan(contextualType.args.head, 34, 35, 34)
      assertSpan(resultType, 39, 46, 39)
      assertSpan(resultType.tpt, 39, 43, 39)
      assertSpan(resultType.args.head, 44, 45, 44)
      assertSpan(method.rhs, 49, 57, 49)
    }
  }

  test("preserves one source identity and NoSymbol for every positioned node") {
    withContext {
      val result = lower(canonicalMethod(), "GeneratedShowCompanion.scala")
      val method = result.tree.asInstanceOf[untpd.DefDef]
      nonEmptyTrees(method).foreach { tree =>
        assert(tree.source.exists, clues(tree))
        assertEquals(tree.source.path, result.virtualSourceName, clues(tree))
        assertEquals(tree.source.content.mkString, result.generatedSource, clues(tree))
        assert(tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol, clues(tree))
        assert(!tree.isInstanceOf[untpd.TypedSplice], clues(tree))
      }
    }
  }

  test("keeps only empty synthetic children source-free and span-free") {
    withContext {
      val result = lower(canonicalMethod(), "GeneratedShowCompanion.scala")
      val method = result.tree.asInstanceOf[untpd.DefDef]
      val typeParameter = method.leadingTypeParams.head
      val parameter = method.trailingParamss.head.head.asInstanceOf[untpd.ValDef]

      typeParameter.rhs match
        case bounds: untpd.TypeBoundsTree =>
          assert(bounds.source.exists)
          assertSpan(bounds, 10, 10, 10)
          Vector(bounds.lo, bounds.hi).foreach(assertSyntheticEmpty)
        case other => fail(s"expected TypeBoundsTree, found $other")
      assertSyntheticEmpty(parameter.rhs)
    }
  }

  test("preserves the exact raw method flags, clauses, types, and body") {
    withContext {
      val method =
        lower(canonicalMethod(), "GeneratedShowCompanion.scala")
          .tree
          .asInstanceOf[untpd.DefDef]
      val typeParameter = method.leadingTypeParams.head
      val parameter = method.trailingParamss.head.head.asInstanceOf[untpd.ValDef]

      assertEquals(method.name.toString, "apply")
      assertEquals(method.mods.flags, Flags.Method)
      assertEquals(method.paramss.map(_.size), List(1, 1))
      assertEquals(typeParameter.name.toString, "A")
      assertEquals(typeParameter.mods.flags, Flags.Param)
      assertEquals(parameter.name.toString, "instance")
      assertEquals(parameter.mods.flags, Flags.Param | Flags.Given)
      assertApplied(parameter.tpt, "Show", "A")
      assertApplied(method.tpt, "Show", "A")
      method.rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "instance")
        case other => fail(s"expected body Ident(instance), found $other")
    }
  }

  test("renders other admitted bounded projections without parsing source") {
    withContext {
      val evidence = CompletedType.named("Evidence").toOption.get
      val value = CompletedType.typeParameter("Value").toOption.get
      val evidenceOfValue =
        CompletedType.applied(evidence, Vector(value)).toOption.get
      val body = CompletedTerm.reference("evidence").toOption.get
      val method = DefinitionConstruction
        .contextualMethod(
          "summonValue",
          "Value",
          "evidence",
          evidenceOfValue,
          evidenceOfValue,
          body
        )
        .toOption
        .get

      val result = lower(method, "GeneratedEvidence.scala")
      assertEquals(
        result.generatedSource,
        "def summonValue[Value](using evidence: Evidence[Value]): Evidence[Value] = evidence"
      )
    }
  }

  test("rejects null input and invalid virtual source names deterministically") {
    withContext {
      val nullResult = PublicContextualMethodGeneratedOriginAdapter.lower(
        null,
        "Generated.scala"
      )
      assert(
        nullResult.left.toOption.exists(
          _.isInstanceOf[
            PublicContextualMethodGeneratedOriginError.ProjectionPlanningFailure
          ]
        )
      )

      Vector("", " Generated.scala", "Generated.scala ", "Bad\nName.scala", "Bad\u0000Name.scala")
        .foreach { name =>
          val failure = PublicContextualMethodGeneratedOriginAdapter.lower(
            canonicalMethod(),
            name
          )
          assert(
            failure.left.toOption.exists(
              _.isInstanceOf[
                PublicContextualMethodGeneratedOriginError.InvalidVirtualSourceName
              ]
            ),
            clues(name, failure)
          )
        }
    }
  }

  test("leaves the contextual-method raw backend source-free and span-free") {
    withContext {
      val raw = PublicContextualMethodUntypedBackend
        .lower(canonicalMethod())
        .toOption
        .get
      allTreesIncludingEmpty(raw).foreach { tree =>
        assert(!tree.source.exists, clues(tree))
        assert(!tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol, clues(tree))
      }
    }
  }

  test("production adapter has no parser, placement, owner, symbol, or public route") {
    val source = Files.readString(
      Path.of(
        "dotty-internal",
        "src",
        "main",
        "scala",
        "quasiquotes",
        "definitions",
        "dotty",
        "PublicContextualMethodGeneratedOriginAdapter.scala"
      ),
      StandardCharsets.UTF_8
    )
    Vector(
      "dotty.tools.dotc.parsing",
      "Parser(",
      "new Parser",
      "owner =",
      "newSymbol",
      "entered",
      "typer",
      "macroparadise",
      "AUXify"
    ).foreach(value => assert(!source.contains(value), clues(value)))
    assert(!Files.exists(Path.of("core", "src", "main", "scala", "quasiquotes", "publicapi", "PublicContextualMethodGeneratedOriginAdapter.scala")))
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def canonicalMethod(): DefinitionResultView =
    val show = CompletedType.named("Show").toOption.get
    val binder = CompletedType.typeParameter("A").toOption.get
    val showOfA = CompletedType.applied(show, Vector(binder)).toOption.get
    val body = CompletedTerm.reference("instance").toOption.get
    DefinitionConstruction
      .contextualMethod(
        "apply",
        "A",
        "instance",
        showOfA,
        showOfA,
        body
      )
      .toOption
      .get

  private def lower(
      method: DefinitionResultView,
      virtualSourceName: String
  )(using Context): GeneratedOriginDefinitionResult =
    PublicContextualMethodGeneratedOriginAdapter.lower(method, virtualSourceName) match
      case Right(result) => result
      case Left(error) => fail(error.message)

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

  private def assertSpan(
      tree: untpd.Tree,
      start: Int,
      end: Int,
      point: Int
  ): Unit =
    assert(tree.span.exists, clues(tree))
    assertEquals(tree.span.start, start, clues(tree))
    assertEquals(tree.span.end, end, clues(tree))
    assertEquals(tree.span.point, point, clues(tree))

  private def assertSyntheticEmpty(tree: untpd.Tree)(using Context): Unit =
    assert(tree.isEmpty, clues(tree))
    assert(!tree.source.exists, clues(tree))
    assert(!tree.span.exists, clues(tree))
    assertEquals(tree.symbol, NoSymbol, clues(tree))

  private def nonEmptyTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(nonEmptyTrees)

  private def allTreesIncludingEmpty(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTreesIncludingEmpty)

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
