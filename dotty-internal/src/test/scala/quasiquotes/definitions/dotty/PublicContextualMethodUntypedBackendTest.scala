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

class PublicContextualMethodUntypedBackendTest extends munit.FunSuite:
  test("lowers the exact bounded public contextual method") {
    withContext {
      val raw = lower(method("provide", "A", "show"))

      assertEquals(raw.name.toString, "provide")
      assertEquals(raw.mods.flags, Flags.Method)
      assertEquals(raw.paramss.map(_.size), List(1, 1))
      assertEquals(raw.leadingTypeParams.map(_.name.toString), List("A"))
      assertEquals(raw.trailingParamss.size, 1)
      raw.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "A")
        case other => fail(s"expected result Ident(A), found $other")
      raw.rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "show")
        case other => fail(s"expected body Ident(show), found $other")
    }
  }

  test("matches the parser-probed type and contextual parameter clauses") {
    withContext {
      val raw = lower(method("provide", "A", "show"))
      val typeParameter = raw.leadingTypeParams.head
      val contextualParameter = raw.trailingParamss.head.head.asInstanceOf[untpd.ValDef]

      assertEquals(typeParameter.mods.flags, Flags.Param)
      typeParameter.rhs match
        case untpd.WildcardTypeBoundsTree() => ()
        case other => fail(s"expected wildcard type bounds, found $other")
      assertEquals(contextualParameter.name.toString, "show")
      assertEquals(contextualParameter.mods.flags, Flags.Param | Flags.Given)
      assert(contextualParameter.unforcedRhs.asInstanceOf[untpd.Tree].isEmpty)
    }
  }

  test("lowers Show[A] structurally at both completed-type sites") {
    withContext {
      val raw = lower(method("provide", "A", "show"))
      val contextualParameter = raw.trailingParamss.head.head.asInstanceOf[untpd.ValDef]

      assertAppliedShowOfA(contextualParameter.tpt)
      raw.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "A")
        case other => fail(s"expected result Ident(A), found $other")
    }
  }

  test("lowers the body as the exact stable contextual reference") {
    withContext {
      val raw = lower(method("provide", "A", "show"))
      raw.rhs match
        case untpd.Ident(name) => assertEquals(name.toString, "show")
        case other => fail(s"expected body Ident(show), found $other")
    }
  }

  test("supports other validated bounded names without widening the shape") {
    withContext {
      val raw = lower(method("summonValue", "Value", "evidence"))
      assertEquals(raw.name.toString, "summonValue")
      assertEquals(raw.leadingTypeParams.map(_.name.toString), List("Value"))
      val parameter = raw.trailingParamss.head.head.asInstanceOf[untpd.ValDef]
      assertEquals(parameter.name.toString, "evidence")
      assertEquals(raw.paramss.map(_.size), List(1, 1))
    }
  }

  test("returns a controlled error for null input") {
    withContext {
      val result = PublicContextualMethodUntypedBackend.lower(null)
      assertEquals(
        result,
        Left(PublicContextualMethodUntypedBackendError.NullDefinitionResult)
      )
    }
  }

  test("constructs source-free span-free symbol-free raw trees only") {
    withContext {
      val raw = lower(method("provide", "A", "show"))
      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists, clues(tree))
        assert(!tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol, clues(tree))
      }
    }
  }

  test("production adapter is structural and has no parser, old backend, placement, or public-backend route") {
    val source = Files.readString(
      Path.of(
        "dotty-internal",
        "src",
        "main",
        "scala",
        "quasiquotes",
        "definitions",
        "dotty",
        "PublicContextualMethodUntypedBackend.scala"
      ),
      StandardCharsets.UTF_8
    )
    Vector(
      "dotty.tools.dotc.parsing",
      "Parser",
      "ConstructedDefinition",
      "ConstructedDefinitionUntypedBackend",
      "CompletedTypeUntypedLowerer",
      "GeneratedOrigin",
      "SourceFile.virtual",
      ".source)",
      ".source,",
      "trait Backend",
      "owner =",
      "newSymbol"
    ).foreach(value => assert(!source.contains(value), clues(value)))
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def method(
      methodName: String,
      typeParameterName: String,
      contextualParameterName: String
  ): DefinitionResultView =
    val namedShow = CompletedType.named("Show").toOption.get
    val binder = CompletedType.typeParameter(typeParameterName).toOption.get
    val showOfBinder = CompletedType.applied(namedShow, Vector(binder)).toOption.get
    val body = CompletedTerm.reference(contextualParameterName).toOption.get
    DefinitionConstruction
      .contextualMethod(
        methodName,
        typeParameterName,
        contextualParameterName,
        showOfBinder,
        binder,
        body
      )
      .toOption
      .get

  private def lower(result: DefinitionResultView)(using Context): untpd.DefDef =
    PublicContextualMethodUntypedBackend.lower(result) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertAppliedShowOfA(tree: untpd.Tree): Unit =
    tree match
      case untpd.AppliedTypeTree(untpd.Ident(constructor), List(untpd.Ident(argument))) =>
        assertEquals(constructor.toString, "Show")
        assertEquals(argument.toString, "A")
      case other => fail(s"expected AppliedTypeTree(Show, A), found $other")

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    val children = tree match
      case value: untpd.DefDef =>
        value.paramss.flatten ++ List(value.tpt, value.rhs)
      case value: untpd.TypeDef => value.rhs :: Nil
      case value: untpd.ValDef => List(value.tpt, value.rhs)
      case value: untpd.AppliedTypeTree => value.tpt :: value.args
      case _ => Nil
    tree :: children.filterNot(_.isEmpty).flatMap(allTrees)
