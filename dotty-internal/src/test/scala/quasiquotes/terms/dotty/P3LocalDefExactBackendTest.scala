package quasiquotes.terms.dotty

import scala.annotation.nowarn

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TermShapeInspector, TinyTermParser, TypeShape, TypeShapeInspector}
import quasiquotes.terms.{ConstructedTerm, TermConstructionError}
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class P3LocalDefExactBackendTest extends munit.FunSuite:
  import ConstructedTermGeneratedOriginError as GeneratedError
  import ConstructedTermUntypedBackendError as RawError
  import TypeNormalForm.*

  test("C009 completes a production LocalDef in parameter result nested preorder") {
    val method = BinderId(7)
    val parameter = BinderId(8)
    val root = localDefBlock(
      method,
      "id",
      parameter,
      "value",
      TypeShape.Identifier("Int"),
      TypeShape.Identifier("String"),
      TermShape.Typed(
        TermShape.BoundReference(parameter, "hostile-body-text"),
        "Boolean"
      ),
      TermShape.BoundReference(method, "hostile-result-text")
    )

    assertEquals(
      ConstructedTerm.fromShape(root).map(_.ascriptionTypes),
      Right(
        Vector(
          STypeIdent("Int"),
          STypeIdent("String"),
          STypeIdent("Boolean")
        )
      )
    )

    val unsupported = localDefBlock(
      method,
      "id",
      parameter,
      "value",
      TypeShape.Identifier("Long"),
      TypeShape.Identifier("Long"),
      TermShape.BoundReference(parameter, "value"),
      TermShape.BoundReference(method, "id")
    )
    assert(
      ConstructedTerm.fromShape(unsupported).left.toOption.exists {
        case TermConstructionError.InvalidTypeTemplateSidecar(0, detail) =>
          detail.contains("Long")
        case _ => false
      }
    )
  }

  test("richer backend emits exact source-free P3 and uses BinderId over hostile text") {
    val method = BinderId(17)
    val parameter = BinderId(18)
    val constructed = ConstructedTerm.fromShape(
      localDefBlock(
        method,
        "id",
        parameter,
        "x",
        TypeShape.Identifier("Int"),
        TypeShape.Identifier("Int"),
        TermShape.BoundReference(parameter, "wrong-body-spelling"),
        TermShape.BoundReference(method, "wrong-result-spelling")
      )
    ).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val expected = TinyTermParser.parseOrThrow("{ def id(x: Int): Int = x; id }")
      assertEquals(TermShapeInspector.rawStructure(raw), expected.rawStructure)
      raw match
        case untpd.Block((definition: untpd.DefDef) :: Nil, result: untpd.Ident) =>
          assertEquals(definition.name.toString, "id")
          assertEquals(definition.mods.flags, Flags.Method)
          assertEquals(definition.leadingTypeParams, Nil)
          assertEquals(definition.paramss.map(_.size), List(1))
          val rawParameter = definition.paramss.head.head.asInstanceOf[untpd.ValDef]
          assertEquals(rawParameter.name.toString, "x")
          assertEquals(rawParameter.mods.flags, Flags.Param)
          assert(rawParameter.rhs.isEmpty)
          assertEquals(TypeShapeInspector.rawStructure(rawParameter.tpt), "Ident(Int)")
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "Ident(Int)")
          assertEquals(TermShapeInspector.rawStructure(definition.rhs), "Ident(x)")
          assertEquals(result.name.toString, "id")
        case other => fail(s"unexpected P3 raw tree: ${other.getClass.getSimpleName}")

      val trees = GeneratedOriginFragmentSupport.allTrees(raw)
      assertEquals(trees.count(_.isInstanceOf[untpd.DefDef]), 1)
      assertEquals(
        trees.collect { case value: untpd.ValDef => value.name.toString },
        Vector("x")
      )
      trees.foreach(assertSourceFree)
    }
  }

  test("P3 sidecars consume parameter result then nested body in strict order") {
    val method = BinderId(20)
    val parameter = BinderId(21)
    val root = localDefBlock(
      method,
      "convert",
      parameter,
      "input",
      TypeShape.Identifier("Int"),
      TypeShape.Identifier("String"),
      TermShape.Typed(
        TermShape.BoundReference(parameter, "ignored"),
        "Boolean"
      ),
      TermShape.BoundReference(method, "ignored")
    )
    val exact = Vector(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
    val constructed = ConstructedTerm.create(root, exact).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      raw match
        case untpd.Block((definition: untpd.DefDef) :: Nil, _) =>
          val rawParameter = definition.paramss.head.head.asInstanceOf[untpd.ValDef]
          assertEquals(TypeShapeInspector.rawStructure(rawParameter.tpt), "Ident(Int)")
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "Ident(String)")
          definition.rhs match
            case untpd.Typed(_, nestedType) =>
              assertEquals(TypeShapeInspector.rawStructure(nestedType), "Ident(Boolean)")
            case other => fail(s"unexpected nested body: $other")
        case other => fail(s"unexpected sidecar tree: $other")

      val missing = corrupt(root, exact.take(1))
      val extra = corrupt(root, exact :+ STypeIdent("Int"))
      val swapped = corrupt(root, Vector(STypeIdent("String"), STypeIdent("Int"), STypeIdent("Boolean")))
      val unsupportedType = STypeIdent("UnsupportedType")
      val unsupported = corrupt(root, Vector(unsupportedType, STypeIdent("String"), STypeIdent("Boolean")))
      assertEquals(
        ConstructedTermUntypedBackend.lower(missing),
        Left(RawError.MissingTypeSidecar(1))
      )
      assertEquals(
        ConstructedTermUntypedBackend.lower(extra),
        Left(RawError.UnconsumedTypeSidecars(3, 4))
      )
      ConstructedTermUntypedBackend.lower(swapped).toOption.get match
        case untpd.Block((definition: untpd.DefDef) :: Nil, _) =>
          val parameter = definition.paramss.head.head.asInstanceOf[untpd.ValDef]
          assertEquals(TypeShapeInspector.rawStructure(parameter.tpt), "Ident(String)")
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "Ident(Int)")
        case other => fail(s"unexpected swapped-sidecar tree: $other")
      assert(ConstructedTerm.create(
        root,
        Vector(STypeIdent("String"), STypeIdent("Int"), STypeIdent("Boolean"))
      ).isLeft)
      assertEquals(
        ConstructedTermUntypedBackend.lower(unsupported),
        Left(RawError.UnsupportedTypeSidecar(0, unsupportedType.render))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(missing, "<u012-missing>"),
        Left(GeneratedError.MissingTypeSidecar(1))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(extra, "<u012-extra>"),
        Left(GeneratedError.UnconsumedTypeSidecars(3, 4))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(unsupported, "<u012-unsupported>"),
        Left(GeneratedError.UnsupportedTypeSidecar(0, unsupportedType.render))
      )
    }
  }

  test("method parameter and enclosing binder scopes follow the exact lifecycle") {
    val outer = BinderId(30)
    val method = BinderId(31)
    val parameter = BinderId(32)
    val root = localDefBlock(
      method,
      "same",
      parameter,
      "same",
      TypeShape.Identifier("Int"),
      TypeShape.Identifier("Int"),
      TermShape.Tuple(
        List(
          TermShape.BoundReference(parameter, "method-looking-text"),
          TermShape.BoundReference(outer, "parameter-looking-text")
        )
      ),
      TermShape.Tuple(
        List(
          TermShape.BoundReference(method, "parameter-looking-text"),
          TermShape.BoundReference(outer, "method-looking-text")
        )
      )
    )
    val constructed = ConstructedTerm.fromShapeInScope(root, outer).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend
        .lowerInScope(constructed, outer, "outer")
        .toOption.get
      raw match
        case untpd.Block((definition: untpd.DefDef) :: Nil, untpd.Tuple(methodUse :: outerResult :: Nil)) =>
          definition.rhs match
            case untpd.Tuple(parameterUse :: outerBody :: Nil) =>
              assertIdent(parameterUse, "same")
              assertIdent(outerBody, "outer")
            case other => fail(s"unexpected body: $other")
          assertIdent(methodUse, "same")
          assertIdent(outerResult, "outer")
        case other => fail(s"unexpected scoped P3: $other")

      val methodInBody = corrupt(
        replaceBody(root, TermShape.BoundReference(method, "same")),
        constructed.ascriptionTypes
      )
      val parameterInResult = corrupt(
        replaceResult(root, TermShape.BoundReference(parameter, "same")),
        constructed.ascriptionTypes
      )
      assertEquals(
        ConstructedTermUntypedBackend.lowerInScope(methodInBody, outer, "outer"),
        Left(RawError.OutOfScopeBoundReference(method.value))
      )
      assertEquals(
        ConstructedTermUntypedBackend.lowerInScope(parameterInResult, outer, "outer"),
        Left(RawError.OutOfScopeBoundReference(parameter.value))
      )
      assertEquals(
        GeneratedOriginFragmentSupport.planDefinitionBodyInScope(methodInBody, outer, "outer"),
        Left(GeneratedError.OutOfScopeBoundReference(method.value))
      )
      assertEquals(
        GeneratedOriginFragmentSupport.planDefinitionBodyInScope(parameterInResult, outer, "outer"),
        Left(GeneratedError.OutOfScopeBoundReference(parameter.value))
      )

      val leakingMethod = corrupt(
        TermShape.Block(List(root), TermShape.BoundReference(method, "same")),
        constructed.ascriptionTypes
      )
      assertEquals(
        ConstructedTermUntypedBackend.lowerInScope(leakingMethod, outer, "outer"),
        Left(RawError.OutOfScopeBoundReference(method.value))
      )
      assertEquals(
        GeneratedOriginFragmentSupport.planDefinitionBodyInScope(leakingMethod, outer, "outer"),
        Left(GeneratedError.OutOfScopeBoundReference(method.value))
      )
      assert(ConstructedTermUntypedBackend.lowerInScope(constructed, outer, "outer").isRight)
      assert(ConstructedTermUntypedBackend.lowerInScope(constructed, outer, "outer").isRight)

      val colliding = ConstructedTerm.fromShapeInScope(root, outer).toOption.get
      val collisionRaw = ConstructedTermUntypedBackend
        .lowerInScope(colliding, outer, "same")
        .toOption.get
      collisionRaw match
        case untpd.Block((definition: untpd.DefDef) :: Nil, untpd.Tuple(methodUse :: outerResult :: Nil)) =>
          assertEquals(definition.name.toString, "same_1")
          val rawParameter = definition.paramss.head.head.asInstanceOf[untpd.ValDef]
          assertEquals(rawParameter.name.toString, "same_1")
          definition.rhs match
            case untpd.Tuple(parameterUse :: outerBody :: Nil) =>
              assertIdent(parameterUse, "same_1")
              assertIdent(outerBody, "same")
            case other => fail(s"unexpected collision body: $other")
          assertIdent(methodUse, "same_1")
          assertIdent(outerResult, "same")
        case other => fail(s"unexpected collision P3: $other")
      assertEquals(
        GeneratedOriginFragmentSupport
          .planDefinitionBodyInScope(colliding, outer, "same")
          .map(_.source),
        Right(
          "{ def same_1(same_1: Int): Int = (same_1, same); (same_1, same) }"
        )
      )
    }
  }

  test("generated-origin P3 is deterministic parser-equivalent and fully positioned") {
    val constructed = fixture("renamed", "argument", "Int")
    withContext {
      val sourceFree = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val generated = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u012-generated-p3>")
        .toOption.get
      assertEquals(
        generated.generatedSource,
        "{ def renamed(argument: Int): Int = argument; renamed }"
      )
      assertEquals(
        TermShapeInspector.rawStructure(generated.tree),
        TinyTermParser.parseOrThrow(generated.generatedSource).rawStructure
      )
      assertEquals(
        TermShapeInspector.rawStructure(generated.tree),
        TermShapeInspector.rawStructure(sourceFree)
      )
      val trees = GeneratedOriginFragmentSupport.allTrees(generated.tree)
      assertEquals(trees.count(_.isInstanceOf[untpd.DefDef]), 1)
      assertEquals(trees.count(_.isInstanceOf[untpd.ValDef]), 1)
      trees.foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, generated.virtualSourceName)
        assert(tree.span.exists)
        assert(tree.span.start >= 0)
        assert(tree.span.end <= generated.generatedSource.length)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }

      val sourceFreeParameterType = sourceFree match
        case untpd.Block((method: untpd.DefDef) :: Nil, _) =>
          method.paramss.head.head.asInstanceOf[untpd.ValDef].tpt
        case other => fail(s"unexpected source-free P3: $other")
      val missingParameterTypeOrigin = generated.tree match
        case block @ untpd.Block((method: untpd.DefDef) :: Nil, result) =>
          val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
          val brokenParameter = untpd.cpy.ValDef(parameter)(
            parameter.name,
            sourceFreeParameterType,
            parameter.rhs
          )
          val brokenMethod = untpd.cpy.DefDef(method)(
            method.name,
            List(List(brokenParameter)),
            method.tpt,
            method.rhs
          )
          untpd.cpy.Block(block)(List(brokenMethod), result)
        case other => fail(s"unexpected generated P3: $other")
      val expectedSource = SourceFile.virtual(
        generated.virtualSourceName,
        generated.generatedSource
      )
      assert(
        GeneratedOriginFragmentSupport
          .validatePositionedTree(
            missingParameterTypeOrigin,
            expectedSource,
            0,
            generated.generatedSource.length
          )
          .left.toOption
          .exists(_.isInstanceOf[GeneratedError.IncompletePositionMap])
      )
    }
  }

  test("N007 Scalameta fixtures compose through both richer routes while direct P3 stays closed") {
    Vector(
      "{ def id(x: Int): Int = x; id }",
      "{ def id(x: String): String = x; id }",
      "{ def id(x: Boolean): Boolean = x; id }",
      "{ def renamed(argument: Int): Int = argument; renamed }"
    ).foreach { source =>
      val projected = ScalametaTermProjection
        .project(Input.String(source).parse[Term].get)
        .toOption.get.shape
      val constructed = ConstructedTerm.fromShape(projected).toOption.get
      withContext {
        assert(CoreTermShapeUntypedLowerer.lower(projected).isLeft)
        val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        val generated = ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<u012-n007-composition>").toOption.get
        assertEquals(
          TermShapeInspector.rawStructure(raw),
          TinyTermParser.parseOrThrow(source).rawStructure
        )
        assertEquals(
          TermShapeInspector.rawStructure(generated.tree),
          TinyTermParser.parseOrThrow(source).rawStructure
        )
      }
    }
  }

  private def fixture(methodName: String, parameterName: String, tpe: String): ConstructedTerm =
    val method = BinderId(40)
    val parameter = BinderId(41)
    ConstructedTerm.fromShape(
      localDefBlock(
        method,
        methodName,
        parameter,
        parameterName,
        TypeShape.Identifier(tpe),
        TypeShape.Identifier(tpe),
        TermShape.BoundReference(parameter, "ignored-body"),
        TermShape.BoundReference(method, "ignored-result")
      )
    ).toOption.get

  private def localDefBlock(
      method: BinderId,
      methodName: String,
      parameter: BinderId,
      parameterName: String,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape,
      result: TermShape
  ): TermShape =
    TermShape.Block(
      List(
        BlockStatement.LocalDef(
          method,
          methodName,
          parameter,
          parameterName,
          parameterType,
          resultType,
          body
        )
      ),
      result
    )

  private def replaceBody(root: TermShape, body: TermShape): TermShape =
    root match
      case TermShape.Block(List(local: BlockStatement.LocalDef), result) =>
        TermShape.Block(List(local.copy(body = body)), result)
      case other => fail(other.render)

  private def replaceResult(root: TermShape, result: TermShape): TermShape =
    root match
      case TermShape.Block(statements, _) => TermShape.Block(statements, result)
      case other => fail(other.render)

  private def assertIdent(tree: untpd.Tree, expected: String): Unit =
    tree match
      case value: untpd.Ident => assertEquals(value.name.toString, expected)
      case other => fail(s"expected Ident($expected), found $other")

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    assert(!tree.source.exists)
    assert(!tree.span.exists)
    assertEquals(tree.symbol, NoSymbol)
    assert(!tree.isInstanceOf[untpd.TypedSplice])

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(root, sidecars).asInstanceOf[ConstructedTerm]
