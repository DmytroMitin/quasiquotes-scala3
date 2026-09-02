package quasiquotes.terms.dotty

import scala.annotation.nowarn

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TermShapeInspector, TinyTermParser, TypeShape, TypeShapeInspector}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class P2LocalValExactBackendTest extends munit.FunSuite:
  import ConstructedTermGeneratedOriginError as GeneratedError
  import ConstructedTermUntypedBackendError as RawError
  import TypeNormalForm.*

  test("richer backend lowers canonical P2 exactly and trusts BinderId over hostile reference text") {
    val binder = BinderId(17)
    val root = localBlock(
      binder,
      "x",
      "Int",
      TermShape.Literal("1"),
      TermShape.BoundReference(binder, "hostileAndUnrelated")
    )
    val constructed = ConstructedTerm.fromShape(root).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      assertEquals(
        TermShapeInspector.rawStructure(raw),
        TinyTermParser.parseOrThrow("{ val x: Int = 1; x }").rawStructure
      )
      raw match
        case untpd.Block((definition: untpd.ValDef) :: Nil, result: untpd.Ident) =>
          assertEquals(definition.name.toString, "x")
          assertEquals(definition.mods.flags, Flags.EmptyFlags)
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "Ident(Int)")
          assertEquals(TermShapeInspector.rawStructure(definition.rhs), "Number(1,Whole(10))")
          assertEquals(result.name.toString, "x")
        case other => fail(s"unexpected P2 raw tree: ${other.getClass.getSimpleName}")
      GeneratedOriginFragmentSupport.allTrees(raw).foreach(assertSourceFree)
    }
  }

  test("declared type precedes initializer later-prefix and result sidecars") {
    val local = BinderId(20)
    val lambda = BinderId(21)
    val listInt = STypeApply(STypeIdent("List"), List(STypeIdent("Int")))
    val root = localBlock(
      local,
      "values",
      "List[Int]",
      TermShape.Typed(ident("seed"), "String"),
      TermShape.Block(
        List(TermShape.Typed(ident("ready"), "Boolean")),
        TermShape.Lambda1(
          lambda,
          "index",
          "Int",
          TermShape.BoundReference(local, "ignored")
        )
      )
    )
    val constructed = ConstructedTerm.create(
      root,
      Vector(listInt, STypeIdent("String"), STypeIdent("Boolean"), STypeIdent("Int"))
    ).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      raw match
        case untpd.Block(
              (definition: untpd.ValDef) :: Nil,
              untpd.Block(
                untpd.Typed(_, prefixType) :: Nil,
                untpd.Function((parameter: untpd.ValDef) :: Nil, _)
              )
            ) =>
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "AppliedTypeTree(Ident(List), [Ident(Int)])")
          definition.rhs match
            case untpd.Typed(_, initializerType) =>
              assertEquals(TypeShapeInspector.rawStructure(initializerType), "Ident(String)")
            case other => fail(s"unexpected initializer: ${other.getClass.getSimpleName}")
          assertEquals(TypeShapeInspector.rawStructure(prefixType), "Ident(Boolean)")
          assertEquals(TypeShapeInspector.rawStructure(parameter.tpt), "Ident(Int)")
        case other => fail(s"unexpected sidecar P2 tree: ${other.getClass.getSimpleName}")

      val generated = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u007-generated-sidecar-order>")
        .toOption
        .get
      assertEquals(
        generated.generatedSource,
        "{ val values: List[Int] = (seed): String; { ready: Boolean; (index: Int) => values } }"
      )
      generated.tree match
        case untpd.Block(
              (definition: untpd.ValDef) :: Nil,
              untpd.Block(
                untpd.Typed(_, prefixType) :: Nil,
                untpd.Function((parameter: untpd.ValDef) :: Nil, _)
              )
            ) =>
          assertEquals(TypeShapeInspector.rawStructure(definition.tpt), "AppliedTypeTree(Ident(List), [Ident(Int)])")
          definition.rhs match
            case untpd.Typed(_, initializerType) =>
              assertEquals(TypeShapeInspector.rawStructure(initializerType), "Ident(String)")
            case other => fail(s"unexpected generated initializer: ${other.getClass.getSimpleName}")
          assertEquals(TypeShapeInspector.rawStructure(prefixType), "Ident(Boolean)")
          assertEquals(TypeShapeInspector.rawStructure(parameter.tpt), "Ident(Int)")
        case other => fail(s"unexpected generated sidecar P2 tree: ${other.getClass.getSimpleName}")
    }
  }

  test("initializer uses old scope later children use new scope and block exit restores it") {
    val outer = BinderId(30)
    val local = BinderId(31)
    val root = localBlock(
      local,
      "x",
      "Int",
      TermShape.BoundReference(outer, "outer-hostile"),
      TermShape.Block(
        List(TermShape.BoundReference(local, "local-hostile")),
        TermShape.BoundReference(outer, "outer-hostile-again")
      )
    )
    val constructed = ConstructedTerm.fromShapeInScope(root, outer).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend
        .lowerInScope(constructed, outer, "outer")
        .toOption
        .get
      raw match
        case untpd.Block((definition: untpd.ValDef) :: Nil, untpd.Block(localUse :: Nil, outerUse)) =>
          assertEquals(TermShapeInspector.rawStructure(definition.rhs), "Ident(outer)")
          assertEquals(TermShapeInspector.rawStructure(localUse), "Ident(x)")
          assertEquals(TermShapeInspector.rawStructure(outerUse), "Ident(outer)")
        case other => fail(s"unexpected scoped P2 tree: ${other.getClass.getSimpleName}")

      val leaking = corrupt(
        TermShape.Block(List(root), TermShape.BoundReference(local, "x")),
        constructed.ascriptionTypes
      )
      assertEquals(
        ConstructedTermUntypedBackend.lowerInScope(leaking, outer, "outer"),
        Left(RawError.OutOfScopeBoundReference(local.value))
      )
      assertEquals(
        GeneratedOriginFragmentSupport.planDefinitionBodyInScope(
          leaking,
          outer,
          "outer"
        ),
        Left(GeneratedError.OutOfScopeBoundReference(local.value))
      )
    }
  }

  test("same-text free and initializer identifiers remain semantic non-bindings") {
    val local = BinderId(35)
    val root = localBlock(
      local,
      "x",
      "Int",
      ident("x"),
      TermShape.Tuple(
        List(
          TermShape.BoundReference(local, "hostile"),
          ident("x")
        )
      )
    )
    val constructed = ConstructedTerm.fromShape(root).toOption.get

    constructed.root match
      case TermShape.Block(
            List(BlockStatement.LocalVal(_, _, _, initializer: TermShape.Identifier)),
            TermShape.Tuple(
              List(_: TermShape.BoundReference, freeResult: TermShape.Identifier)
            )
          ) =>
        assertEquals(initializer.name, "x")
        assertEquals(freeResult.name, "x")
      case other => fail(s"unexpected same-text semantic shape: ${other.render}")

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      raw match
        case untpd.Block((definition: untpd.ValDef) :: Nil, untpd.Tuple(boundUse :: freeUse :: Nil)) =>
          assertEquals(TermShapeInspector.rawStructure(definition.rhs), "Ident(x)")
          assertEquals(TermShapeInspector.rawStructure(boundUse), "Ident(x)")
          assertEquals(TermShapeInspector.rawStructure(freeUse), "Ident(x)")
        case other => fail(s"unexpected same-text raw tree: ${other.getClass.getSimpleName}")
    }
  }

  test("distinct-name Lambda1 and P2 compose in either admitted nesting direction") {
    val lambda = BinderId(40)
    val local = BinderId(41)
    val lambdaThenP2 = TermShape.Lambda1(
      lambda,
      "outer",
      "Int",
      localBlock(
        local,
        "x",
        "Int",
        TermShape.BoundReference(lambda, "ignored"),
        TermShape.BoundReference(local, "ignored")
      )
    )
    val p2ThenLambda = localBlock(
      local,
      "x",
      "Int",
      TermShape.Literal("1"),
      TermShape.Lambda1(
        lambda,
        "inner",
        "Int",
        TermShape.Infix(
          TermShape.BoundReference(local, "ignored"),
          "+",
          TermShape.BoundReference(lambda, "ignored")
        )
      )
    )

    withContext {
      Vector(lambdaThenP2, p2ThenLambda).foreach { shape =>
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        assert(ConstructedTermUntypedBackend.lower(constructed).isRight, clues(shape.render))
        assert(
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, "<u007-distinct-binders>")
            .isRight,
          clues(shape.render)
        )
      }
    }
  }

  test("ambient definition-body binders do not widen the established Lambda1 boundary") {
    val ambient = BinderId(45)
    val lambda = BinderId(46)
    val root = TermShape.Lambda1(
      lambda,
      "inner",
      "Int",
      TermShape.Infix(
        TermShape.BoundReference(ambient, "ambient"),
        "+",
        TermShape.BoundReference(lambda, "inner")
      )
    )
    val constructed = ConstructedTerm.createInScope(
      root,
      Vector(STypeIdent("Int")),
      ambient
    ).toOption.get

    assertEquals(
      ConstructedTermUntypedBackend.lowerInScope(constructed, ambient, "outer"),
      Left(RawError.NestedLambda1Unsupported)
    )
    assertEquals(
      GeneratedOriginFragmentSupport.planDefinitionBodyInScope(
        constructed,
        ambient,
        "outer"
      ),
      Left(GeneratedError.NestedLambda1Unsupported)
    )
  }

  test("generated-origin P2 is deterministic parser-equivalent and completely positioned") {
    val binder = BinderId(50)
    val constructed = ConstructedTerm.fromShape(
      localBlock(
        binder,
        "x",
        "Int",
        TermShape.Literal("1"),
        TermShape.BoundReference(binder, "doNotRenderThis")
      )
    ).toOption.get

    withContext {
      val generated = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u007-generated-p2>")
        .toOption
        .get
      assertEquals(generated.generatedSource, "{ val x: Int = 1; x }")
      assertEquals(
        TermShapeInspector.rawStructure(generated.tree),
        TinyTermParser.parseOrThrow(generated.generatedSource).rawStructure
      )
      generated.tree match
        case block @ untpd.Block((definition: untpd.ValDef) :: Nil, result: untpd.Ident) =>
          assertEquals((block.span.start, block.span.point, block.span.end), (0, 0, 21))
          assertEquals(
            (definition.span.start, definition.span.point, definition.span.end),
            (2, 6, 16)
          )
          assertEquals(
            (definition.tpt.span.start, definition.tpt.span.point, definition.tpt.span.end),
            (9, 9, 12)
          )
          assertEquals(
            (definition.rhs.span.start, definition.rhs.span.point, definition.rhs.span.end),
            (15, 15, 16)
          )
          assertEquals((result.span.start, result.span.point, result.span.end), (18, 18, 19))
        case other => fail(s"unexpected positioned P2 tree: ${other.getClass.getSimpleName}")
      GeneratedOriginFragmentSupport.allTrees(generated.tree).foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, generated.virtualSourceName)
        assert(tree.span.exists)
        assert(tree.span.start >= 0)
        assert(tree.span.end <= generated.generatedSource.length)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("N006 Scalameta P2 projection reaches both exact richer routes while direct stays closed") {
    val projected = ScalametaTermProjection
      .project(Input.String("{ val x: Int = 1; x }").parse[Term].get)
      .toOption
      .get
      .shape
    val constructed = ConstructedTerm.fromShape(projected).toOption.get

    withContext {
      assert(CoreTermShapeUntypedLowerer.lower(projected).isLeft)
      assert(ConstructedTermUntypedBackend.lower(constructed).isRight)
      assert(
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<u007-n006-composition>")
          .isRight
      )
    }
  }

  test("P2 failures remain controlled and P3 LocalDef remains rejected") {
    val binder = BinderId(60)
    val p2 = localBlock(
      binder,
      "x",
      "Int",
      TermShape.Literal("1"),
      TermShape.BoundReference(binder, "x")
    )
    val unsupportedType = STypeIdent("UnsupportedType")
    val missing = corrupt(p2, Vector.empty)
    val extra = corrupt(p2, Vector(STypeIdent("Int"), STypeIdent("String")))
    val unsupported = corrupt(p2, Vector(unsupportedType))
    val selfReference = corrupt(
      localBlock(
        binder,
        "x",
        "Int",
        TermShape.BoundReference(binder, "x"),
        TermShape.BoundReference(binder, "x")
      ),
      Vector(STypeIdent("Int"))
    )
    val malformedName = corrupt(
      localBlock(binder, "bad.name", "Int", TermShape.Literal("1"), TermShape.BoundReference(binder, "x")),
      Vector(STypeIdent("Int"))
    )
    val localDef = corrupt(
      TermShape.Block(
        List(
          BlockStatement.LocalDef(
            BinderId(61),
            "f",
            BinderId(62),
            "x",
            TypeShape.Identifier("Int"),
            TypeShape.Identifier("Int"),
            TermShape.BoundReference(BinderId(62), "x")
          )
        ),
        TermShape.BoundReference(BinderId(61), "f")
      ),
      Vector(STypeIdent("Int"), STypeIdent("Int"))
    )

    assertEquals(ConstructedTermUntypedBackend.lower(missing), Left(RawError.MissingTypeSidecar(0)))
    assertEquals(ConstructedTermUntypedBackend.lower(extra), Left(RawError.UnconsumedTypeSidecars(1, 2)))
    assertEquals(
      ConstructedTermUntypedBackend.lower(unsupported),
      Left(RawError.UnsupportedTypeSidecar(0, unsupportedType.render))
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(selfReference),
      Left(RawError.OutOfScopeBoundReference(binder.value))
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(malformedName),
      Left(RawError.UnsupportedTermNode("LocalValName"))
    )
    assert(ConstructedTermUntypedBackend.lower(localDef).isLeft)

    withContext {
      val directLocalDef = CoreTermShapeUntypedLowerer.lower(localDef.root)
      assert(directLocalDef.isLeft)
      assert(directLocalDef.left.toOption.get.message.contains("LocalDef"))
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(missing, "<u007-missing>"),
        Left(GeneratedError.MissingTypeSidecar(0))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(extra, "<u007-extra>"),
        Left(GeneratedError.UnconsumedTypeSidecars(1, 2))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(unsupported, "<u007-unsupported>"),
        Left(GeneratedError.UnsupportedTypeSidecar(0, unsupportedType.render))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(selfReference, "<u007-self-reference>"),
        Left(GeneratedError.OutOfScopeBoundReference(binder.value))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(malformedName, "<u007-malformed-name>"),
        Left(GeneratedError.UnrenderableName("local val", "bad.name"))
      )
      assert(ConstructedTermGeneratedOriginAdapter.lower(localDef, "<u007-p3>").isLeft)

      val valid = ConstructedTerm.fromShape(p2).toOption.get
      val fragment = GeneratedOriginFragmentSupport.planTerm(valid).toOption.get
      val mismatchedRaw = ConstructedTermUntypedBackend
        .lower(ConstructedTerm.fromShape(TermShape.Literal("1")).toOption.get)
        .toOption
        .get
      val source = SourceFile.virtual("<u007-plan-mismatch>", fragment.source)
      assert(
        GeneratedOriginFragmentSupport
          .positionTerm(mismatchedRaw, fragment, source, baseOffset = 0)
          .left
          .toOption
          .exists(_.isInstanceOf[GeneratedError.RawTreePlanMismatch])
      )
    }
  }

  private def localBlock(
      binder: BinderId,
      name: String,
      declaredType: String,
      initializer: TermShape,
      result: TermShape
  ): TermShape =
    TermShape.Block(
      List(BlockStatement.LocalVal(binder, name, declaredType, initializer)),
      result
    )

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    assert(!tree.source.exists)
    assert(!tree.span.exists)
    assertEquals(tree.symbol, NoSymbol)
    assert(!tree.isInstanceOf[untpd.TypedSplice])

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(root, sidecars).asInstanceOf[ConstructedTerm]
