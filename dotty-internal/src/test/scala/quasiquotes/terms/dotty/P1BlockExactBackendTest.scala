package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TermShapeInspector, TinyTermParser, TypeShapeInspector}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class P1BlockExactBackendTest extends munit.FunSuite:
  import TypeNormalForm.*

  test("direct lowerer preserves ordered prefix result and nested P1 topology") {
    val shape =
      block(
        List(
          TermShape.Literal("1"),
          block(List(TermShape.Literal("2")), TermShape.Literal("3"))
        ),
        TermShape.Literal("4")
      )

    withContext {
      val raw = CoreTermShapeUntypedLowerer.lower(shape).toOption.get
      assertEquals(
        TermShapeInspector.rawStructure(raw),
        TinyTermParser.parseOrThrow("{ 1; { 2; 3 }; 4 }").rawStructure
      )
      assertEquals(TermShapeInspector.inspect(raw), shape)
    }
  }

  test("direct lowerer recursively preserves the source-free invariant across Block edges") {
    val shape =
      block(
        List(
          TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("true"))),
          TermShape.If(ident("flag"), TermShape.Literal("2"), TermShape.Literal("3"))
        ),
        TermShape.Apply(ident("f"), List(TermShape.Literal("4")))
      )

    withContext {
      val raw = CoreTermShapeUntypedLowerer.lower(shape).toOption.get
      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("direct lowerer rejects malformed and statement-bearing Blocks without throwing") {
    val local = BlockStatement.LocalVal(BinderId(0), "x", "Int", TermShape.Literal("1"))
    val cases = Vector(
      TermShape.Block(List(null), TermShape.Literal("1")) -> "prefix entry 0 is null",
      TermShape.Block(List(TermShape.Literal("1")), null) -> "missing core TermShape",
      TermShape.Block(List(local), TermShape.Literal("1")) -> "prefix entry 0 is LocalVal",
      TermShape.Block(
        List(
          TermShape.Typed(TermShape.Literal("1"), "Int")
        ),
        TermShape.Literal("1")
      ) ->
        "Unsupported core TermShape"
    )

    withContext {
      cases.foreach { (shape, expected) =>
        val result = CoreTermShapeUntypedLowerer.lower(shape)
        assert(result.isLeft, clues(shape))
        assert(result.left.toOption.get.message.contains(expected), clues(result))
      }
    }
  }

  test("Core construction makes missing and empty Block prefixes impossible before backend entry") {
    intercept[NullPointerException] {
      TermShape.Block(null, TermShape.Literal("1"))
    }
    val error = intercept[IllegalArgumentException] {
      TermShape.Block(Nil, TermShape.Literal("1"))
    }
    assert(error.getMessage.contains("block must contain at least one statement"))
  }

  test("richer backend lowers P1 left to right and consumes typed sidecars in prefix-result preorder") {
    val root =
      block(
        List(
          TermShape.Typed(ident("first"), "Int"),
          TermShape.Typed(ident("second"), "String")
        ),
        TermShape.Typed(ident("result"), "Boolean")
      )
    val constructed = ConstructedTerm.create(
      root,
      Vector(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
    ).toOption.get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      raw match
        case untpd.Block(
              untpd.Typed(_, firstType) :: untpd.Typed(_, secondType) :: Nil,
              untpd.Typed(_, resultType)
            ) =>
          assertEquals(TypeShapeInspector.rawStructure(firstType), "Ident(Int)")
          assertEquals(TypeShapeInspector.rawStructure(secondType), "Ident(String)")
          assertEquals(TypeShapeInspector.rawStructure(resultType), "Ident(Boolean)")
        case other => fail(s"unexpected P1 raw tree: ${other.getClass.getSimpleName}")
      allTrees(raw).foreach(assertSourceFree)
    }
  }

  test("richer backend composes a Lambda1 prefix only through its authoritative type sidecar") {
    val binder = BinderId(41)
    val root =
      block(
        List(
          TermShape.Lambda1(
            binder,
            "x",
            "descriptive.NotAuthoritative",
            TermShape.BoundReference(binder, "ignored-display")
          )
        ),
        TermShape.Literal("1")
      )
    val constructed = corrupt(root, Vector(STypeIdent("Int")))

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      raw match
        case untpd.Block(
              untpd.Function((parameter: untpd.ValDef) :: Nil, body) :: Nil,
              result
            ) =>
          assertEquals(TypeShapeInspector.rawStructure(parameter.tpt), "Ident(Int)")
          assertEquals(TermShapeInspector.rawStructure(body), "Ident(x)")
          assertEquals(TermShapeInspector.rawStructure(result), "Number(1,Whole(10))")
        case other => fail(s"unexpected Lambda1 P1 tree: ${other.getClass.getSimpleName}")
      allTrees(raw).foreach(assertSourceFree)
    }
  }

  test("richer and generated-origin paths fail closed for corrupted P1 structure and sidecars") {
    val malformed = Vector(
      corrupt(TermShape.Block(List(null), TermShape.Literal("1")), Vector.empty),
      corrupt(TermShape.Block(List(TermShape.Literal("1")), null), Vector.empty)
    )
    val missing = corrupt(
      block(List(TermShape.Typed(ident("first"), "Int")), TermShape.Literal("1")),
      Vector.empty
    )
    val extra = corrupt(
      block(List(TermShape.Literal("1")), TermShape.Literal("2")),
      Vector(STypeIdent("Int"))
    )
    val outerBinder = BinderId(51)
    val innerBinder = BinderId(52)
    val nestedLambda = corrupt(
      block(
        List(
          TermShape.Lambda1(
            outerBinder,
            "outer",
            "Int",
            TermShape.Lambda1(
              innerBinder,
              "inner",
              "String",
              TermShape.BoundReference(innerBinder, "inner")
            )
          )
        ),
        TermShape.Literal("1")
      ),
      Vector(STypeIdent("Int"), STypeIdent("String"))
    )

    withContext {
      assert(ConstructedTermUntypedBackend.lower(null).isLeft)
      assert(
        ConstructedTermGeneratedOriginAdapter
          .lower(null, "<u006-missing-constructed-term>")
          .isLeft
      )
      (malformed :+ missing :+ extra :+ nestedLambda).foreach { value =>
        assert(ConstructedTermUntypedBackend.lower(value).isLeft)
        assert(
          ConstructedTermGeneratedOriginAdapter
            .lower(value, "<u006-malformed-block>")
            .isLeft
        )
      }
    }
  }

  test("generated-origin block source is deterministic ordered and completely positioned") {
    val constructed = ConstructedTerm.fromShape(
      block(
        List(
          TermShape.Literal("1"),
          block(List(TermShape.Literal("2")), TermShape.Literal("3"))
        ),
        TermShape.Literal("4")
      )
    ).toOption.get

    withContext {
      val result = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u006-generated-block>")
        .toOption
        .get
      assertEquals(result.generatedSource, "{ 1; { 2; 3 }; 4 }")
      assertEquals(
        TermShapeInspector.rawStructure(result.tree),
        TinyTermParser.parseOrThrow(result.generatedSource).rawStructure
      )
      GeneratedOriginFragmentSupport.allTrees(result.tree).foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, result.virtualSourceName)
        assert(tree.span.exists)
        assert(tree.span.start >= 0)
        assert(tree.span.end <= result.generatedSource.length)
        assertEquals(tree.symbol, NoSymbol)
      }

      val selected = ConstructedTerm.fromShape(
        TermShape.Select(
          block(List(TermShape.Literal("1")), TermShape.Literal("2")),
          "toString"
        )
      ).toOption.get
      val selectedResult = ConstructedTermGeneratedOriginAdapter
        .lower(selected, "<u006-generated-grouped-block>")
        .toOption
        .get
      assertEquals(selectedResult.generatedSource, "({ 1; 2 }).toString")
      assertEquals(
        TermShapeInspector.rawStructure(selectedResult.tree),
        TinyTermParser.parseOrThrow(selectedResult.generatedSource).rawStructure
      )

      val typed = ConstructedTerm.create(
        TermShape.Typed(
          block(List(TermShape.Literal("1")), TermShape.Literal("2")),
          "Int"
        ),
        Vector(STypeIdent("Int"))
      ).toOption.get
      val typedResult = ConstructedTermGeneratedOriginAdapter
        .lower(typed, "<u006-generated-typed-block>")
        .toOption
        .get
      assertEquals(typedResult.generatedSource, "({ 1; 2 }): Int")
      assertEquals(
        TermShapeInspector.rawStructure(typedResult.tree),
        TinyTermParser.parseOrThrow(typedResult.generatedSource).rawStructure
      )
    }
  }

  test("generated-origin P1 preserves Lambda1 and Typed sidecar order") {
    val binder = BinderId(7)
    val root =
      block(
        List(
          TermShape.Lambda1(
            binder,
            "value",
            "wrong.Display",
            TermShape.BoundReference(binder, "value")
          ),
          TermShape.Typed(ident("text"), "String")
        ),
        TermShape.Typed(ident("flag"), "Boolean")
      )
    val constructed = corrupt(
      root,
      Vector(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
    )

    withContext {
      val result = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u006-generated-sidecars>")
        .toOption
        .get
      assertEquals(
        result.generatedSource,
        "{ ((value: Int) => value); text: String; flag: Boolean }"
      )
      assertEquals(
        TermShapeInspector.rawStructure(result.tree),
        TinyTermParser.parseOrThrow(result.generatedSource).rawStructure
      )
      GeneratedOriginFragmentSupport.allTrees(result.tree).foreach { tree =>
        assert(tree.source.exists)
        assertEquals(tree.source.path, result.virtualSourceName)
        assert(tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("richer P1 composes New interpolation and Parenthesized children in source-free and generated-origin modes") {
    val root = block(
      List(
        TermShape.New("java.lang.StringBuilder", Nil),
        TermShape.InterpolatedString("s", List("prefix"), Nil)
      ),
      TermShape.Parenthesized(ident("result"))
    )
    val constructed = ConstructedTerm.fromShape(root).toOption.get
    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      allTrees(raw).foreach(assertSourceFree)
      val generated = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<u006-richer-only-block-children>")
        .toOption
        .get
      assertEquals(
        generated.generatedSource,
        "{ new java.lang.StringBuilder(); s\"prefix\"; (result) }"
      )
      assertEquals(
        TermShapeInspector.rawStructure(generated.tree),
        TinyTermParser.parseOrThrow(generated.generatedSource).rawStructure
      )
    }
  }

  test("direct and richer backends agree on the bounded no-sidecar overlap") {
    val shapes = Vector(
      block(List(TermShape.Literal("1")), TermShape.Literal("2")),
      block(List(TermShape.Literal("\"prefix\""), TermShape.Literal("true")), TermShape.Literal("0")),
      block(List(TermShape.Select(ident("service"), "first")), TermShape.Apply(ident("result"), Nil)),
      block(List(TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2"))), TermShape.Unary("-", TermShape.Literal("3"))),
      block(List(TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2")))), TermShape.If(ident("flag"), TermShape.Literal("3"), TermShape.Literal("4"))),
      block(List(block(List(TermShape.Literal("1")), TermShape.Literal("2"))), TermShape.Literal("3"))
    )

    withContext {
      shapes.foreach { shape =>
        val direct = CoreTermShapeUntypedLowerer.lower(shape).toOption.get
        val richer = ConstructedTermUntypedBackend
          .lower(ConstructedTerm.fromShape(shape).toOption.get)
          .toOption
          .get
        assertEquals(
          TermShapeInspector.rawStructure(direct),
          TermShapeInspector.rawStructure(richer),
          clues(shape)
        )
      }
    }
  }

  test("N005 P0 and P1 project through the direct exact backend without manufacturing P0 Block") {
    val cases = Vector(
      "{ 1; 2 }",
      "{ first(); second(); result }",
      "{ (1, true); if flag then 2 else 3 }",
      "{ 1; { 2; 3 }; 4 }"
    )

    withContext {
      cases.foreach { source =>
        val projected = project(source)
        val raw = CoreTermShapeUntypedLowerer.lower(projected).toOption.get
        assertEquals(TermShapeInspector.inspect(raw), projected, clues(source))
      }

      val p0 = project("{ 1 }")
      assert(!p0.isInstanceOf[TermShape.Block])
      assert(CoreTermShapeUntypedLowerer.lower(p0).toOption.get.isInstanceOf[untpd.Number])
    }
  }

  test("reconciled N006 P2 projection keeps direct rejection and reaches U007 richer routes") {
    val p2 = project("{ val x: Int = 1; x }")
    p2 match
      case TermShape.Block(
            List(BlockStatement.LocalVal(binder, "x", "Int", TermShape.Literal("1"))),
            TermShape.BoundReference(reference, "x")
          ) => assertEquals(reference, binder)
      case other => fail(s"unexpected reconciled N006 P2 shape: ${other.render}")
    val constructed = ConstructedTerm.fromShape(p2).toOption.get

    withContext {
      assert(CoreTermShapeUntypedLowerer.lower(p2).isLeft)
      assert(ConstructedTermUntypedBackend.lower(constructed).isRight)
      assert(
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<u007-p2-accepted>")
          .isRight
      )
    }
  }

  private def block(prefix: List[TermShape], result: TermShape): TermShape =
    TermShape.Block(prefix, result)

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def project(source: String): TermShape =
    val term = Input.String(source).parse[Term].get
    ScalametaTermProjection.project(term).toOption.get.shape

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val children =
      tree match
        case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
        case value: untpd.Select => Vector(value.qualifier)
        case value: untpd.Apply => value.fun +: value.args.toVector
        case value: untpd.New => Vector(value.tpt)
        case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
        case value: untpd.PrefixOp => Vector(value.op, value.od)
        case value: untpd.InterpolatedString => value.segments.toVector
        case value: untpd.Thicket => value.trees.toVector
        case value: untpd.Tuple => value.trees.toVector
        case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
        case value: untpd.Block => value.stats.toVector :+ value.expr
        case value: untpd.Typed => Vector(value.expr, value.tpt)
        case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
        case value: untpd.Function => value.args.toVector :+ value.body
        case value: untpd.Parens => Vector(value.t)
        case _ => Vector.empty
    tree +: children.flatMap(allTrees)

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
    assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
    assertEquals(tree.symbol, NoSymbol)
    assert(!tree.isInstanceOf[untpd.TypedSplice])

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(root, sidecars).asInstanceOf[ConstructedTerm]
