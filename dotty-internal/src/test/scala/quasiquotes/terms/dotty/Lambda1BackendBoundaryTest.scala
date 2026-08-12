package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class Lambda1BackendBoundaryTest extends munit.FunSuite:
  import ConstructedTermGeneratedOriginError as GeneratedError
  import ConstructedTermUntypedBackendError as RawError

  private final case class Fixture(
      source: String,
      parameterType: TypeNormalForm,
      body: BinderId => TermShape
  )

  private val fixtures = Vector(
    Fixture("(x: Int) => x", typeIdent("Int"), bound),
    Fixture(
      "(x: Int) => x + 1",
      typeIdent("Int"),
      id => TermShape.Infix(bound(id), "+", TermShape.Literal("1"))
    ),
    Fixture(
      "(x: Int) => f(x)",
      typeIdent("Int"),
      id => TermShape.Apply(ident("f"), List(bound(id)))
    ),
    Fixture("(x: String) => x", typeIdent("String"), bound),
    Fixture(
      "(x: Boolean) => if x then false else true",
      typeIdent("Boolean"),
      id =>
        TermShape.If(
          bound(id),
          TermShape.Literal("false"),
          TermShape.Literal("true")
        )
    )
  )

  fixtures.foreach { fixture =>
    test(s"source-free exact Lambda1 lowering matches the raw parser: ${fixture.source}") {
      withParserContext(fixture.source) { expected =>
        val constructed = lambda(fixture.parameterType, fixture.body)
        val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

        assertEquals(structure(raw), structure(expected))
        raw match
          case function: untpd.Function =>
            assertEquals(function.args.size, 1)
            val parameter = function.args.head.asInstanceOf[untpd.ValDef]
            assertEquals(parameter.name.toString, "x")
            assertEquals(parameter.mods.flags, Flags.Param)
            assert(parameter.rhs.isEmpty)
          case other =>
            fail(s"expected Function, found ${other.getClass.getSimpleName}")
        allTrees(raw).foreach { tree =>
          assert(!tree.source.exists)
          assert(!tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }

    test(s"generated-origin Lambda1 source and positions match the raw parser: ${fixture.source}") {
      withParserContext(fixture.source) { expected =>
        val constructed = lambda(fixture.parameterType, fixture.body)
        val result = ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<lambda1-generated-origin>")
          .toOption
          .get

        assertEquals(result.generatedSource, fixture.source)
        assertEquals(
          snapshot(result.tree, result.generatedSource),
          snapshot(expected, fixture.source)
        )
        allTrees(result.tree).foreach { tree =>
          assertEquals(tree.source.path, result.sourceFile.path)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("bound-reference BinderId overrides hostile display text") {
    val binderId = BinderId(7)
    val constructed = ConstructedTerm
      .create(
        TermShape.Lambda1(
          binderId,
          "declared",
          "Int",
          TermShape.BoundReference(binderId, "misleading")
        ),
        Vector(typeIdent("Int"))
      )
      .toOption
      .get

    ConstructedTermUntypedBackend.lower(constructed).toOption.get match
      case function: untpd.Function =>
        val parameter = function.args.head.asInstanceOf[untpd.ValDef]
        val body = function.body.asInstanceOf[untpd.Ident]
        assertEquals(parameter.name.toString, "declared")
        assertEquals(body.name.toString, "declared")
      case other => fail(s"expected Function, found ${other.getClass.getSimpleName}")
  }

  test("free same-text identifiers remain ordinary while bound lookup uses BinderId") {
    val binderId = BinderId(8)
    val constructed = ConstructedTerm
      .create(
        TermShape.Lambda1(
          binderId,
          "x",
          "Int",
          TermShape.Tuple(
            List(
              ident("x"),
              TermShape.BoundReference(binderId, "hostile-bound-text")
            )
          )
        ),
        Vector(typeIdent("Int"))
      )
      .toOption
      .get

    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val bodyNames = raw.asInstanceOf[untpd.Function].body
        .asInstanceOf[untpd.Tuple]
        .trees
        .map(_.asInstanceOf[untpd.Ident].name.toString)
      assertEquals(bodyNames, List("x", "x"))

      val generated = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<lambda1-free-bound-same-text>")
        .toOption
        .get
      assertEquals(generated.generatedSource, "(x: Int) => (x, x)")
    }
  }

  test("Lambda parameter and typed-body sidecars are consumed once in preorder") {
    val binderId = BinderId(3)
    val root = TermShape.Lambda1(
      binderId,
      "x",
      "Int",
      TermShape.Tuple(
        List(
          TermShape.Typed(bound(binderId), "String"),
          TermShape.Typed(
            TermShape.Apply(ident("f"), List(bound(binderId))),
            "Boolean"
          )
        )
      )
    )
    val constructed = ConstructedTerm
      .create(
        root,
        Vector(typeIdent("Int"), typeIdent("String"), typeIdent("Boolean"))
      )
      .toOption
      .get
    withContext {
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

      assertEquals(
        allTrees(raw).collect {
          case parameter: untpd.ValDef => typeText(parameter.tpt)
          case typed: untpd.Typed => typeText(typed.tpt)
        },
        Vector("Int", "String", "Boolean")
      )
    }
  }

  test("a single Lambda1 remains supported as an ordinary application argument") {
    val binderId = BinderId(11)
    val constructed = ConstructedTerm
      .create(
        TermShape.Apply(
          ident("consume"),
          List(TermShape.Lambda1(binderId, "x", "Int", bound(binderId)))
        ),
        Vector(typeIdent("Int"))
      )
      .toOption
      .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    raw match
      case application: untpd.Apply =>
        assert(application.args.head.isInstanceOf[untpd.Function])
      case other => fail(s"expected Apply, found ${other.getClass.getSimpleName}")
  }

  test("generated origin positions a Lambda1 application argument recursively") {
    val binderId = BinderId(11)
    val constructed = ConstructedTerm
      .create(
        TermShape.Apply(
          ident("consume"),
          List(TermShape.Lambda1(binderId, "x", "Int", bound(binderId)))
        ),
        Vector(typeIdent("Int"))
      )
      .toOption
      .get
    val source = "consume((x: Int) => x)"

    withParserContext(source) { expected =>
      val result = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<lambda1-argument>")
        .toOption
        .get
      assertEquals(result.generatedSource, source)
      assertEquals(snapshot(result.tree, source), snapshot(expected, source))
    }
  }

  test("completed sidecars control parameter and typed-body rendering in preorder") {
    val binderId = BinderId(13)
    val constructed = ConstructedTerm
      .create(
        TermShape.Lambda1(
          binderId,
          "x",
          "Int",
          TermShape.Typed(bound(binderId), "String")
        ),
        Vector(typeIdent("Int"), typeIdent("String"))
      )
      .toOption
      .get
    val source = "(x: Int) => (x: String)"

    withParserContext(source) { expected =>
      val result = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<lambda1-typed-body>")
        .toOption
        .get
      assertEquals(result.generatedSource, source)
      assertEquals(snapshot(result.tree, source), snapshot(expected, source))
      assertEquals(
        allTrees(result.tree).collect {
          case parameter: untpd.ValDef => typeText(parameter.tpt)
          case typed: untpd.Typed => typeText(typed.tpt)
        },
        Vector("Int", "String")
      )
    }
  }

  test("completed parameter sidecar overrides hostile stored type text") {
    val binderId = BinderId(17)
    val constructed = corrupt(
      TermShape.Lambda1(
        binderId,
        "declared",
        "HostileStoredTypeText",
        TermShape.BoundReference(binderId, "misleading")
      ),
      Vector(typeIdent("Int"))
    )
    val source = "(declared: Int) => declared"

    withParserContext(source) { expected =>
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      assertEquals(structure(raw), structure(expected))
      val result = ConstructedTermGeneratedOriginAdapter
        .lower(constructed, "<hostile-lambda1-text>")
        .toOption
        .get
      assertEquals(result.generatedSource, source)
      assertEquals(snapshot(result.tree, source), snapshot(expected, source))
    }
  }

  test("stray bound references fail closed by BinderId in both backend modes") {
    val binderId = BinderId(99)
    val constructed = corrupt(
      TermShape.BoundReference(binderId, "free-looking"),
      Vector.empty
    )

    assertEquals(
      ConstructedTermUntypedBackend.lower(constructed),
      Left(RawError.OutOfScopeBoundReference(99))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          constructed,
          "<stray-bound-reference>"
        ),
        Left(GeneratedError.OutOfScopeBoundReference(99))
      )
    }
  }

  test("nested or conflicting Lambda1 binder scopes remain rejected") {
    val outer = BinderId(1)
    val inner = BinderId(2)
    val constructed = corrupt(
      TermShape.Lambda1(
        outer,
        "x",
        "Int",
        TermShape.Lambda1(inner, "y", "String", bound(inner))
      ),
      Vector(typeIdent("Int"), typeIdent("String"))
    )

    assertEquals(
      ConstructedTermUntypedBackend.lower(constructed),
      Left(RawError.NestedLambda1Unsupported)
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          constructed,
          "<nested-lambda1>"
        ),
        Left(GeneratedError.NestedLambda1Unsupported)
      )
    }
  }

  test("Lambda parameter sidecar failures retain exact ordinals") {
    val binderId = BinderId(23)
    val shape = TermShape.Lambda1(binderId, "x", "Int", bound(binderId))
    val missing = corrupt(shape, Vector.empty)
    val unsupportedType = TypeNormalForm.STypeIdent("UnsupportedType")
    val unsupported = corrupt(shape, Vector(unsupportedType))
    val extra = corrupt(
      shape,
      Vector(typeIdent("Int"), typeIdent("String"))
    )

    assertEquals(
      ConstructedTermUntypedBackend.lower(missing),
      Left(RawError.MissingTypeSidecar(0))
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(unsupported),
      Left(RawError.UnsupportedTypeSidecar(0, unsupportedType.render))
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(extra),
      Left(RawError.UnconsumedTypeSidecars(1, 2))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          missing,
          "<missing-lambda-sidecar>"
        ),
        Left(GeneratedError.MissingTypeSidecar(0))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          unsupported,
          "<unsupported-lambda-sidecar>"
        ),
        Left(GeneratedError.UnsupportedTypeSidecar(0, unsupportedType.render))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          extra,
          "<extra-lambda-sidecar>"
        ),
        Left(GeneratedError.UnconsumedTypeSidecars(1, 2))
      )
    }
  }

  private final case class TreeStructure(
      kind: String,
      detail: String,
      children: Vector[TreeStructure]
  )

  private final case class TreeSnapshot(
      kind: String,
      start: Int,
      point: Int,
      end: Int,
      slice: String,
      detail: String,
      children: Vector[TreeSnapshot]
  )

  private def structure(tree: untpd.Tree)(using Context): TreeStructure =
    TreeStructure(
      tree.getClass.getSimpleName,
      treeDetail(tree),
      directChildren(tree).map(structure)
    )

  private def snapshot(tree: untpd.Tree, source: String)(using Context): TreeSnapshot =
    val span = tree.span
    TreeSnapshot(
      tree.getClass.getSimpleName,
      span.start,
      span.point,
      span.end,
      source.slice(span.start, span.end),
      treeDetail(tree),
      directChildren(tree).map(snapshot(_, source))
    )

  private def treeDetail(tree: untpd.Tree): String =
    tree match
      case value: untpd.Ident => s"name=${value.name}"
      case value: untpd.Select => s"name=${value.name}"
      case value: untpd.ValDef => s"name=${value.name},flags=${value.mods.flags}"
      case value: untpd.Apply => s"arguments=${value.args.size}"
      case value: untpd.InfixOp => s"operator=${value.op.name}"
      case value: untpd.Literal => s"constant=${value.const.value}"
      case _: untpd.Number => "number"
      case _ => ""

  private def withParserContext(source: String)(body: Context ?=> untpd.Tree => Unit): Unit =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile = SourceFile.virtual("Lambda1BackendOracle.scala", source)
    val parser = new Parser(sourceFile)
    val raw = parser.expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    assertEquals(parser.in.token, Tokens.EOF)
    body(raw)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def lambda(
      parameterType: TypeNormalForm,
      body: BinderId => TermShape
  ): ConstructedTerm =
    val binderId = BinderId(0)
    ConstructedTerm
      .create(
        TermShape.Lambda1(
          binderId,
          "x",
          typeSource(parameterType),
          body(binderId)
        ),
        Vector(parameterType)
      )
      .toOption
      .get

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def bound(binderId: BinderId): TermShape =
    TermShape.BoundReference(binderId, "x")

  private def typeIdent(name: String): TypeNormalForm =
    TypeNormalForm.STypeIdent(name)

  private def typeSource(normalForm: TypeNormalForm): String =
    normalForm match
      case TypeNormalForm.STypeIdent(name) => name
      case other => other.render

  private def typeText(tree: untpd.Tree): String =
    tree match
      case value: untpd.Ident => value.name.toString
      case other => other.getClass.getSimpleName

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(root, sidecars).asInstanceOf[ConstructedTerm]

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.InterpolatedString => value.segments.toVector
      case value: untpd.Thicket => value.trees.toVector
      case value: untpd.Block => value.stats.toVector :+ value.expr
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens => Vector(value.t)
      case _ => Vector.empty
