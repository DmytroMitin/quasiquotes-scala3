package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.parser.TermShape
import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.TermShapeInspector
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

class ConstructorNewBackendBoundaryTest extends munit.FunSuite:
  import ConstructedTermGeneratedOriginError as GeneratedError
  import ConstructedTermUntypedBackendError as RawError

  private val cases = Vector(
    "new java.lang.StringBuilder()" ->
      TermShape.New("java.lang.StringBuilder", Nil),
    "new java.lang.StringBuilder(16)" ->
      TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("16"))),
    "new java.lang.StringBuilder(\"u010\")" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Literal("\"u010\""))
      ),
    "new java.lang.RuntimeException(\"boom\")" ->
      TermShape.New(
        "java.lang.RuntimeException",
        List(TermShape.Literal("\"boom\""))
      ),
    "new java.lang.StringBuilder(foo(x))" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Apply(ident("foo"), List(ident("x"))))
      ),
    "new java.lang.StringBuilder(if cond then 8 else 16)" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(
          TermShape.If(
            ident("cond"),
            TermShape.Literal("8"),
            TermShape.Literal("16")
          )
        )
      ),
    "new synthetic.unresolved.Widget(new other.missing.Value(1))" ->
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(
          TermShape.New("other.missing.Value", List(TermShape.Literal("1")))
        )
      )
  )

  cases.foreach { case (source, shape) =>
    test(s"source-free exact backend closes constructor new structurally: $source") {
      withParserContext(source) { expected =>
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

        assertEquals(structure(raw), structure(expected))
        assertConstructorSkeleton(raw)
        assert(allConstructorNameRoles(raw).forall(isQualifiedTypePath))
        allTrees(raw).foreach { tree =>
          assert(!tree.source.exists)
          assert(!tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }

    test(s"generated-origin exact backend matches parser kind order and positions: $source") {
      withParserContext(source) { expected =>
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        val result = ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<phase75-constructor-new>")
          .toOption
          .get

        assertEquals(result.generatedSource, source)
        assertEquals(snapshot(result.tree, source), snapshot(expected, source))
        assertConstructorSkeleton(result.tree)
        assert(allConstructorNameRoles(result.tree).forall(isQualifiedTypePath))
        allTrees(result.tree).foreach { tree =>
          assertEquals(tree.source.path, result.sourceFile.path)
          assert(tree.span.exists)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("parser direct richer and generated constructor paths agree on Dotty name roles") {
    val required = Vector(
      "new java.lang.StringBuilder()" ->
        TermShape.New("java.lang.StringBuilder", Nil),
      "new java.lang.StringBuilder(\"u010\")" ->
        TermShape.New(
          "java.lang.StringBuilder",
          List(TermShape.Literal("\"u010\""))
        ),
      "new java.lang.RuntimeException(\"boom\")" ->
        TermShape.New(
          "java.lang.RuntimeException",
          List(TermShape.Literal("\"boom\""))
        )
    )

    required.foreach { case (source, shape) =>
      withParserContext(source) { parsed =>
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        val direct = CoreTermShapeUntypedLowerer.lower(shape).toOption.get
        val richer = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        val generated = ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<u010-constructor-name-role>")
          .toOption
          .get
          .tree

        val expected = Vector(false, false, true)
        assertEquals(constructorNameRoles(parsed), expected, clues(source, "parser"))
        assertEquals(constructorNameRoles(direct), expected, clues(source, "direct"))
        assertEquals(constructorNameRoles(richer), expected, clues(source, "richer"))
        assertEquals(constructorNameRoles(generated), expected, clues(source, "generated"))
      }
    }
  }

  test("constructor arguments consume completed type sidecars in deterministic preorder") {
    val root =
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(
          TermShape.Typed(ident("first"), "Int"),
          TermShape.New(
            "other.missing.Value",
            List(TermShape.Typed(ident("second"), "String"))
          ),
          TermShape.Typed(ident("third"), "Boolean")
        )
      )
    val constructed = ConstructedTerm
      .create(
        root,
        Vector(
          TypeNormalForm.STypeIdent("Int"),
          TypeNormalForm.STypeIdent("String"),
          TypeNormalForm.STypeIdent("Boolean")
        )
      )
      .toOption
      .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
    val base = new ContextBase
    given Context = base.initialCtx
    assertEquals(
      allTrees(raw).collect {
        case value: untpd.Typed =>
          value.tpt match
            case identifier: untpd.Ident => identifier.name.toString
            case other => other.getClass.getSimpleName
      },
      Vector("Int", "String", "Boolean")
    )

    val generated = ConstructedTermGeneratedOriginAdapter
      .lower(constructed, "<phase75-constructor-sidecars>")
      .toOption
      .get
    assertEquals(
      generated.generatedSource,
      "new synthetic.unresolved.Widget((first): Int, new other.missing.Value((second): String), (third): Boolean)"
    )
  }

  test("accepted N008 constructors compose through richer and generated exact backends") {
    val sources = Vector(
      "new java.lang.StringBuilder()",
      "new java.lang.StringBuilder(16)",
      "new synthetic.unresolved.Widget(1, 2)",
      "new java.lang.StringBuilder(foo(x))",
      "new java.lang.StringBuilder(if cond then 8 else 16)",
      "new synthetic.unresolved.Widget(new other.missing.Value(1))"
    )

    sources.foreach { source =>
      val projected = ScalametaTermProjection
        .project(Scala3(source).parse[Term].get)
        .toOption
        .get
      val constructed = ConstructedTerm.fromShape(projected.shape).toOption.get

      withParserContext(source) { parsed =>
        val richer = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        val generated = ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<u010-n008-generated-constructor>")
          .toOption
          .get

        assertEquals(
          TermShapeInspector.rawStructure(richer),
          TermShapeInspector.rawStructure(parsed),
          clues(source, "richer")
        )
        assertEquals(generated.generatedSource, source)
        assertEquals(
          TermShapeInspector.rawStructure(generated.tree),
          TermShapeInspector.rawStructure(parsed),
          clues(source, "generated")
        )
        assert(allConstructorNameRoles(richer).forall(isQualifiedTypePath))
        assert(allConstructorNameRoles(generated.tree).forall(isQualifiedTypePath))
      }
    }
  }

  test("defensive constructor corruptions return bounded internal errors") {
    val invalidNames = Vector(
      null,
      "",
      "StringBuilder",
      ".java.lang.StringBuilder",
      "java.lang.StringBuilder.",
      "java..lang.StringBuilder",
      "java.lang.StringBuilder[Int]",
      "java.lang.`StringBuilder`",
      "java.lang.StringBuilder$"
    )
    invalidNames.foreach { name =>
      val constructed = corrupt(TermShape.New(name, Nil), Vector.empty)
      assert(
        ConstructedTermUntypedBackend
          .lower(constructed)
          .left
          .toOption
          .exists(_.isInstanceOf[RawError.InvalidConstructorName])
      )
      withContext {
        assert(
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, "<phase75-invalid-constructor>")
            .left
            .toOption
            .exists(_.isInstanceOf[GeneratedError.InvalidConstructorName])
        )
      }
    }

    val nullArguments = corrupt(
      TermShape.New("java.lang.StringBuilder", null),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(nullArguments),
      Left(RawError.MalformedConstructorArguments(-1))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          nullArguments,
          "<phase75-null-arguments>"
        ),
        Left(GeneratedError.MalformedConstructorArguments(-1))
      )
    }

    val nullArgument = corrupt(
      TermShape.New("java.lang.StringBuilder", List(null)),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(nullArgument),
      Left(RawError.NullConstructorArgument(0))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          nullArgument,
          "<phase75-null-argument>"
        ),
        Left(GeneratedError.NullConstructorArgument(0))
      )
    }

    val laterNullArgument = corrupt(
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Literal("1"), null)
      ),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(laterNullArgument),
      Left(RawError.NullConstructorArgument(1))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          laterNullArgument,
          "<u010-later-null-argument>"
        ),
        Left(GeneratedError.NullConstructorArgument(1))
      )
    }

    val nestedMalformed = corrupt(
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.New("Value", Nil))
      ),
      Vector.empty
    )
    assert(
      ConstructedTermUntypedBackend
        .lower(nestedMalformed)
        .left
        .toOption
        .exists(_.isInstanceOf[RawError.InvalidConstructorName])
    )
    withContext {
      assert(
        ConstructedTermGeneratedOriginAdapter
          .lower(nestedMalformed, "<u010-nested-malformed-constructor>")
          .left
          .toOption
          .exists(_.isInstanceOf[GeneratedError.InvalidConstructorName])
      )
    }

    val unsupported = corrupt(
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Unsupported("LocalDefinition", "def local = 1"))
      ),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(unsupported),
      Left(RawError.UnsupportedTermNode("LocalDefinition"))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          unsupported,
          "<phase75-unsupported-argument>"
        ),
        Left(GeneratedError.UnsupportedTermNode("LocalDefinition"))
      )
    }

    val missingSidecar = corrupt(
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Typed(ident("value"), "Int"))
      ),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(missingSidecar),
      Left(RawError.MissingTypeSidecar(0))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          missingSidecar,
          "<phase75-missing-sidecar>"
        ),
        Left(GeneratedError.MissingTypeSidecar(0))
      )
    }
  }

  private final case class TreeSnapshot(
      kind: String,
      start: Int,
      point: Int,
      end: Int,
      slice: String,
      detail: String,
      children: Vector[TreeSnapshot]
  )

  private final case class TreeStructure(
      kind: String,
      detail: String,
      children: Vector[TreeStructure]
  )

  private def structure(tree: untpd.Tree)(using Context): TreeStructure =
    TreeStructure(
      tree.getClass.getName,
      treeDetail(tree),
      directChildren(tree).map(structure)
    )

  private def snapshot(tree: untpd.Tree, source: String)(using Context): TreeSnapshot =
    val span = tree.span
    TreeSnapshot(
      tree.getClass.getName,
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
      case value: untpd.Apply => s"arguments=${value.args.size}"
      case _: untpd.New => "new"
      case _ => ""

  private def assertConstructorSkeleton(tree: untpd.Tree)(using Context): Unit =
    assert(allTrees(tree).exists(_.isInstanceOf[untpd.New]))
    assert(allTrees(tree).exists {
      case value: untpd.Select =>
        value.name.toString == "<init>" && value.qualifier.isInstanceOf[untpd.New]
      case _ => false
    })

  private def constructorNameRoles(tree: untpd.Tree): Vector[Boolean] =
    tree match
      case untpd.Apply(untpd.Select(fresh: untpd.New, _), _) =>
        typePathNames(fresh.tpt).map(_.isTypeName)
      case other =>
        fail(s"unexpected constructor topology: ${other.getClass.getSimpleName}")

  private def typePathNames(
      tree: untpd.Tree
  ): Vector[dotty.tools.dotc.core.Names.Name] =
    tree match
      case identifier: untpd.Ident => Vector(identifier.name)
      case selected: untpd.Select =>
        typePathNames(selected.qualifier) :+ selected.name
      case other =>
        fail(s"unexpected constructor type path: ${other.getClass.getSimpleName}")

  private def allConstructorNameRoles(
      tree: untpd.Tree
  )(using Context): Vector[Vector[Boolean]] =
    allTrees(tree).collect { case fresh: untpd.New =>
      typePathNames(fresh.tpt).map(_.isTypeName)
    }

  private def isQualifiedTypePath(roles: Vector[Boolean]): Boolean =
    roles.size >= 2 && roles.init.forall(_ == false) && roles.last

  private def withParserContext(source: String)(body: Context ?=> untpd.Tree => Unit): Unit =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile = SourceFile.virtual("Phase75ConstructorOracle.scala", source)
    val parser = new Parser(sourceFile)
    val raw = parser.expr()
    assertEquals(reporter.pendingMessages.toList, Nil)
    assertEquals(parser.in.token, Tokens.EOF)
    body(raw)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

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
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case _ => Vector.empty
