package quasiquotes.terms.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.parser.{
  DottySourceSpanAdapter,
  TermShape,
  TermShapeInspector,
  TinyTermParser
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class ConstructedTermGeneratedOriginAdapterTest extends munit.FunSuite:
  import ConstructedTermGeneratedOriginError.*
  import TypeNormalForm.*

  private val PeerSourceName =
    "<macroparadise-generated:externalQuasiquotesTerm:QuasiquotesBackendUser>"
  private val PeerSource =
    """if true then ("phase44:" + "QuasiquotesBackendUser"): String else "unreachable""""

  test("reproduces the exact peer source identity text and complete nine-node span map") {
    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(peerFixture, PeerSourceName)
          .toOption
          .get

      assertEquals(result.virtualSourceName, PeerSourceName)
      assertEquals(result.generatedSource, PeerSource)
      assertEquals(result.generatedSource.length, 79)

      val nodes = peerNodes(result.tree)
      assertSpan(nodes("root"), 0, 79, 0)
      assertSpan(nodes("condition"), 3, 7, 3)
      assertSpan(nodes("then.typed"), 13, 60, 13)
      assertSpan(nodes("then.type"), 54, 60, 54)
      assertSpan(nodes("then.infix"), 14, 51, 25)
      assertSpan(nodes("then.infix.left"), 14, 24, 14)
      assertSpan(nodes("then.infix.operator"), 25, 26, 25)
      assertSpan(nodes("then.infix.right"), 27, 51, 27)
      assertSpan(nodes("else"), 66, 79, 66)

      assertEquals(nodes.size, 9)
      assertPositionedResult(result, peerFixture)
    }
  }

  test("delimits every supported prefix from a negative decimal operand") {
    val expected = Vector(
      "-" -> "-(-1)",
      "+" -> "+(-1)",
      "!" -> "!(-1)",
      "~" -> "~(-1)"
    )

    withContext {
      expected.zipWithIndex.foreach { case ((operator, source), index) =>
        val constructed =
          fromShape(
            TermShape.Unary(operator, TermShape.Literal("-1"))
          )
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-origin-prefix-negative-$index>")
            .toOption
            .get

        assertEquals(result.generatedSource, source)
        result.tree match
          case root @ untpd.PrefixOp(rawOperator, operand) =>
            assertSpan(root, 0, 5, 0)
            assertSpan(rawOperator, 0, 1, 0)
            assertSpan(operand, 2, 4, 2)
          case other =>
            fail(s"expected PrefixOp, found ${other.getClass.getSimpleName}")
        assertPositionedResult(result, constructed)
      }
    }
  }

  test("repairs a completed core term received across the backend SPI") {
    val completed =
      fromShape(TermShape.Unary("-", TermShape.Literal("-1")))

    assertEquals(
      completed.root,
      TermShape.Unary("-", TermShape.Literal("-1"))
    )
    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(completed, "<generated-origin-completed-prefix-negative>")
          .toOption
          .get
      assertEquals(result.generatedSource, "-(-1)")
      assertPositionedResult(result, completed)
    }
  }

  test("keeps nested prefix operands lexically separated") {
    val cases = Vector(
      fromShape(
        TermShape.Unary(
          "-",
          TermShape.Unary("-", ident("x"))
        )
      ) -> "-(-x)",
      fromShape(
        TermShape.Unary(
          "!",
          TermShape.Unary("~", ident("bits"))
        )
      ) -> "!(~bits)"
    )

    withContext {
      cases.zipWithIndex.foreach { case ((constructed, expected), index) =>
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-origin-nested-prefix-$index>")
            .toOption
            .get
        assertEquals(result.generatedSource, expected)
        assertPositionedResult(result, constructed)
      }
    }
  }

  test("keeps the old raw backend recursively source-free span-free and symbol-free") {
    withContext {
      val constructed =
        typed(
          TermShape.If(
            ident("condition"),
            TermShape.Apply(
              TermShape.Select(ident("service"), "answer"),
              List(TermShape.Tuple(List(ident("left"), ident("right"))))
            ),
            TermShape.Literal("\"fallback\"")
          ),
          STypeApply(STypeIdent("List"), List(STypeIdent("String")))
        )
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assertEquals(DottySourceSpanAdapter.fromTree(tree), None)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("positions every admitted term form with structural parser agreement") {
    val values = Vector(
      fromShape(ident("value")),
      fromShape(TermShape.Literal("123")),
      fromShape(TermShape.Literal("true")),
      fromShape(TermShape.Literal("\"text\"")),
      fromShape(TermShape.Select(ident("service"), "answer")),
      fromShape(
        TermShape.Apply(
          TermShape.Select(ident("service"), "answer"),
          List(ident("left"), ident("right"))
        )
      ),
      fromShape(TermShape.Infix(ident("left"), "+", ident("right"))),
      fromShape(
        TermShape.Infix(
          TermShape.Infix(ident("a"), "+", ident("b")),
          "*",
          TermShape.Infix(ident("c"), "+", ident("d"))
        )
      ),
      fromShape(TermShape.Unary("+", ident("value"))),
      fromShape(TermShape.Unary("-", ident("value"))),
      fromShape(TermShape.Unary("!", ident("condition"))),
      fromShape(TermShape.Unary("~", ident("bits"))),
      typed(ident("value"), STypeIdent("Int")),
      fromShape(TermShape.Tuple(List(ident("left"), ident("right")))),
      fromShape(
        TermShape.If(ident("condition"), ident("left"), ident("right"))
      ),
      fromShape(TermShape.Parenthesized(ident("value")))
    )

    withContext {
      values.zipWithIndex.foreach { case (constructed, index) =>
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-origin-shape-$index>")
            .toOption
            .get
        assertPositionedResult(result, constructed)
      }
    }
  }

  test("positions every admitted completed type family and nested sidecars in preorder") {
    val normalForms = Vector(
      STypeIdent("Int"),
      STypeIdent("String"),
      STypeIdent("Boolean"),
      STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      STypeApply(
        STypeIdent("Option"),
        List(STypeApply(STypeIdent("List"), List(STypeIdent("String"))))
      ),
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
      STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ),
      STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      ),
      STypeApply(
        STypeIdent("List"),
        List(STypeFunction(List(STypeIdent("Int")), STypeIdent("String")))
      )
    )

    withContext {
      normalForms.zipWithIndex.foreach { case (normalForm, index) =>
        val constructed = typed(ident("value"), normalForm)
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-origin-type-$index>")
            .toOption
            .get
        assertPositionedResult(result, constructed)
      }

      val outer = STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      )
      val firstInner =
        STypeApply(STypeIdent("List"), List(STypeIdent("Int")))
      val secondInner =
        STypeFunction(
          List(STypeIdent("Int"), STypeIdent("String")),
          STypeIdent("Boolean")
        )
      val root =
        TermShape.Typed(
          TermShape.Tuple(
            List(
              TermShape.Typed(ident("left"), renderType(firstInner)),
              TermShape.Typed(ident("right"), renderType(secondInner))
            )
          ),
          renderType(outer)
        )
      val constructed =
        ConstructedTerm
          .create(root, Vector(outer, firstInner, secondInner))
          .toOption
          .get
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<generated-origin-nested-sidecars>")
          .toOption
          .get
      assertPositionedResult(result, constructed)
      assertEquals(
        typedTypeTexts(result.tree),
        Vector(renderType(outer), renderType(firstInner), renderType(secondInner))
      )
    }
  }

  test("positions Tuple2 Tuple3 and Tuple22 term boundaries") {
    withContext {
      Vector(2, 3, 22).foreach { arity =>
        val constructed =
          fromShape(
            TermShape.Tuple(
              (1 to arity).map(index => ident(s"value$index")).toList
            )
          )
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-origin-tuple-$arity>")
            .toOption
            .get
        assertPositionedResult(result, constructed)
        result.tree match
          case untpd.Tuple(elements) => assertEquals(elements.size, arity)
          case other =>
            fail(s"expected Tuple, found ${other.getClass.getSimpleName}")
      }
    }
  }

  test("escapes semantic strings and counts BMP and surrogate-pair offsets as UTF-16") {
    val semantic =
      "\"quote=\" slash=\\ newline=\n return=\r tab=\t back=\b form=\f control=\u0001 BMP=λ supplementary=😀\""
    val constructed = fromShape(TermShape.Literal(semantic))

    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<generated-origin-string-fidelity>")
          .toOption
          .get

      assert(result.generatedSource.contains("\\\""))
      assert(result.generatedSource.contains("\\\\"))
      assert(result.generatedSource.contains("\\n"))
      assert(result.generatedSource.contains("\\r"))
      assert(result.generatedSource.contains("\\t"))
      assert(result.generatedSource.contains("\\b"))
      assert(result.generatedSource.contains("\\f"))
      assert(result.generatedSource.contains("\\u0001"))
      assert(result.generatedSource.contains("λ"))
      assert(result.generatedSource.contains("😀"))
      assertEquals(result.tree.span.end, result.generatedSource.length)
      assertPositionedResult(result, constructed)
    }
  }

  test("uses structural offsets for repeated collision-prone literal text") {
    val repeated = TermShape.Literal("\"same\"")
    val constructed =
      fromShape(
        TermShape.Apply(
          ident("f"),
          List(repeated, repeated, TermShape.Tuple(List(repeated, repeated)))
        )
      )

    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<generated-origin-repeated-text>")
          .toOption
          .get
      val literalSpans =
        allTrees(result.tree)
          .collect { case literal: untpd.Literal =>
            literal.span.start -> literal.span.end
          }

      assertEquals(literalSpans.distinct.size, 4)
      assertEquals(literalSpans, literalSpans.sortBy(_._1))
      assertPositionedResult(result, constructed)
    }
  }

  test("renders long negative decimals and bounded backticked keywords truthfully") {
    val constructed =
      fromShape(
        TermShape.Tuple(
          List(
            TermShape.Literal("-214748364900000000000000000000"),
            ident("class"),
            TermShape.Select(ident("value"), "type")
          )
        )
      )

    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<generated-origin-name-fidelity>")
          .toOption
          .get
      assertEquals(
        result.generatedSource,
        "(-214748364900000000000000000000, `class`, value.`type`)"
      )
      assertPositionedResult(result, constructed)
    }
  }

  test("returns stable ordinary-input errors") {
    withContext {
      Vector("", " whitespace ", "bad\nname", "bad\rname", "bad\u0000name")
        .foreach { name =>
          val result =
            ConstructedTermGeneratedOriginAdapter.lower(
              fromShape(ident("value")),
              name
            )
          assert(result.left.toOption.exists(_.isInstanceOf[InvalidVirtualSourceName]))
        }

      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          fromShape(ident("not-renderable.name")),
          "<generated-origin-invalid-ident>"
        ),
        Left(UnrenderableName("identifier", "not-renderable.name"))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          fromShape(TermShape.Select(ident("value"), "bad.member")),
          "<generated-origin-invalid-member>"
        ),
        Left(UnrenderableName("selected member", "bad.member"))
      )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          fromShape(TermShape.Literal("3.14")),
          "<generated-origin-invalid-literal>"
        ),
        Left(UnsupportedLiteral("3.14"))
      )
    }
  }

  test("detects defensive corruptions without throwing") {
    withContext {
      val unsupportedUnary =
        corrupt(
          TermShape.Unary("*", ident("value")),
          Vector.empty
        )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          unsupportedUnary,
          "<generated-origin-corrupt-unary>"
        ),
        Left(UnsupportedUnaryOperator("*"))
      )

      val missing =
        corrupt(
          TermShape.Typed(ident("value"), "Int"),
          Vector.empty
        )
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          missing,
          "<generated-origin-missing-sidecar>"
        ),
        Left(MissingTypeSidecar(0))
      )

      val extra =
        corrupt(ident("value"), Vector(STypeIdent("Int")))
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          extra,
          "<generated-origin-extra-sidecar>"
        ),
        Left(UnconsumedTypeSidecars(0, 1))
      )
    }
  }

  test("detects incomplete source-mismatched out-of-bounds and noncontained maps") {
    withContext {
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(peerFixture, PeerSourceName)
          .toOption
          .get
      val raw = ConstructedTermUntypedBackend.lower(peerFixture).toOption.get

      assert(
        ConstructedTermGeneratedOriginAdapter
          .validatePositionedForTest(
            raw,
            result.sourceFile,
            result.generatedSource.length
          )
          .left
          .toOption
          .exists(_.isInstanceOf[IncompletePositionMap])
      )

      val otherSource = SourceFile.virtual("<other-generated-origin>", PeerSource)
      val wrongSource = result.tree.cloneIn(otherSource)
      assert(
        ConstructedTermGeneratedOriginAdapter
          .validatePositionedForTest(
            wrongSource,
            result.sourceFile,
            result.generatedSource.length
          )
          .left
          .toOption
          .exists(_.message.contains("instead of"))
      )

      val outOfBounds =
        result.tree.withSpan(Span(0, result.generatedSource.length + 1, 0))
      assert(
        ConstructedTermGeneratedOriginAdapter
          .validatePositionedForTest(
            outOfBounds,
            result.sourceFile,
            result.generatedSource.length
          )
          .left
          .toOption
          .exists(_.message.contains("out-of-bounds"))
      )

      val noncontained =
        result.tree match
          case root: untpd.If =>
            val condition =
              root.cond.withSpan(Span(0, result.generatedSource.length, 0))
            untpd.cpy.If(root)(condition, root.thenp, root.elsep)
          case other =>
            fail(s"expected If, found ${other.getClass.getSimpleName}")
      assert(
        ConstructedTermGeneratedOriginAdapter
          .validatePositionedForTest(
            noncontained,
            result.sourceFile,
            result.generatedSource.length
          )
          .left
          .toOption
          .exists(error =>
            error.message.contains("overlap") ||
              error.message.contains("out of source order")
          )
      )
    }
  }

  test("keeps the adapter internal parser-free Quotes-free and framework-free") {
    val root =
      Path.of("dotty-internal", "src", "main", "scala", "quasiquotes", "terms", "dotty")
    val adapter =
      Files.readString(
        root.resolve("ConstructedTermGeneratedOriginAdapter.scala"),
        StandardCharsets.UTF_8
      )
    val result =
      Files.readString(
        root.resolve("GeneratedOriginTermResult.scala"),
        StandardCharsets.UTF_8
      )
    val rawBackend =
      Files.readString(
        root.resolve("ConstructedTermUntypedBackend.scala"),
        StandardCharsets.UTF_8
      )

    assert(adapter.contains("private[quasiquotes] object ConstructedTermGeneratedOriginAdapter"))
    assert(result.contains("private[quasiquotes] final class GeneratedOriginTermResult"))
    assert(!adapter.contains("TinyTermParser"))
    assert(!adapter.contains("dotty.tools.dotc.parsing"))
    assert(!adapter.contains("scala.quoted"))
    assert(!adapter.contains("quotes.reflect"))
    assert(!adapter.contains("MacroParadise"))
    assert(!adapter.contains("trait Backend"))
    assert(!adapter.contains("ConstructedDefinition"))
    assert(!rawBackend.contains("ConstructedTermGeneratedOriginAdapter"))
    assert(!rawBackend.contains("GeneratedOriginTermResult"))
  }

  private def peerFixture: ConstructedTerm =
    val root =
      TermShape.If(
        TermShape.Literal("true"),
        TermShape.Typed(
          TermShape.Infix(
            TermShape.Literal("\"phase44:\""),
            "+",
            TermShape.Literal("\"QuasiquotesBackendUser\"")
          ),
          "String"
        ),
        TermShape.Literal("\"unreachable\"")
      )
    ConstructedTerm
      .create(root, Vector(STypeIdent("String")))
      .toOption
      .get

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def fromShape(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).toOption.get

  private def typed(
      expression: TermShape,
      normalForm: TypeNormalForm
  ): ConstructedTerm =
    ConstructedTerm
      .create(
        TermShape.Typed(expression, renderType(normalForm)),
        Vector(normalForm)
      )
      .toOption
      .get

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def renderType(normalForm: TypeNormalForm): String =
    normalForm match
      case STypeIdent(name) => name
      case STypeApply(constructor, arguments) =>
        s"${renderType(constructor)}[${arguments.map(renderType).mkString(", ")}]"
      case STypeTuple(elements) =>
        s"(${elements.map(renderType).mkString(", ")})"
      case STypeFunction(argument :: Nil, result) =>
        s"${renderType(argument)} => ${renderType(result)}"
      case STypeFunction(arguments, result) =>
        s"(${arguments.map(renderType).mkString(", ")}) => ${renderType(result)}"

  private def assertPositionedResult(
      result: GeneratedOriginTermResult,
      constructed: ConstructedTerm
  )(using Context): Unit =
    assertEquals(TermShapeInspector.inspect(result.tree), constructed.root)
    val reparsed = TinyTermParser.parseOrThrow(result.generatedSource)
    assertEquals(
      eraseParentheses(reparsed.shape),
      eraseParentheses(constructed.root)
    )
    allTrees(result.tree).foreach { tree =>
      assert(tree.source.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.source.path, result.virtualSourceName)
      assert(tree.span.exists, clues(tree.getClass.getSimpleName))
      assert(tree.span.start >= 0)
      assert(tree.span.start <= tree.span.point)
      assert(tree.span.point <= tree.span.end)
      assert(tree.span.end <= result.generatedSource.length)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }
    assertEquals(result.tree.span.start, 0)
    assertEquals(result.tree.span.end, result.generatedSource.length)

  private def assertSpan(
      tree: untpd.Tree,
      start: Int,
      end: Int,
      point: Int
  ): Unit =
    assertEquals(tree.span.start, start)
    assertEquals(tree.span.end, end)
    assertEquals(tree.span.point, point)

  private def peerNodes(tree: untpd.Tree): Map[String, untpd.Tree] =
    tree match
      case root @ untpd.If(condition, typed @ untpd.Typed(
            infix @ untpd.InfixOp(left, operator, right),
            typeTree
          ), elseBranch) =>
        Map(
          "root" -> root,
          "condition" -> condition,
          "then.typed" -> typed,
          "then.type" -> typeTree,
          "then.infix" -> infix,
          "then.infix.left" -> left,
          "then.infix.operator" -> operator,
          "then.infix.right" -> right,
          "else" -> elseBranch
        )
      case other =>
        fail(s"unexpected peer fixture tree: ${other.getClass.getSimpleName}")

  private def typedTypeTexts(tree: untpd.Tree): Vector[String] =
    tree match
      case value: untpd.Typed =>
        Vector(TermShapeInspector.inspect(value).asInstanceOf[TermShape.Typed].typeName) ++
          typedTypeTexts(value.expr)
      case value: untpd.Select => typedTypeTexts(value.qualifier)
      case value: untpd.Apply =>
        typedTypeTexts(value.fun) ++ value.args.toVector.flatMap(typedTypeTexts)
      case value: untpd.InfixOp =>
        typedTypeTexts(value.left) ++ typedTypeTexts(value.right)
      case value: untpd.PrefixOp => typedTypeTexts(value.od)
      case value: untpd.Tuple => value.trees.toVector.flatMap(typedTypeTexts)
      case value: untpd.If =>
        typedTypeTexts(value.cond) ++
          typedTypeTexts(value.thenp) ++
          typedTypeTexts(value.elsep)
      case value: untpd.Parens => typedTypeTexts(value.t)
      case _ => Vector.empty

  private def eraseParentheses(shape: TermShape): TermShape =
    shape match
      case TermShape.Parenthesized(expression) =>
        eraseParentheses(expression)
      case TermShape.Select(qualifier, name) =>
        TermShape.Select(eraseParentheses(qualifier), name)
      case TermShape.Apply(function, arguments) =>
        TermShape.Apply(
          eraseParentheses(function),
          arguments.map(eraseParentheses)
        )
      case TermShape.Infix(left, operator, right) =>
        TermShape.Infix(
          eraseParentheses(left),
          operator,
          eraseParentheses(right)
        )
      case TermShape.Unary(operator, operand) =>
        TermShape.Unary(operator, eraseParentheses(operand))
      case TermShape.Typed(expression, typeName) =>
        TermShape.Typed(eraseParentheses(expression), "<validated-type-sidecar>")
      case TermShape.Tuple(elements) =>
        TermShape.Tuple(elements.map(eraseParentheses))
      case TermShape.If(condition, thenBranch, elseBranch) =>
        TermShape.If(
          eraseParentheses(condition),
          eraseParentheses(thenBranch),
          eraseParentheses(elseBranch)
        )
      case other => other

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    val children =
      tree match
        case value: untpd.Select => Vector(value.qualifier)
        case value: untpd.Apply => value.fun +: value.args.toVector
        case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
        case value: untpd.PrefixOp => Vector(value.op, value.od)
        case value: untpd.Typed => Vector(value.expr, value.tpt)
        case value: untpd.AppliedTypeTree =>
          value.tpt +: value.args.toVector
        case value: untpd.Tuple => value.trees.toVector
        case value: untpd.Function => value.args.toVector :+ value.body
        case value: untpd.If =>
          Vector(value.cond, value.thenp, value.elsep)
        case value: untpd.Parens => Vector(value.t)
        case _ => Vector.empty
    tree +: children.flatMap(allTrees)

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor =
      classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(root, sidecars)
      .asInstanceOf[ConstructedTerm]
