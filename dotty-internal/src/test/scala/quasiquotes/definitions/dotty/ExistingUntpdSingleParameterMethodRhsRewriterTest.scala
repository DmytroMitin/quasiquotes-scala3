package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdSingleParameterMethodRhsRewriterTest extends munit.FunSuite:
  test("rewrites only the selected RHS while preserving the exact parameter and type objects") {
    withContext {
      val root = parseClass(
        """class Converter:
          |  val before: Int = 1
          |  def convert(x: Int): String = x.toString
          |  type After = String
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter
        .capture(root)
        .fold(problem => fail(problem.message), identity)
      val view = ExistingUntpdSingleParameterMethodView
        .capture(captured, 1)
        .fold(problem => fail(problem.message), identity)
      val originalNodes = ExistingUntpdClassMemberFilter.allTrees(root).map(tree =>
        (tree, tree.source, tree.span)
      )
      given SourceFile = NoSource
      val replacement = untpd.Literal(Constant("rewritten"))

      val result = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        result.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.SingleNode
      )
      val structural = result.structuralResult
      assert(!structural.rebuiltRoot.eq(root))
      assert(!structural.rebuiltTemplate.eq(template))
      assert(!structural.rebuiltTarget.eq(view.method))
      val structuralParameter =
        structural.rebuiltTarget.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(structuralParameter.eq(view.parameter))
      assert(structuralParameter.tpt.eq(view.parameterType))
      assert(structural.rebuiltTarget.tpt.eq(view.resultType))
      assert(structural.rebuiltTarget.rhs.eq(replacement))
      assert(structural.rebuiltTemplate.body.head.eq(template.body.head))
      assert(structural.rebuiltTemplate.body(2).eq(template.body(2)))

      val positioned = result.positionedResult
      val positionedParameter =
        positioned.positionedTarget.paramss.head.head.asInstanceOf[untpd.ValDef]
      assert(positionedParameter.eq(view.parameter))
      assert(positionedParameter.tpt.eq(view.parameterType))
      assert(positioned.positionedTarget.tpt.eq(view.resultType))
      assert(positioned.positionedTarget.rhs.eq(positioned.positionedReplacement))
      assertEquals(positioned.positionedRoot.source, root.source)
      assertEquals(positioned.positionedRoot.span, root.span)
      assertEquals(positioned.positionedTemplate.source, template.source)
      assertEquals(positioned.positionedTemplate.span, template.span)
      assertEquals(positioned.positionedTarget.source, view.method.source)
      assertEquals(positioned.positionedTarget.span, view.method.span)
      assertEquals(positioned.positionedReplacement.source, view.rhs.source)
      assertEquals(positioned.positionedReplacement.span, view.rhs.span)
      assert(positioned.positionedTemplate.body.head.eq(template.body.head))
      assert(positioned.positionedTemplate.body(2).eq(template.body(2)))

      val currentNodes = ExistingUntpdClassMemberFilter.allTrees(root)
      assertEquals(currentNodes.size, originalNodes.size)
      currentNodes.zip(originalNodes).foreach { case (tree, (original, source, span)) =>
        assert(tree.eq(original))
        assertEquals(tree.source, source)
        assertEquals(tree.span, span)
      }
    }
  }

  test("reuses the U005 direct-Ident Apply replacement family") {
    withContext {
      val view = viewFor(
        """class DirectApply:
          |  def transform(value: Int): Int = value + 1
          |""".stripMargin,
        0
      )
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Ident(termName("identity")),
        untpd.Number("9", untpd.NumberKind.Whole(10)) :: Nil
      )

      val result = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        result.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentApply
      )
      assertUniformReplacementSite(result)
    }
  }

  test("reuses the U013 direct-Ident-qualified selected Apply replacement family") {
    withContext {
      val view = viewFor(
        """class SelectedApply:
          |  def transform(value: Int): String = value.toString.reverse
          |""".stripMargin,
        0
      )
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Select(untpd.Ident(termName("String")), termName("valueOf")),
        untpd.Ident(termName("value")) :: Nil
      )

      val result = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(
        result.replacementFamily,
        ExistingUntpdSingleParameterMethodRhsRewriter.ReplacementFamily.DirectIdentQualifiedSelectedApply
      )
      assertUniformReplacementSite(result)
    }
  }

  test("fails closed for null, empty, source, span, symbol, and TypedSplice replacements") {
    withContext {
      val view = viewFor("class Boundary:\n  def change(x: Int): Int = x\n", 0)

      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, null),
        "REPLACEMENT_BODY_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, untpd.EmptyTree),
        "REPLACEMENT_BODY_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, parseTerm("sourceValue")),
        "REPLACEMENT_SOURCE_PROVENANCE"
      )

      given SourceFile = NoSource
      val spanned = untpd.Number("1", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0))
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, spanned),
        "REPLACEMENT_SPAN_PROVENANCE"
      )
      val symbol = newSymbol(NoSymbol, termName("u029Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("value")).withType(symbol.termRef)
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, symbolBearing),
        "REPLACEMENT_SYMBOL_PROVENANCE"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(
          view,
          untpd.TypedSplice(symbolBearing)
        ),
        "REPLACEMENT_TYPED_SPLICE_UNSUPPORTED"
      )
    }
  }

  test("returns a structured error for malformed replacement descendants") {
    withContext {
      val view = viewFor("class Malformed:\n  def change(x: Int): Int = x\n", 0)
      given SourceFile = NoSource
      val function = untpd.Ident(termName("f"))
      val leaf = untpd.Literal(Constant(1))
      val malformed = List[untpd.Tree](
        untpd.Apply(null.asInstanceOf[untpd.Tree], leaf :: Nil),
        untpd.Apply(
          untpd.Select(null.asInstanceOf[untpd.Tree], termName("f")),
          leaf :: Nil
        ),
        untpd.Apply(function, null.asInstanceOf[untpd.Tree] :: Nil),
        untpd.Apply(function, null.asInstanceOf[List[untpd.Tree]]),
        untpd.Apply(
          function,
          untpd.Apply(function, null.asInstanceOf[List[untpd.Tree]]) :: Nil
        )
      )

      malformed.foreach(replacement =>
        assertCode(
          ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, replacement),
          "REPLACEMENT_GRAPH_MALFORMED"
        )
      )
    }
  }

  test("fails closed for forged views, malformed captures, and reused target identity") {
    withContext {
      val view = viewFor(
        """class Captured:
          |  def before(x: Int): Int = x
          |  def change(x: Int): Int = x + 1
          |""".stripMargin,
        1
      )
      given SourceFile = NoSource
      val replacement = untpd.Number("7", untpd.NumberKind.Whole(10))

      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(null, replacement),
        "VIEW_REQUIRED"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(
          view.copy(rhs = view.parameterType),
          replacement
        ),
        "VIEW_INVALID"
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(
          view.copy(captured = view.captured.copy(members = view.captured.members.reverse)),
          replacement
        ),
        "VIEW_INVALID"
      )

      val duplicateTemplate = untpd.cpy.Template(view.captured.originalTemplate)(
        view.captured.originalTemplate.constr,
        view.captured.originalTemplate.parentsOrDerived,
        view.captured.originalTemplate.derived,
        view.captured.originalTemplate.self,
        List(view.method, view.method)
      )
      val duplicateRoot = untpd.cpy.TypeDef(view.captured.originalRoot)(
        view.captured.originalRoot.name,
        duplicateTemplate
      )
      val duplicateCapture = ExistingUntpdClassMemberFilter.Capture(
        duplicateRoot,
        duplicateTemplate,
        Vector(
          ExistingUntpdClassMemberFilter.Member(0, view.method),
          ExistingUntpdClassMemberFilter.Member(1, view.method)
        )
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(
          view.copy(captured = duplicateCapture, memberIndex = 0),
          replacement
        ),
        "TARGET_IDENTITY_NOT_UNIQUE"
      )
    }
  }

  test("rejects replacement topology outside the inherited U003 U005 and U013 families") {
    withContext {
      val view = viewFor("class Family:\n  def change(x: Int): Int = x\n", 0)
      given SourceFile = NoSource
      val block = untpd.Block(
        untpd.Literal(Constant(1)) :: Nil,
        untpd.Literal(Constant(2))
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, block),
        "REPLACEMENT_FAMILY_UNSUPPORTED"
      )

      val tooManyArguments = untpd.Apply(
        untpd.Ident(termName("combine")),
        List(1, 2, 3, 4).map(value => untpd.Literal(Constant(value)))
      )
      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.rewrite(view, tooManyArguments),
        "APPLY_ARGUMENT_COUNT_REQUIRED"
      )
    }
  }

  test("rejects a final reconstruction that loses the exact captured parameter identity") {
    withContext {
      val view = viewFor("class FinalCheck:\n  def change(x: Int): Int = x\n", 0)
      given SourceFile = NoSource
      val replacement = untpd.Literal(Constant(8))
      val result = ExistingUntpdSingleParameterMethodRhsRewriter
        .rewrite(view, replacement)
        .fold(problem => fail(problem.message), identity)
      val clonedParameter = untpd.cpy.ValDef(view.parameter)(
        view.parameter.name,
        view.parameter.tpt,
        view.parameter.rhs
      )
      val forgedTarget = untpd.cpy.DefDef(result.positionedResult.positionedTarget)(
        result.positionedResult.positionedTarget.name,
        List(List(clonedParameter)),
        result.positionedResult.positionedTarget.tpt,
        result.positionedResult.positionedTarget.rhs
      )

      assertCode(
        ExistingUntpdSingleParameterMethodRhsRewriter.validateResult(
          view,
          result.replacementFamily,
          result.structuralResult,
          result.positionedResult.copy(positionedTarget = forgedTarget)
        ),
        "FINAL_REWRITE_INVARIANT_FAILED"
      )
    }
  }

  test("retains structured legacy diagnostics for malformed origin carriers") {
    withContext {
      val view = viewFor("class Carrier:\n  def change(x: Int): Int = x\n", 0)
      given SourceFile = NoSource
      val leaf = untpd.Literal(Constant(1))
      val directApply = untpd.Apply(untpd.Ident(termName("f")), leaf :: Nil)
      val selectedApply = untpd.Apply(
        untpd.Select(untpd.Ident(termName("service")), termName("f")),
        leaf :: Nil
      )
      val singleStructural = ExistingUntpdMethodBodyRewriter
        .rewriteSingleParameter(view, leaf)
        .fold(problem => fail(problem.message), identity)
      val directStructural = ExistingUntpdMethodBodyRewriter
        .rewriteSingleParameter(view, directApply)
        .fold(problem => fail(problem.message), identity)
      val selectedStructural = ExistingUntpdMethodBodyRewriter
        .rewriteSingleParameter(view, selectedApply)
        .fold(problem => fail(problem.message), identity)

      assertOriginCode(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adapt(
          singleStructural.copy(rebuiltRoot = null)
        ),
        "ORIGIN_ADAPTATION_FAILED"
      )
      assertOriginCode(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptApply(
          directStructural.copy(rebuiltTemplate = null)
        ),
        "ORIGIN_ADAPTATION_FAILED"
      )
      assertOriginCode(
        ExistingUntpdMethodBodyRewriteOriginAdapter.adaptSelectedApply(
          selectedStructural.copy(rebuiltTarget = null)
        ),
        "ORIGIN_ADAPTATION_FAILED"
      )
    }
  }

  private def assertUniformReplacementSite(
      result: ExistingUntpdSingleParameterMethodRhsRewriter.Result
  )(using Context): Unit =
    val structuralNodes =
      ExistingUntpdClassMemberFilter.allTrees(result.structuralResult.replacementBody)
    val positionedNodes =
      ExistingUntpdClassMemberFilter.allTrees(result.positionedResult.positionedReplacement)
    assertEquals(structuralNodes.size, positionedNodes.size)
    structuralNodes.foreach { tree =>
      assert(!tree.source.exists)
      assert(!tree.span.exists)
    }
    positionedNodes.foreach { tree =>
      assertEquals(tree.source, result.view.rhs.source)
      assertEquals(tree.span, result.view.rhs.span)
    }

  private def viewFor(
      source: String,
      memberIndex: Int
  )(using Context): ExistingUntpdSingleParameterMethodView.View =
    val root = parseClass(source)
    val captured = ExistingUntpdClassMemberFilter
      .capture(root)
      .fold(problem => fail(problem.message), identity)
    ExistingUntpdSingleParameterMethodView
      .capture(captured, memberIndex)
      .fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U029RhsRewrite.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outer: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U029Replacement.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed

  private def assertCode[A](
      result: Either[ExistingUntpdSingleParameterMethodRhsRewriteError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def assertOriginCode[A](
      result: Either[ExistingUntpdMethodBodyRewriteOriginError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    body(using base.initialCtx)
