package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdSelectedApplyArgumentWrapSiblingRewriterTest extends munit.FunSuite:
  test("preserves the exact selected child and inserts one exact fresh sibling") {
    withContext {
      val root = parseClass(
        "class U019Structural:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      given SourceFile = NoSource
      val function = untpd.Ident(termName("helper"))
      val sibling = untpd.Number("20", untpd.NumberKind.Whole(10))

      val result = ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
        .rewrite(root, target, original, function, sibling)
        .fold(problem => fail(problem.message), identity)

      assertEquals(result.wrapperApply.args.size, 2)
      assert(result.wrapperApply.args(0).eq(original))
      assert(result.wrapperApply.args(1).eq(sibling))
      assert(result.wrapperApply.fun.eq(function))
      assert(result.rebuiltApply.args.head.eq(result.wrapperApply))
      assert(result.rebuiltApply.args(1).eq(outer.args(1)))
      assert(result.rebuiltApply.fun.eq(outer.fun))
      assert(!sibling.eq(original))
      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget,
        result.rebuiltApply,
        result.wrapperApply,
        function,
        sibling
      ).foreach { node =>
        assert(!node.source.exists)
        assert(!node.span.exists)
        assertEquals(node.symbol, NoSymbol)
        assert(!node.isInstanceOf[untpd.TypedSplice])
      }
      assert(original.source.exists)
      assert(original.span.exists)
    }
  }

  test("accepts each fresh sibling leaf kind") {
    withContext {
      val root = parseClass(
        "class U019Kinds:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val original = target.rhs.asInstanceOf[untpd.Apply].args.head
      given SourceFile = NoSource
      val siblings = List[untpd.Tree](
        untpd.Ident(termName("freshValue")),
        untpd.Number("20", untpd.NumberKind.Whole(10)),
        untpd.Literal(dotty.tools.dotc.core.Constants.Constant(20))
      )
      siblings.foreach { sibling =>
        val result = ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
          .rewrite(root, target, original, untpd.Ident(termName("helper")), sibling)
          .fold(problem => fail(problem.message), identity)
        assert(result.wrapperApply.args(0).eq(original))
        assert(result.wrapperApply.args(1).eq(sibling))
      }
    }
  }

  test("fails closed for invalid wrapper functions and fresh siblings") {
    withContext {
      val root = parseClass(
        "class U019Failures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val original = target.rhs.asInstanceOf[untpd.Apply].args.head
      given SourceFile = NoSource
      val function = untpd.Ident(termName("helper"))
      val sibling = untpd.Number("20", untpd.NumberKind.Whole(10))

      assertError("ROOT_REQUIRED")(rewrite(null, target, original, function, sibling))
      assertError("TARGET_REQUIRED")(rewrite(root, null, original, function, sibling))
      assertError("ARGUMENT_REQUIRED")(rewrite(root, target, null, function, sibling))
      assertError("WRAPPER_FUNCTION_REQUIRED")(
        rewrite(root, target, original, null, sibling)
      )
      assertError("WRAPPER_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, original,
          untpd.Select(untpd.Ident(termName("service")), termName("helper")), sibling)
      )
      assertError("WRAPPER_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, original, untpd.Apply(function, sibling :: Nil), sibling)
      )
      assertError("WRAPPER_FUNCTION_SOURCE_PROVENANCE")(
        rewrite(root, target, original, parseTerm("helper"), sibling)
      )
      assertError("WRAPPER_FUNCTION_SPAN_PROVENANCE")(
        rewrite(root, target, original,
          untpd.Ident(termName("helper")).withSpan(Span(0, 1, 0)), sibling)
      )
      val symbol = newSymbol(NoSymbol, termName("u019Symbol"), EmptyFlags, NoType)
      val symbolBearingFunction = untpd.Ident(termName("symbolBearingFunction"))
        .withType(symbol.termRef)
      assertError("WRAPPER_FUNCTION_SYMBOL_PROVENANCE")(
        rewrite(root, target, original, symbolBearingFunction, sibling)
      )
      assertError("WRAPPER_FUNCTION_TYPED_SPLICE_UNSUPPORTED")(
        rewrite(root, target, original, untpd.TypedSplice(symbolBearingFunction), sibling)
      )
      assertError("FRESH_SIBLING_REQUIRED")(rewrite(root, target, original, function, null))
      assertError("FRESH_SIBLING_LEAF_REQUIRED")(
        rewrite(root, target, original, function, untpd.Apply(function, sibling :: Nil))
      )
      assertError("FRESH_SIBLING_SOURCE_PROVENANCE")(
        rewrite(root, target, original, function, parseTerm("20"))
      )
      assertError("FRESH_SIBLING_SPAN_PROVENANCE")(
        rewrite(root, target, original, function,
          untpd.Number("20", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0)))
      )
      val symbolBearingSibling = untpd.Ident(termName("symbolBearingSibling"))
        .withType(symbol.termRef)
      assertError("FRESH_SIBLING_SYMBOL_PROVENANCE")(
        rewrite(root, target, original, function, symbolBearingSibling)
      )
      assertError("FRESH_SIBLING_TYPED_SPLICE_UNSUPPORTED")(
        rewrite(root, target, original, function, untpd.TypedSplice(symbolBearingSibling))
      )
      assertError("FRESH_SIBLING_ALIASES_ORIGINAL")(
        rewrite(root, target, original, function, original)
      )
      assertError("FRESH_SIBLING_ALIASES_EXISTING_ARGUMENT")(
        rewrite(root, target, original, function,
          target.rhs.asInstanceOf[untpd.Apply].args(1))
      )
    }
  }

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      argument: untpd.Tree,
      function: untpd.Tree,
      sibling: untpd.Tree
  )(using Context) =
    ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
      .rewrite(root, target, argument, function, sibling)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U019Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U019Sibling.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
