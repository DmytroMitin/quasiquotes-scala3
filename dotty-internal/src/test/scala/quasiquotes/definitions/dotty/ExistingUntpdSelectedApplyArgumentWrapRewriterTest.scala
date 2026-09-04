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

class ExistingUntpdSelectedApplyArgumentWrapRewriterTest extends munit.FunSuite:
  test("wraps one exact existing argument while preserving that argument as the unary child") {
    withContext {
      val root = parseClass(
        "class U018Structural:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val originalApply = target.rhs.asInstanceOf[untpd.Apply]
      val exactArgument = originalApply.args.head
      given SourceFile = NoSource
      val wrapperFunction = untpd.Ident(termName("helper"))

      val result = ExistingUntpdSelectedApplyArgumentWrapRewriter
        .rewrite(root, target, exactArgument, wrapperFunction)
        .fold(problem => fail(problem.message), identity)

      assert(result.wrapperApply.fun.eq(wrapperFunction))
      assertEquals(result.wrapperApply.args.size, 1)
      assert(result.wrapperApply.args.head.eq(exactArgument))
      assert(result.rebuiltApply.args.head.eq(result.wrapperApply))
      assert(result.rebuiltApply.args(1).eq(originalApply.args(1)))
      assert(result.rebuiltApply.fun.eq(originalApply.fun))
      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget,
        result.rebuiltApply,
        result.wrapperApply,
        wrapperFunction
      ).foreach { node =>
        assert(!node.source.exists)
        assert(!node.span.exists)
        assertEquals(node.symbol, NoSymbol)
        assert(!node.isInstanceOf[untpd.TypedSplice])
      }
      assert(exactArgument.source.exists)
      assert(exactArgument.span.exists)
    }
  }

  test("selects exact existing arguments at every admitted outer index") {
    withContext {
      val root = parseClass(
        "class U018Indexes:\n  def change: Int = service.invoke(first, second, third)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]

      outer.args.indices.foreach { index =>
        given SourceFile = NoSource
        val wrapperFunction = untpd.Ident(termName("helper"))
        val result = ExistingUntpdSelectedApplyArgumentWrapRewriter
          .rewrite(root, target, outer.args(index), wrapperFunction)
          .fold(problem => fail(problem.message), identity)
        assertEquals(result.argumentIndex, index)
        assert(result.wrapperApply.args.head.eq(outer.args(index)))
        outer.args.indices.filterNot(_ == index).foreach { untouched =>
          assert(result.rebuiltApply.args(untouched).eq(outer.args(untouched)))
        }
      }
    }
  }

  test("fails closed for invalid wrapper functions and retains U014 envelope errors") {
    withContext {
      val root = parseClass(
        "class U018Failures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val argument = target.rhs.asInstanceOf[untpd.Apply].args.head
      given SourceFile = NoSource
      val wrapper = untpd.Ident(termName("helper"))
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))

      assertError("ROOT_REQUIRED")(rewrite(null, target, argument, wrapper))
      assertError("TARGET_REQUIRED")(rewrite(root, null, argument, wrapper))
      assertError("ARGUMENT_REQUIRED")(rewrite(root, target, null, wrapper))
      assertError("WRAPPER_FUNCTION_REQUIRED")(rewrite(root, target, argument, null))
      assertError("WRAPPER_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Select(untpd.Ident(termName("service")), termName("helper")))
      )
      assertError("WRAPPER_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, argument, untpd.Apply(wrapper, leaf :: Nil))
      )
      assertError("WRAPPER_FUNCTION_SOURCE_PROVENANCE")(
        rewrite(root, target, argument, parseTerm("helper"))
      )
      assertError("WRAPPER_FUNCTION_SPAN_PROVENANCE")(
        rewrite(root, target, argument, wrapper.withSpan(Span(0, 1, 0)))
      )
      val symbol = newSymbol(NoSymbol, termName("u018Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertError("WRAPPER_FUNCTION_SYMBOL_PROVENANCE")(
        rewrite(root, target, argument, symbolBearing)
      )
      assertError("WRAPPER_FUNCTION_TYPED_SPLICE_UNSUPPORTED")(
        rewrite(root, target, argument, untpd.TypedSplice(symbolBearing))
      )
    }
  }

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      argument: untpd.Tree,
      wrapperFunction: untpd.Tree
  )(using Context) =
    ExistingUntpdSelectedApplyArgumentWrapRewriter
      .rewrite(root, target, argument, wrapperFunction)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U018Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U018Wrapper.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
