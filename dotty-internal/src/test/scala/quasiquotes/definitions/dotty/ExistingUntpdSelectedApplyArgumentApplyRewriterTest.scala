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

class ExistingUntpdSelectedApplyArgumentApplyRewriterTest extends munit.FunSuite:
  test("replaces one exact existing argument with one bounded child-bearing Apply") {
    withContext {
      val root = parseClass(
        "class U015Structural:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val originalApply = target.rhs.asInstanceOf[untpd.Apply]
      val exactArgument = originalApply.args.head
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Ident(termName("helper")),
        untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
      )

      val result = ExistingUntpdSelectedApplyArgumentApplyRewriter
        .rewrite(root, target, exactArgument, replacement)
        .fold(problem => fail(problem.message), identity)

      assert(result.rebuiltApply.args.head.eq(replacement))
      assert(result.rebuiltApply.args(1).eq(originalApply.args(1)))
      assert(result.rebuiltApply.fun.eq(originalApply.fun))
      val replacementNodes = replacement +: replacement.fun +: replacement.args.toVector
      replacementNodes.foreach { node =>
        assert(!node.source.exists)
        assert(!node.span.exists)
        assertEquals(node.symbol, NoSymbol)
      }
    }
  }

  test("selects exact arguments at indexes zero, one, and the upper third boundary") {
    withContext {
      val root = parseClass(
        "class U015Indexes:\n  def change: Int = service.invoke(first, second, third)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      outer.args.indices.foreach { index =>
        given SourceFile = NoSource
        val replacement = untpd.Apply(
          untpd.Ident(termName("helper")),
          List(untpd.Ident(termName("x")), untpd.Number("20", untpd.NumberKind.Whole(10)))
        )
        val result = ExistingUntpdSelectedApplyArgumentApplyRewriter
          .rewrite(root, target, outer.args(index), replacement)
          .fold(problem => fail(problem.message), identity)
        assertEquals(result.argumentIndex, index)
        assert(result.rebuiltApply.args(index).eq(replacement))
        outer.args.indices.filterNot(_ == index).foreach { untouched =>
          assert(result.rebuiltApply.args(untouched).eq(outer.args(untouched)))
        }
      }
    }
  }

  test("keeps U014 leaf-only and fails closed for replacement topology and provenance") {
    withContext {
      val root = parseClass(
        "class U015Failures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val argument = target.rhs.asInstanceOf[untpd.Apply].args.head
      given SourceFile = NoSource
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))
      val valid = untpd.Apply(untpd.Ident(termName("helper")), leaf :: Nil)

      val u014 = ExistingUntpdSelectedApplyArgumentRewriter.rewrite(root, target, argument, valid)
      assertEquals(u014.left.map(_.code), Left("REPLACEMENT_LEAF_REQUIRED"))
      assertError("REPLACEMENT_REQUIRED")(rewrite(root, target, argument, null))
      assertError("REPLACEMENT_APPLY_REQUIRED")(rewrite(root, target, argument, leaf))
      assertError("REPLACEMENT_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Select(untpd.Ident(termName("svc")), termName("helper")), leaf :: Nil))
      )
      assertError("REPLACEMENT_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Apply(untpd.Ident(termName("f")), leaf :: Nil), leaf :: Nil))
      )
      List(Nil, List.fill(4)(leaf)).foreach { arguments =>
        assertError("REPLACEMENT_ARGUMENT_COUNT_REQUIRED")(
          rewrite(root, target, argument, untpd.Apply(untpd.Ident(termName("helper")), arguments))
        )
      }
      assertError("REPLACEMENT_ARGUMENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Ident(termName("helper")), null.asInstanceOf[untpd.Tree] :: Nil))
      )
      assertError("REPLACEMENT_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Ident(termName("helper")), valid :: Nil))
      )
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        rewrite(root, target, argument,
          untpd.Apply(parseTerm("helper"), leaf :: Nil))
      )
      assertError("REPLACEMENT_SPAN_PROVENANCE")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Ident(termName("helper")), leaf.withSpan(Span(0, 1, 0)) :: Nil))
      )
      val symbol = newSymbol(NoSymbol, termName("u015Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertError("REPLACEMENT_SYMBOL_PROVENANCE")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Ident(termName("helper")), symbolBearing :: Nil))
      )
      assertError("REPLACEMENT_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Ident(termName("helper")), untpd.TypedSplice(symbolBearing) :: Nil))
      )
    }
  }

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      argument: untpd.Tree,
      replacement: untpd.Tree
  )(using Context) =
    ExistingUntpdSelectedApplyArgumentApplyRewriter.rewrite(root, target, argument, replacement)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U015Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U015Replacement.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentApplyRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
