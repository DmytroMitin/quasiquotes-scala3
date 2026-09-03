package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdSelectedApplyArgumentSelectedApplyRewriterTest extends munit.FunSuite:
  test("replaces one exact existing argument with one bounded selected-member Apply") {
    withContext {
      val (root, target, outer) = fixture("oldArg, keptArg")
      val exactArgument = outer.args.head
      given SourceFile = NoSource
      val replacement = selectedReplacement(List(untpd.Number("20", untpd.NumberKind.Whole(10))))

      val result = rewrite(root, target, exactArgument, replacement)
        .fold(problem => fail(problem.message), identity)

      assert(result.rebuiltApply.args.head.eq(replacement))
      assert(result.rebuiltApply.args(1).eq(outer.args(1)))
      assert(result.rebuiltApply.fun.eq(outer.fun))
      val selection = replacement.fun.asInstanceOf[untpd.Select]
      val nodes = replacement +: selection +: selection.qualifier +: replacement.args.toVector
      nodes.foreach { node =>
        assert(!node.source.exists)
        assert(!node.span.exists)
        assertEquals(node.symbol, NoSymbol)
      }
    }
  }

  test("selects exact arguments at indexes zero, one, and the upper third boundary") {
    withContext {
      val (root, target, outer) = fixture("first, second, third")
      outer.args.indices.foreach { index =>
        given SourceFile = NoSource
        val replacement = selectedReplacement(
          List(untpd.Ident(termName("x")), untpd.Number("20", untpd.NumberKind.Whole(10)))
        )
        val result = rewrite(root, target, outer.args(index), replacement)
          .fold(problem => fail(problem.message), identity)
        assertEquals(result.argumentIndex, index)
        assert(result.rebuiltApply.args(index).eq(replacement))
        outer.args.indices.filterNot(_ == index).foreach { untouched =>
          assert(result.rebuiltApply.args(untouched).eq(outer.args(untouched)))
        }
      }
    }
  }

  test("keeps U014 and U015 narrow and fails closed for topology and provenance") {
    withContext {
      val (root, target, outer) = fixture("oldArg, keptArg")
      val argument = outer.args.head
      given SourceFile = NoSource
      val leaf = untpd.Number("20", untpd.NumberKind.Whole(10))
      val valid = selectedReplacement(leaf :: Nil)

      assertEquals(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(root, target, argument, valid).left.map(_.code),
        Left("REPLACEMENT_LEAF_REQUIRED")
      )
      assertEquals(
        ExistingUntpdSelectedApplyArgumentApplyRewriter.rewrite(root, target, argument, valid).left.map(_.code),
        Left("REPLACEMENT_FUNCTION_IDENT_REQUIRED")
      )
      assertError("REPLACEMENT_REQUIRED")(rewrite(root, target, argument, null))
      assertError("REPLACEMENT_APPLY_REQUIRED")(rewrite(root, target, argument, leaf))
      assertError("REPLACEMENT_FUNCTION_SELECT_REQUIRED")(
        rewrite(root, target, argument, untpd.Apply(untpd.Ident(termName("helper")), leaf :: Nil))
      )
      assertError("REPLACEMENT_QUALIFIER_IDENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(
            untpd.Select(
              untpd.Select(untpd.Ident(termName("pkg")), termName("helper")),
              termName("make")
            ),
            leaf :: Nil
          ))
      )
      assertError("REPLACEMENT_QUALIFIER_IDENT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(
            untpd.Select(untpd.Apply(untpd.Ident(termName("helper")), leaf :: Nil), termName("make")),
            leaf :: Nil
          ))
      )
      assertError("REPLACEMENT_FUNCTION_SELECT_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.TypeApply(valid.fun, untpd.TypeTree() :: Nil), leaf :: Nil))
      )
      assertError("REPLACEMENT_MEMBER_TERM_NAME_REQUIRED")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Select(untpd.Ident(termName("helper")), typeName("Make")), leaf :: Nil))
      )
      List(Nil, List.fill(4)(leaf)).foreach { arguments =>
        assertError("REPLACEMENT_ARGUMENT_COUNT_REQUIRED")(
          rewrite(root, target, argument,
            untpd.Apply(untpd.Select(untpd.Ident(termName("helper")), termName("make")), arguments))
        )
      }
      assertError("REPLACEMENT_ARGUMENT_REQUIRED")(
        rewrite(root, target, argument,
          selectedReplacement(null.asInstanceOf[untpd.Tree] :: Nil))
      )
      assertError("REPLACEMENT_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, argument, selectedReplacement(valid :: Nil))
      )
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        rewrite(root, target, argument, untpd.Apply(parseTerm("helper.make"), leaf :: Nil))
      )
      assertError("REPLACEMENT_SPAN_PROVENANCE")(
        rewrite(root, target, argument,
          selectedReplacement(leaf.withSpan(Span(0, 1, 0)) :: Nil))
      )
      val symbol = newSymbol(NoSymbol, termName("u016Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("helper")).withType(symbol.termRef)
      val freshLeaf = untpd.Number("20", untpd.NumberKind.Whole(10))
      assertError("REPLACEMENT_SYMBOL_PROVENANCE")(
        rewrite(root, target, argument,
          untpd.Apply(untpd.Select(symbolBearing, termName("make")), freshLeaf :: Nil))
      )
      assertError("REPLACEMENT_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, argument, selectedReplacement(untpd.TypedSplice(symbolBearing) :: Nil))
      )
    }
  }

  private def selectedReplacement(arguments: List[untpd.Tree])(using SourceFile): untpd.Apply =
    untpd.Apply(
      untpd.Select(untpd.Ident(termName("helper")), termName("make")),
      arguments
    )

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      argument: untpd.Tree,
      replacement: untpd.Tree
  )(using Context) =
    ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter
      .rewrite(root, target, argument, replacement)

  private def fixture(arguments: String)(using Context): (untpd.TypeDef, untpd.DefDef, untpd.Apply) =
    val root = parseClass(
      s"class U016Structural:\n  def change: Int = service.invoke($arguments)\n"
    )
    val template = root.rhs.asInstanceOf[untpd.Template]
    val target = template.body.collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))
    (root, target, target.rhs.asInstanceOf[untpd.Apply])

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U016Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U016Replacement.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
