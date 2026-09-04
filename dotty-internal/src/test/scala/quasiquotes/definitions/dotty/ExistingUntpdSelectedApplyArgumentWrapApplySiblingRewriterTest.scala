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

class ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriterTest
    extends munit.FunSuite:
  test("preserves the exact selected child and installs one exact fresh child-bearing sibling") {
    withFixture { (root, target, outer, original) =>
      given SourceFile = NoSource
      val wrapperFunction = untpd.Ident(termName("helper"))
      val siblingFunction = untpd.Ident(termName("product"))
      val siblingArguments = List[untpd.Tree](
        untpd.Ident(termName("freshValue")),
        untpd.Number("4", untpd.NumberKind.Whole(10)),
        untpd.Literal(dotty.tools.dotc.core.Constants.Constant(5))
      )
      val siblingApply = untpd.Apply(siblingFunction, siblingArguments)

      val result = ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
        .rewrite(root, target, original, wrapperFunction, siblingApply)
        .fold(problem => fail(problem.message), identity)

      assertEquals(result.wrapperApply.args.size, 2)
      assert(result.wrapperApply.args(0).eq(original))
      assert(result.wrapperApply.args(1).eq(siblingApply))
      assert(result.wrapperApply.fun.eq(wrapperFunction))
      assert(result.freshSiblingApply.eq(siblingApply))
      assert(result.freshSiblingFunction.eq(siblingFunction))
      assertEquals(result.freshSiblingArguments.size, 3)
      siblingArguments.indices.foreach(index =>
        assert(result.freshSiblingArguments(index).eq(siblingArguments(index)))
      )
      assert(result.rebuiltApply.args.head.eq(result.wrapperApply))
      assert(result.rebuiltApply.args(1).eq(outer.args(1)))
      assert(result.rebuiltApply.fun.eq(outer.fun))
      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget,
        result.rebuiltApply,
        result.wrapperApply,
        wrapperFunction,
        siblingApply,
        siblingFunction
      ).++(siblingArguments).foreach(assertDetached)
      assert(original.source.exists)
      assert(original.span.exists)
    }
  }

  test("accepts one to three Ident Number Literal sibling arguments in exact order") {
    withFixture { (root, target, _, original) =>
      given SourceFile = NoSource
      val cases = List(
        List[untpd.Tree](untpd.Ident(termName("freshValue"))),
        List[untpd.Tree](untpd.Number("4", untpd.NumberKind.Whole(10))),
        List[untpd.Tree](untpd.Literal(dotty.tools.dotc.core.Constants.Constant(5))),
        List[untpd.Tree](
          untpd.Ident(termName("freshValue")),
          untpd.Number("4", untpd.NumberKind.Whole(10)),
          untpd.Literal(dotty.tools.dotc.core.Constants.Constant(5))
        )
      )
      cases.foreach { arguments =>
        val function = untpd.Ident(termName("product"))
        val sibling = untpd.Apply(function, arguments)
        val result = ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
          .rewrite(root, target, original, untpd.Ident(termName("helper")), sibling)
          .fold(problem => fail(problem.message), identity)
        assert(result.wrapperApply.args(0).eq(original))
        assert(result.wrapperApply.args(1).eq(sibling))
        assert(result.freshSiblingFunction.eq(function))
        arguments.indices.foreach(index =>
          assert(result.freshSiblingArguments(index).eq(arguments(index)))
        )
      }
    }
  }

  test("fails closed for malformed sibling Apply topology and contaminated descendants") {
    withFixture { (root, target, outer, original) =>
      given SourceFile = NoSource
      val wrapper = untpd.Ident(termName("helper"))
      val function = untpd.Ident(termName("product"))
      val leaf = untpd.Number("4", untpd.NumberKind.Whole(10))
      val valid = untpd.Apply(function, leaf :: Nil)

      assertError("ROOT_REQUIRED")(rewrite(null, target, original, wrapper, valid))
      assertError("TARGET_REQUIRED")(rewrite(root, null, original, wrapper, valid))
      assertError("ARGUMENT_REQUIRED")(rewrite(root, target, null, wrapper, valid))
      assertError("WRAPPER_FUNCTION_REQUIRED")(rewrite(root, target, original, null, valid))
      assertError("FRESH_SIBLING_APPLY_REQUIRED")(
        rewrite(root, target, original, wrapper, null)
      )
      assertError("FRESH_SIBLING_APPLY_REQUIRED")(
        rewrite(root, target, original, wrapper, leaf)
      )
      assertError("FRESH_SIBLING_FUNCTION_IDENT_REQUIRED")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(untpd.Select(untpd.Ident(termName("service")), termName("product")),
            leaf :: Nil))
      )
      assertError("FRESH_SIBLING_ARGUMENT_COUNT")(
        rewrite(root, target, original, wrapper, untpd.Apply(function, Nil))
      )
      assertError("FRESH_SIBLING_ARGUMENT_COUNT")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, List.fill(4)(untpd.Number("4", untpd.NumberKind.Whole(10)))))
      )
      assertError("FRESH_SIBLING_ARGUMENT_REQUIRED")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, null.asInstanceOf[untpd.Tree] :: Nil))
      )
      assertError("FRESH_SIBLING_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, untpd.Apply(function, leaf :: Nil) :: Nil))
      )
      assertError("FRESH_SIBLING_FUNCTION_SOURCE_PROVENANCE")(
        rewrite(root, target, original, wrapper, parseTerm("product(4)"))
      )
      assertError("FRESH_SIBLING_FUNCTION_SOURCE_PROVENANCE")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(parseTerm("product"), leaf :: Nil))
      )
      assertError("FRESH_SIBLING_FUNCTION_SPAN_PROVENANCE")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(untpd.Ident(termName("product")),
            untpd.Number("4", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0)) :: Nil))
      )
      val symbol = newSymbol(NoSymbol, termName("u020Symbol"), EmptyFlags, NoType)
      val symbolLeaf = untpd.Ident(termName("symbolLeaf")).withType(symbol.termRef)
      assertError("FRESH_SIBLING_ARGUMENT_SYMBOL_PROVENANCE")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(untpd.Ident(termName("product")), symbolLeaf :: Nil))
      )
      assertError("FRESH_SIBLING_ARGUMENT_TYPED_SPLICE_UNSUPPORTED")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(untpd.Ident(termName("product")),
            untpd.TypedSplice(symbolLeaf) :: Nil))
      )
      assertError("FRESH_SIBLING_ALIASES_ORIGINAL")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, original :: Nil))
      )
      assertError("FRESH_SIBLING_ALIASES_EXISTING_ARGUMENT")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, outer.args(1) :: Nil))
      )
      assertError("FRESH_SIBLING_ALIASES_OUTER_FUNCTION")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(function, outer.fun.asInstanceOf[untpd.Select].qualifier :: Nil))
      )
      assertError("FRESH_SIBLING_ALIASES_WRAPPER_FUNCTION")(
        rewrite(root, target, original, wrapper, untpd.Apply(function, wrapper :: Nil))
      )
      assertError("FRESH_SIBLING_NODE_ALIAS")(
        rewrite(root, target, original, wrapper, untpd.Apply(function, leaf :: leaf :: Nil))
      )
    }
  }

  private def rewrite(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      argument: untpd.Tree,
      wrapper: untpd.Tree,
      sibling: untpd.Tree
  )(using Context) =
    ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
      .rewrite(root, target, argument, wrapper, sibling)

  private def assertDetached(tree: untpd.Tree)(using Context): Unit =
    assert(!tree.source.exists)
    assert(!tree.span.exists)
    assertEquals(tree.symbol, NoSymbol)
    assert(!tree.isInstanceOf[untpd.TypedSplice])

  private def withFixture(
      run: Context ?=> (untpd.TypeDef, untpd.DefDef, untpd.Apply, untpd.Tree) => Unit
  ): Unit = withContext {
    val root = parseClass(
      "class U020Structural:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
    )
    val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))
    val outer = target.rhs.asInstanceOf[untpd.Apply]
    run(root, target, outer, outer.args.head)
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U020Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U020Sibling.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
