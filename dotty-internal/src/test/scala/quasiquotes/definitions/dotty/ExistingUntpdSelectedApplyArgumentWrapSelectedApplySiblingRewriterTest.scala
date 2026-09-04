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

class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriterTest
    extends munit.FunSuite:
  test("preserves the exact old child and installs one exact selected-member fresh sibling") {
    withContext {
      val root = parseClass(
        "class U021Structural:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      given SourceFile = NoSource
      val wrapper = untpd.Ident(termName("helper"))
      val qualifier = untpd.Ident(termName("catalog"))
      val selection = untpd.Select(qualifier, termName("product"))
      val leaves = List[untpd.Tree](
        untpd.Ident(termName("freshValue")),
        untpd.Number("4", untpd.NumberKind.Whole(10)),
        untpd.Literal(dotty.tools.dotc.core.Constants.Constant(true))
      )
      val sibling = untpd.Apply(selection, leaves)

      val result = ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter
        .rewrite(root, target, original, wrapper, sibling)
        .fold(problem => fail(problem.message), identity)

      assert(result.wrapperApply.args(0).eq(original))
      assert(result.wrapperApply.args(1).eq(sibling))
      assert(result.freshSiblingApply.eq(sibling))
      assert(result.freshSiblingSelection.eq(selection))
      assert(result.freshSiblingQualifier.eq(qualifier))
      assertEquals(result.freshSiblingMemberName, termName("product"))
      leaves.indices.foreach(index =>
        assert(result.freshSiblingArguments(index).eq(leaves(index)))
      )
      assert(result.rebuiltApply.args.head.eq(result.wrapperApply))
      assert(result.rebuiltApply.args(1).eq(outer.args(1)))
    }
  }

  test("fails closed for malformed selected-member topology, provenance, and aliases") {
    withContext {
      val root = parseClass(
        "class U021Failures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      given SourceFile = NoSource
      val wrapper = untpd.Ident(termName("helper"))
      val qualifier = untpd.Ident(termName("catalog"))
      val leaf = untpd.Number("4", untpd.NumberKind.Whole(10))
      def selected(arguments: List[untpd.Tree],
          selectedQualifier: untpd.Tree = untpd.Ident(termName("catalog")),
          member: dotty.tools.dotc.core.Names.Name = termName("product")): untpd.Apply =
        untpd.Apply(untpd.Select(selectedQualifier, member), arguments)

      assertError("FRESH_SIBLING_APPLY_REQUIRED")(
        rewrite(root, target, original, wrapper, null))
      assertError("FRESH_SIBLING_SELECTION_REQUIRED")(
        rewrite(root, target, original, wrapper,
          untpd.Apply(untpd.Ident(termName("product")), leaf :: Nil)))
      assertError("FRESH_SIBLING_QUALIFIER_IDENT_REQUIRED")(
        rewrite(root, target, original, wrapper,
          selected(leaf :: Nil, untpd.Select(qualifier, termName("nested")))))
      assertError("FRESH_SIBLING_TERM_MEMBER_REQUIRED")(
        rewrite(root, target, original, wrapper, selected(leaf :: Nil, member = typeName("Product"))))
      assertError("FRESH_SIBLING_ARGUMENT_COUNT")(
        rewrite(root, target, original, wrapper, selected(Nil)))
      assertError("FRESH_SIBLING_ARGUMENT_COUNT")(
        rewrite(root, target, original, wrapper, selected(List.fill(4)(
          untpd.Number("4", untpd.NumberKind.Whole(10))))))
      assertError("FRESH_SIBLING_ARGUMENT_REQUIRED")(
        rewrite(root, target, original, wrapper,
          selected(null.asInstanceOf[untpd.Tree] :: Nil)))
      assertError("FRESH_SIBLING_ARGUMENT_LEAF_REQUIRED")(
        rewrite(root, target, original, wrapper,
          selected(untpd.Apply(untpd.Ident(termName("nested")), leaf :: Nil) :: Nil)))
      assertError("FRESH_SIBLING_APPLY_SPAN_PROVENANCE")(
        rewrite(root, target, original, wrapper,
          selected(leaf :: Nil, qualifier.withSpan(Span(0, 1, 0)))))
      val symbol = newSymbol(NoSymbol, termName("u021Symbol"), EmptyFlags, NoType)
      val symbolLeaf = untpd.Ident(termName("symbolLeaf")).withType(symbol.termRef)
      assertError("FRESH_SIBLING_ARGUMENT_SYMBOL_PROVENANCE")(
        rewrite(root, target, original, wrapper, selected(symbolLeaf :: Nil)))
      assertError("FRESH_SIBLING_ARGUMENT_TYPED_SPLICE_UNSUPPORTED")(
        rewrite(root, target, original, wrapper,
          selected(untpd.TypedSplice(symbolLeaf) :: Nil)))
      assertError("FRESH_SIBLING_ALIASES_ORIGINAL")(
        rewrite(root, target, original, wrapper, selected(original :: Nil)))
      assertError("FRESH_SIBLING_ALIASES_EXISTING_ARGUMENT")(
        rewrite(root, target, original, wrapper, selected(outer.args(1) :: Nil)))
      assertError("FRESH_SIBLING_ALIASES_OUTER_FUNCTION")(
        rewrite(root, target, original, wrapper,
          selected(outer.fun.asInstanceOf[untpd.Select].qualifier :: Nil)))
      assertError("FRESH_SIBLING_ALIASES_WRAPPER_FUNCTION")(
        rewrite(root, target, original, wrapper, selected(wrapper :: Nil)))
      assertError("FRESH_SIBLING_NODE_ALIAS")(
        rewrite(root, target, original, wrapper, selected(leaf :: leaf :: Nil)))
    }
  }

  private def rewrite(root: untpd.TypeDef, target: untpd.DefDef, argument: untpd.Tree,
      wrapper: untpd.Tree, sibling: untpd.Tree)(using Context) =
    ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter
      .rewrite(root, target, argument, wrapper, sibling)

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U021Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
