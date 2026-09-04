package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("T7 positions wrapper, selected sibling, qualifier, and leaves at the old argument site") {
    withContext {
      val root = parseClass(
        "class U021Origin:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      val originalState = (original.source, original.span, original.symbol)
      given SourceFile = NoSource
      val selection = untpd.Select(untpd.Ident(termName("catalog")), termName("product"))
      val sibling = untpd.Apply(selection, List[untpd.Tree](
        untpd.Number("4", untpd.NumberKind.Whole(10)),
        untpd.Literal(dotty.tools.dotc.core.Constants.Constant(true))))
      val structural = ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter
        .rewrite(root, target, original, untpd.Ident(termName("helper")), sibling)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter
        .adapt(structural).fold(problem => fail(problem.message), identity)

      val positioned = Vector[untpd.Tree](adapted.positionedWrapperApply,
        adapted.positionedWrapperFunction, adapted.positionedFreshSiblingApply,
        adapted.positionedFreshSiblingSelection, adapted.positionedFreshSiblingQualifier) ++
        adapted.positionedFreshSiblingArguments
      positioned.foreach { node =>
        assertEquals(node.source, original.source)
        assertEquals(node.span, original.span)
        assertEquals(node.symbol, NoSymbol)
      }
      assert(adapted.positionedWrapperApply.args(0).eq(original))
      assert(adapted.positionedWrapperApply.args(1).eq(adapted.positionedFreshSiblingApply))
      assert(adapted.positionedFreshSiblingApply.fun.eq(adapted.positionedFreshSiblingSelection))
      assert(adapted.positionedFreshSiblingSelection.qualifier
        .eq(adapted.positionedFreshSiblingQualifier))
      assertEquals(adapted.positionedFreshSiblingSelection.name, termName("product"))
      assertEquals((original.source, original.span, original.symbol), originalState)
    }
  }

  test("rejects null and forged structural carriers") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.adapt(null))
      val root = parseClass(
        "class U021OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val sibling = untpd.Apply(
        untpd.Select(untpd.Ident(termName("catalog")), termName("product")),
        untpd.Number("4", untpd.NumberKind.Whole(10)) :: Nil)
      val structural = ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriter
        .rewrite(root, target, outer.args.head, untpd.Ident(termName("helper")), sibling)
        .fold(problem => fail(problem.message), identity)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.adapt(
          structural.copy(suppliedFreshSiblingMemberName = termName("forged"))))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.adapt(
          structural.copy(validatedExisting = structural.validatedExisting.copy(argumentIndex = 1))))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])))
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U021Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(result: Either[
      ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError, ?]): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
