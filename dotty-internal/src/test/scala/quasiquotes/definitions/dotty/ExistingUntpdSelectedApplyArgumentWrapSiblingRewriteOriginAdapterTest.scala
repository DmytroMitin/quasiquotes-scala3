package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("S4 positions every fresh wrapper node at the selected argument site") {
    withContext {
      val root = parseClass(
        "class U019Origin:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      val originalState = (original.source, original.span, original.symbol)
      given SourceFile = NoSource
      val function = untpd.Ident(termName("helper"))
      val sibling = untpd.Number("20", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
        .rewrite(root, target, original, function, sibling)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      Vector[untpd.Tree](
        adapted.positionedWrapperApply,
        adapted.positionedWrapperFunction,
        adapted.positionedFreshSibling
      ).foreach { node =>
        assertEquals(node.source, original.source)
        assertEquals(node.span, original.span)
        assertEquals(node.symbol, NoSymbol)
        assert(!node.isInstanceOf[untpd.TypedSplice])
      }
      assertEquals(adapted.positionedWrapperApply.args.size, 2)
      assert(adapted.positionedWrapperApply.args(0).eq(original))
      assert(adapted.positionedWrapperApply.args(1).eq(adapted.positionedFreshSibling))
      assert(!adapted.positionedFreshSibling.eq(sibling))
      assert(adapted.positionedApply.fun.eq(outer.fun))
      assert(adapted.positionedApply.args(1).eq(outer.args(1)))
      assertEquals((original.source, original.span, original.symbol), originalState)
      assert(structural.wrapperApply.args(0).eq(original))
      assert(structural.wrapperApply.args(1).eq(sibling))
      assert(!structural.wrapperApply.source.exists)
    }
  }

  test("rejects null carriers and forged wrapper order or identity") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(null)
      )
      val root = parseClass(
        "class U019OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val function = untpd.Ident(termName("helper"))
      val sibling = untpd.Number("20", untpd.NumberKind.Whole(10))
      val structural = ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
        .rewrite(root, target, outer.args.head, function, sibling)
        .fold(problem => fail(problem.message), identity)

      val emptyWrapper = untpd.Apply(function, Nil)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          coherentCopy(structural, emptyWrapper)
        )
      )

      val reversed = untpd.Apply(function, sibling :: outer.args.head :: Nil)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          coherentCopy(structural, reversed)
        )
      )
      val clonedOriginal = outer.args.head.cloneIn(outer.args.head.source)
        .withSpan(outer.args.head.span)
      val clonedChild = untpd.Apply(function, clonedOriginal :: sibling :: Nil)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          coherentCopy(structural, clonedChild)
        )
      )

      val wrongOuterFunction = untpd.Apply(
        untpd.Ident(termName("differentService")),
        structural.rebuiltApply.args
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          coherentApplyCopy(structural, wrongOuterFunction)
        )
      )

      val forgedUntouchedSibling = untpd.Ident(termName("differentKeptArg"))
      val wrongUntouchedArgument = untpd.Apply(
        structural.originalApply.fun,
        structural.rebuiltApply.args.updated(1, forgedUntouchedSibling)
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          coherentApplyCopy(structural, wrongUntouchedArgument)
        )
      )

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          structural.copy(
            validatedExisting = structural.validatedExisting.copy(argumentIndex = 1)
          )
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])
        )
      )
    }
  }

  private def coherentCopy(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result,
      wrapper: untpd.Apply
  )(using Context): ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result =
    given SourceFile = NoSource
    val arguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => wrapper
      case (argument, _) => argument
    }
    val rebuiltApply = untpd.Apply(structural.originalApply.fun, arguments)
    coherentApplyCopy(structural, rebuiltApply).copy(wrapperApply = wrapper)

  private def coherentApplyCopy(
      structural: ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result,
      rebuiltApply: untpd.Apply
  )(using Context): ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter.Result =
    given SourceFile = NoSource
    val rebuiltTarget = untpd.DefDef(
      structural.originalTarget.name,
      structural.originalTarget.paramss,
      structural.originalTarget.tpt,
      rebuiltApply
    ).withMods(structural.originalTarget.mods)
    val rebuiltTemplate = untpd.Template(
      structural.originalTemplate.constr,
      structural.originalTemplate.parentsOrDerived,
      structural.originalTemplate.derived,
      structural.originalTemplate.self,
      structural.prefix ::: rebuiltTarget :: structural.suffix
    )
    val rebuiltRoot = untpd.TypeDef(structural.originalRoot.name, rebuiltTemplate)
      .withMods(structural.originalRoot.mods)
    structural.copy(
      rebuiltRoot = rebuiltRoot,
      rebuiltTemplate = rebuiltTemplate,
      rebuiltTarget = rebuiltTarget,
      rebuiltApply = rebuiltApply
    )

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U019Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
