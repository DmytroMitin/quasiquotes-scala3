package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("positions only fresh wrapper nodes at the selected argument site") {
    withContext {
      val root = parseClass(
        "class U018Origin:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val exactArgument = outer.args.head
      val originalArgumentState =
        (exactArgument.source, exactArgument.span, exactArgument.symbol)
      given SourceFile = NoSource
      val wrapperFunction = untpd.Ident(termName("helper"))
      val structural = ExistingUntpdSelectedApplyArgumentWrapRewriter
        .rewrite(root, target, exactArgument, wrapperFunction)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      Vector[untpd.Tree](adapted.positionedWrapperApply, adapted.positionedWrapperFunction)
        .foreach { node =>
          assertEquals(node.source, exactArgument.source)
          assertEquals(node.span, exactArgument.span)
          assertEquals(node.symbol, NoSymbol)
          assert(!node.isInstanceOf[untpd.TypedSplice])
        }
      assertEquals(adapted.positionedWrapperApply.args.size, 1)
      assert(adapted.positionedWrapperApply.args.head.eq(exactArgument))
      assert(adapted.positionedApply.fun.eq(outer.fun))
      assert(adapted.positionedApply.args.head.eq(adapted.positionedWrapperApply))
      assert(adapted.positionedApply.args(1).eq(outer.args(1)))
      assertEquals(
        (exactArgument.source, exactArgument.span, exactArgument.symbol),
        originalArgumentState
      )
      assert(structural.wrapperApply.args.head.eq(exactArgument))
      assert(!structural.wrapperApply.source.exists)
      assert(!structural.wrapperFunction.source.exists)
    }
  }

  test("rejects null carriers, forged wrapper children, and forged preserved identities") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(null)
      )
      val root = parseClass(
        "class U018OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val wrapperFunction = untpd.Ident(termName("helper"))
      val structural = ExistingUntpdSelectedApplyArgumentWrapRewriter
        .rewrite(root, target, outer.args.head, wrapperFunction)
        .fold(problem => fail(problem.message), identity)

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          structural.copy(wrapperApply = null.asInstanceOf[untpd.Apply])
        )
      )

      val clonedArgument = outer.args.head
        .cloneIn(outer.args.head.source).withSpan(outer.args.head.span)
      val wrongChildWrapper = untpd.Apply(wrapperFunction, clonedArgument :: Nil)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          coherentCopy(structural, outer.fun, wrongChildWrapper :: outer.args(1) :: Nil,
            wrongChildWrapper, target.name)
        )
      )

      val clonedFunction = outer.fun.cloneIn(outer.fun.source).withSpan(outer.fun.span)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          coherentCopy(structural, clonedFunction, structural.rebuiltApply.args,
            structural.wrapperApply, target.name)
        )
      )

      val clonedSibling = outer.args(1).cloneIn(outer.args(1).source).withSpan(outer.args(1).span)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          coherentCopy(structural, outer.fun,
            structural.wrapperApply :: clonedSibling :: Nil,
            structural.wrapperApply, target.name)
        )
      )

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          coherentCopy(structural, outer.fun, structural.rebuiltApply.args,
            structural.wrapperApply, termName("wrongTarget"))
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          structural.copy(validatedExisting =
            structural.validatedExisting.copy(argumentIndex = 1))
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])
        )
      )
    }
  }

  private def coherentCopy(
      structural: ExistingUntpdSelectedApplyArgumentWrapRewriter.Result,
      function: untpd.Tree,
      arguments: List[untpd.Tree],
      wrapperApply: untpd.Apply,
      targetName: dotty.tools.dotc.core.Names.TermName
  )(using Context): ExistingUntpdSelectedApplyArgumentWrapRewriter.Result =
    given SourceFile = NoSource
    val rebuiltApply = untpd.Apply(function, arguments)
    val rebuiltTarget = untpd.DefDef(
      targetName,
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
      wrapperApply = wrapperApply,
      rebuiltRoot = rebuiltRoot,
      rebuiltTemplate = rebuiltTemplate,
      rebuiltTarget = rebuiltTarget,
      rebuiltApply = rebuiltApply
    )

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U018Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapRewriteOriginError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
