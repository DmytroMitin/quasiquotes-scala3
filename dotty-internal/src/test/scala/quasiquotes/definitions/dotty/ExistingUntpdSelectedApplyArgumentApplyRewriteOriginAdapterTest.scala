package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapterTest extends munit.FunSuite:
  test("positions every replacement subtree node at the exact old argument site") {
    withContext {
      val root = parseClass(
        "class U015Origin:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val exactArgument = outer.args.head
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Ident(termName("helper")),
        untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
      )
      val structural = ExistingUntpdSelectedApplyArgumentApplyRewriter
        .rewrite(root, target, exactArgument, replacement)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      val nodes = adapted.positionedReplacement +:
        adapted.positionedReplacement.fun +: adapted.positionedReplacement.args.toVector
      nodes.foreach { node =>
        assertEquals(node.source, exactArgument.source)
        assertEquals(node.span, exactArgument.span)
        assertEquals(node.symbol, NoSymbol)
      }
      assert(adapted.positionedApply.fun.eq(outer.fun))
      assert(adapted.positionedApply.args(1).eq(outer.args(1)))
      assert(adapted.positionedApply.args.head.eq(adapted.positionedReplacement))
    }
  }

  test("rejects null, contaminated replacement descendants, and forged structural graphs") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(null)
      )
      val root = parseClass(
        "class U015OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = template.body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Ident(termName("helper")),
        untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
      )
      val structural = ExistingUntpdSelectedApplyArgumentApplyRewriter
        .rewrite(root, target, outer.args.head, replacement)
        .fold(problem => fail(problem.message), identity)

      val sourceBearing = untpd.Apply(outer.args.head, replacement.args)
      assertError("SOURCE_FREE_REPLACEMENT_APPLY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          structural.copy(replacementApply = sourceBearing)
        )
      )
      val wrongOuter = untpd.Apply(untpd.Ident(termName("wrong")), structural.rebuiltApply.args)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = wrongOuter)
        )
      )
      val wrongTarget = untpd.DefDef(
        termName("wrongTarget"), structural.rebuiltTarget.paramss,
        structural.rebuiltTarget.tpt, structural.rebuiltApply
      ).withMods(structural.rebuiltTarget.mods)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          structural.copy(rebuiltTarget = wrongTarget)
        )
      )

      val clonedFunction = structural.originalApply.fun
        .cloneIn(structural.originalApply.fun.source)
        .withSpan(structural.originalApply.fun.span)
      assert(!clonedFunction.eq(structural.originalApply.fun))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          coherentCopy(structural, clonedFunction, structural.rebuiltApply.args,
            structural.originalTarget.name)
        )
      )

      val clonedSibling = outer.args(1).cloneIn(outer.args(1).source).withSpan(outer.args(1).span)
      assert(!clonedSibling.eq(outer.args(1)))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          coherentCopy(
            structural,
            structural.originalApply.fun,
            structural.replacementApply :: clonedSibling :: Nil,
            structural.originalTarget.name
          )
        )
      )

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          coherentCopy(
            structural,
            structural.originalApply.fun,
            structural.rebuiltApply.args,
            termName("wrongTarget")
          )
        )
      )

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          structural.copy(validatedExisting = structural.validatedExisting.copy(argumentIndex = 1))
        )
      )

      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])
        )
      )
    }
  }

  private def coherentCopy(
      structural: ExistingUntpdSelectedApplyArgumentApplyRewriter.Result,
      function: untpd.Tree,
      arguments: List[untpd.Tree],
      targetName: dotty.tools.dotc.core.Names.TermName
  )(using Context): ExistingUntpdSelectedApplyArgumentApplyRewriter.Result =
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
      rebuiltRoot = rebuiltRoot,
      rebuiltTemplate = rebuiltTemplate,
      rebuiltTarget = rebuiltTarget,
      rebuiltApply = rebuiltApply
    )

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U015Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentApplyRewriteOriginError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
