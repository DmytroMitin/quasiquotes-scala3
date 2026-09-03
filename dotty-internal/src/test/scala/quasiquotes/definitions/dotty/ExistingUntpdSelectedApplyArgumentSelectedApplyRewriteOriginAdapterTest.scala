package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("positions every selected-member replacement node at the exact old argument site") {
    withContext {
      val (root, target, outer) = fixture("U016Origin")
      val exactArgument = outer.args.head
      given SourceFile = NoSource
      val replacement = selectedReplacement()
      val structural = ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter
        .rewrite(root, target, exactArgument, replacement)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      val nodes = adapted.positionedReplacement +:
        adapted.positionedReplacementSelection +:
        adapted.positionedReplacementQualifier +:
        adapted.positionedReplacementArguments.toVector
      nodes.foreach { node =>
        assertEquals(node.source, exactArgument.source)
        assertEquals(node.span, exactArgument.span)
        assertEquals(node.symbol, NoSymbol)
      }
      assert(adapted.positionedReplacement.fun.eq(adapted.positionedReplacementSelection))
      assert(adapted.positionedReplacementSelection.qualifier
        .eq(adapted.positionedReplacementQualifier))
      assert(adapted.positionedApply.fun.eq(outer.fun))
      assert(adapted.positionedApply.args(1).eq(outer.args(1)))
      assert(adapted.positionedApply.args.head.eq(adapted.positionedReplacement))
      assert(!adapted.positionedReplacement.eq(replacement))
      assert(!adapted.positionedReplacementSelection.eq(replacement.fun))
      assert(!adapted.positionedReplacementQualifier
        .eq(replacement.fun.asInstanceOf[untpd.Select].qualifier))
    }
  }

  test("rejects null, contaminated descendants, and forged structural graphs") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(null)
      )
      val (root, target, outer) = fixture("U016OriginFailures")
      given SourceFile = NoSource
      val replacement = selectedReplacement()
      val structural = ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter
        .rewrite(root, target, outer.args.head, replacement)
        .fold(problem => fail(problem.message), identity)

      val sourceBearingSelection = untpd.Select(outer.args.head, termName("make"))
      val sourceBearing = untpd.Apply(sourceBearingSelection, replacement.args)
      assertError("SOURCE_FREE_REPLACEMENT_SELECTED_APPLY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          structural.copy(replacementApply = sourceBearing)
        )
      )
      val directFunction = untpd.Apply(untpd.Ident(termName("helper")), replacement.args)
      assertError("SOURCE_FREE_REPLACEMENT_SELECTED_APPLY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          structural.copy(replacementApply = directFunction)
        )
      )
      val wrongOuter = untpd.Apply(untpd.Ident(termName("wrong")), structural.rebuiltApply.args)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = wrongOuter)
        )
      )
      val clonedFunction = structural.originalApply.fun
        .cloneIn(structural.originalApply.fun.source)
        .withSpan(structural.originalApply.fun.span)
      assert(!clonedFunction.eq(structural.originalApply.fun))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          coherentCopy(structural, clonedFunction, structural.rebuiltApply.args,
            structural.originalTarget.name)
        )
      )
      val clonedSibling = outer.args(1).cloneIn(outer.args(1).source).withSpan(outer.args(1).span)
      assert(!clonedSibling.eq(outer.args(1)))
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          coherentCopy(
            structural,
            structural.originalApply.fun,
            structural.replacementApply :: clonedSibling :: Nil,
            structural.originalTarget.name
          )
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          coherentCopy(
            structural,
            structural.originalApply.fun,
            structural.rebuiltApply.args,
            termName("wrongTarget")
          )
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          structural.copy(validatedExisting = structural.validatedExisting.copy(argumentIndex = 1))
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])
        )
      )
    }
  }

  private def selectedReplacement()(using SourceFile): untpd.Apply =
    untpd.Apply(
      untpd.Select(untpd.Ident(termName("helper")), termName("make")),
      untpd.Number("20", untpd.NumberKind.Whole(10)) :: Nil
    )

  private def coherentCopy(
      structural: ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result,
      function: untpd.Tree,
      arguments: List[untpd.Tree],
      targetName: dotty.tools.dotc.core.Names.TermName
  )(using Context): ExistingUntpdSelectedApplyArgumentSelectedApplyRewriter.Result =
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

  private def fixture(name: String)(using Context): (untpd.TypeDef, untpd.DefDef, untpd.Apply) =
    val root = parseClass(
      s"class $name:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
    )
    val template = root.rhs.asInstanceOf[untpd.Template]
    val target = template.body.collectFirst {
      case value: untpd.DefDef if value.name.toString == "change" => value
    }.getOrElse(fail("missing change method"))
    (root, target, target.rhs.asInstanceOf[untpd.Apply])

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U016Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
