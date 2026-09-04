package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

class ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapterTest
    extends munit.FunSuite:
  test("S6 positions every fresh wrapper and child-bearing sibling node at the argument site") {
    withContext {
      val root = parseClass(
        "class U020Origin:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      val original = outer.args.head
      val originalState = (original.source, original.span, original.symbol)
      given SourceFile = NoSource
      val wrapperFunction = untpd.Ident(termName("helper"))
      val siblingFunction = untpd.Ident(termName("product"))
      val siblingArguments = List[untpd.Tree](
        untpd.Number("4", untpd.NumberKind.Whole(10)),
        untpd.Literal(dotty.tools.dotc.core.Constants.Constant(5))
      )
      val siblingApply = untpd.Apply(siblingFunction, siblingArguments)
      val structural = ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
        .rewrite(root, target, original, wrapperFunction, siblingApply)
        .fold(problem => fail(problem.message), identity)

      val adapted = ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter
        .adapt(structural)
        .fold(problem => fail(problem.message), identity)

      val positionedFresh = Vector[untpd.Tree](
        adapted.positionedWrapperApply,
        adapted.positionedWrapperFunction,
        adapted.positionedFreshSiblingApply,
        adapted.positionedFreshSiblingFunction
      ) ++ adapted.positionedFreshSiblingArguments
      positionedFresh.foreach { node =>
        assertEquals(node.source, original.source)
        assertEquals(node.span, original.span)
        assertEquals(node.symbol, NoSymbol)
        assert(!node.isInstanceOf[untpd.TypedSplice])
      }
      assert(adapted.positionedWrapperApply.args(0).eq(original))
      assert(adapted.positionedWrapperApply.args(1).eq(adapted.positionedFreshSiblingApply))
      assert(adapted.positionedFreshSiblingApply.fun.eq(adapted.positionedFreshSiblingFunction))
      siblingArguments.indices.foreach(index =>
        assert(adapted.positionedFreshSiblingApply.args(index)
          .eq(adapted.positionedFreshSiblingArguments(index)))
      )
      assert(adapted.positionedApply.fun.eq(outer.fun))
      assert(adapted.positionedApply.args(1).eq(outer.args(1)))
      assertEquals((original.source, original.span, original.symbol), originalState)
      assert(structural.wrapperApply.args(0).eq(original))
      assert(structural.wrapperApply.args(1).eq(siblingApply))
      assert(!structural.wrapperApply.source.exists)
      assert(!structural.freshSiblingApply.source.exists)
    }
  }

  test("rejects null carriers and forged sibling order or rebuilt identity") {
    withContext {
      assertError("RESULT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.adapt(null)
      )
      val root = parseClass(
        "class U020OriginFailures:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val target = root.rhs.asInstanceOf[untpd.Template].body.collectFirst {
        case value: untpd.DefDef if value.name.toString == "change" => value
      }.getOrElse(fail("missing change method"))
      val outer = target.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val wrapper = untpd.Ident(termName("helper"))
      val siblingFunction = untpd.Ident(termName("product"))
      val left = untpd.Number("4", untpd.NumberKind.Whole(10))
      val right = untpd.Number("5", untpd.NumberKind.Whole(10))
      val sibling = untpd.Apply(siblingFunction, left :: right :: Nil)
      val structural = ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter
        .rewrite(root, target, outer.args.head, wrapper, sibling)
        .fold(problem => fail(problem.message), identity)

      val reversedSibling = untpd.Apply(siblingFunction, right :: left :: Nil)
      val forgedWrapper = untpd.Apply(wrapper, outer.args.head :: reversedSibling :: Nil)
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.adapt(
          coherentCopy(structural, forgedWrapper)
        )
      )

      val wrongOuterFunction = untpd.Apply(
        untpd.Ident(termName("differentService")), structural.rebuiltApply.args
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.adapt(
          coherentApplyCopy(structural, wrongOuterFunction)
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.adapt(
          structural.copy(validatedExisting = structural.validatedExisting.copy(argumentIndex = 1))
        )
      )
      assertError("STRUCTURAL_IDENTITY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.adapt(
          structural.copy(rebuiltApply = null.asInstanceOf[untpd.Apply])
        )
      )
    }
  }

  private def coherentCopy(
      structural: ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result,
      wrapper: untpd.Apply
  )(using Context): ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result =
    given SourceFile = NoSource
    val arguments = structural.originalApply.args.zipWithIndex.map {
      case (_, index) if index == structural.argumentIndex => wrapper
      case (argument, _) => argument
    }
    coherentApplyCopy(structural, untpd.Apply(structural.originalApply.fun, arguments))
      .copy(wrapperApply = wrapper)

  private def coherentApplyCopy(
      structural: ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result,
      rebuiltApply: untpd.Apply
  )(using Context): ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter.Result =
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
    structural.copy(rebuiltRoot = rebuiltRoot, rebuiltTemplate = rebuiltTemplate,
      rebuiltTarget = rebuiltTarget, rebuiltApply = rebuiltApply)

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U020Origin.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginError, ?]
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expectedCode)
    case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
