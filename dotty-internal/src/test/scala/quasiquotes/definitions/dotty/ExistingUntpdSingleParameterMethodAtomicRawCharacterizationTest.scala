package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm

class ExistingUntpdSingleParameterMethodAtomicRawCharacterizationTest
    extends munit.FunSuite:
  test("characterizes one atomic parameter result and selected-Apply replacement at original sites") {
    withContext {
      val root = parseClass(
        """class AtomicSites:
          |  val before: Int = 1
          |  def bump(x: AnyVal): AnyVal = x
          |  type After = String
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val view = ExistingUntpdSingleParameterMethodView.capture(captured, 1).toOption.get
      given SourceFile = NoSource
      val replacement = untpd.Apply(
        untpd.Select(untpd.Ident(termName("Math")), termName("abs")),
        List(untpd.Ident(termName("x")))
      )

      val result = ExistingUntpdSingleParameterMethodAtomicRewriter
        .rewrite(
          view,
          TypeNormalForm.STypeIdent("Int"),
          TypeNormalForm.STypeIdent("Int"),
          replacement
        )
        .fold(problem => fail(problem.message), identity)

      assert(!result.parameterRewrite.loweredParameterType.eq(result.resultRewrite.loweredResultType))
      assert(!result.parameterRewrite.positionedParameterType.eq(result.resultRewrite.positionedResultType))
      assertEquals(result.parameterRewrite.positionedParameterType.source, view.parameterType.source)
      assertEquals(result.parameterRewrite.positionedParameterType.span, view.parameterType.span)
      assertEquals(result.parameterRewrite.positionedParameter.source, view.parameter.source)
      assertEquals(result.parameterRewrite.positionedParameter.span, view.parameter.span)
      assertEquals(result.resultRewrite.positionedResultType.source, view.resultType.source)
      assertEquals(result.resultRewrite.positionedResultType.span, view.resultType.span)
      val rhsNodes = ExistingUntpdClassMemberFilter.allTrees(result.rhsRewrite.positionedResult.positionedReplacement)
      assert(rhsNodes.forall(tree => tree.source == view.rhs.source && tree.span == view.rhs.span))
      assertEquals(result.positionedMethod.source, view.method.source)
      assertEquals(result.positionedMethod.span, view.method.span)
      assert(result.positionedMethod.paramss.head.head.eq(result.parameterRewrite.positionedParameter))
      assert(result.positionedMethod.tpt.eq(result.resultRewrite.positionedResultType))
      assert(result.positionedMethod.rhs.eq(result.rhsRewrite.positionedResult.positionedReplacement))
      assert(ExistingUntpdClassMemberFilter.allTrees(result.positionedRoot).forall(tree =>
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
      ))
    }
  }

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U032AtomicRaw.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
