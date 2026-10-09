package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView}

class ExistingCrossMemberCallingModeOwnerTransactionCharacterizationTest
    extends munit.FunSuite:
  test("canonical owner exposes distinct admitted by-name and repeated method scopes") {
    withContext {
      val root = parseClass(canonical("MultipleModes", "eval", "lazyValue", "sum", "values"))
      val template = root.rhs.asInstanceOf[untpd.Template]
      val rawEval = template.body(0).asInstanceOf[untpd.DefDef]
      val rawSum = template.body(1).asInstanceOf[untpd.DefDef]
      val rawUntouched = template.body(2).asInstanceOf[untpd.ValDef]
      val evalParameter = rawEval.paramss.head.head.asInstanceOf[untpd.ValDef]
      val sumParameter = rawSum.paramss.head.head.asInstanceOf[untpd.ValDef]

      assertEquals(rawEval.name.toString, "eval")
      assertEquals(rawSum.name.toString, "sum")
      assertEquals(rawUntouched.name.toString, "untouched")
      assertEquals(rawEval.mods.flags, Flags.Method)
      assertEquals(rawSum.mods.flags, Flags.Method)
      assertEquals(evalParameter.mods.flags, Flags.Param)
      assertEquals(sumParameter.mods.flags, Flags.Param)
      assert(evalParameter.tpt.isInstanceOf[untpd.ByNameTypeTree])
      assert(sumParameter.tpt.isInstanceOf[untpd.PostfixOp])
      Vector(
        rawEval,
        rawSum,
        rawUntouched,
        evalParameter,
        sumParameter,
        evalParameter.tpt,
        sumParameter.tpt
      ).foreach { tree =>
        assert(tree.source.eq(root.source))
        assert(tree.span.exists)
      }


      val captured = capture(root)
      assertEquals(captured.members.map(_.index), Vector(0, 1, 2))
      assertEquals(captured.members.map(_.diagnosticName), Vector(Some("eval"), Some("sum"), Some("untouched")))
      val eval = method(captured, 0)
      val sum = method(captured, 1)
      val evalView = eval.parameterClauses.head.parameters.head
      val sumView = sum.parameterClauses.head.parameters.head

      assertEquals(eval.parameterClauses.size, 1)
      assertEquals(sum.parameterClauses.size, 1)
      assertEquals(evalView.clauseIndex, 0)
      assertEquals(sumView.clauseIndex, 0)
      assertEquals(evalView.parameterIndex, 0)
      assertEquals(sumView.parameterIndex, 0)
      assert(evalView.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(evalParameter)))
      assert(sumView.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(sumParameter)))
      assert(evalView.declaredType.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(evalParameter.tpt)))
      assert(sumView.declaredType.identity.sameObjectAs(new ExistingClassUntypedRewrite.ExactIdentity(sumParameter.tpt)))
      assert(evalView.declaredType.semantic.isLeft)
      assert(sumView.declaredType.semantic.isLeft)
      assert(!eval.ref.eq(sum.ref))
      assert(!evalView.ref.eq(sumView.ref))
      assertPublicCode(eval.parameterScope.reference(sumView.ref), "SELECTION_FAILED")
      assertPublicCode(sum.parameterScope.reference(evalView.ref), "SELECTION_FAILED")
      assertPreTyperClean(root)
    }
  }

  test("renamed methods and parameters retain topology without name-based selection") {
    withContext {
      val root = parseClass(canonical("RenamedModes", "defer", "later", "collect", "items"))
      val captured = capture(root)
      val byName = method(captured, 0)
      val repeated = method(captured, 1)

      assertEquals(byName.diagnosticName, "defer")
      assertEquals(repeated.diagnosticName, "collect")
      assertEquals(byName.parameterClauses.head.parameters.head.diagnosticName, "later")
      assertEquals(repeated.parameterClauses.head.parameters.head.diagnosticName, "items")
      assert(captured.members(0).identity.sameObjectAs(byNameMemberIdentity(root, 0)))
      assert(captured.members(1).identity.sameObjectAs(byNameMemberIdentity(root, 1)))
      assertPreTyperClean(root)
    }
  }

  test("empty public plan preserves the exact owner and every direct member") {
    withContext {
      val root = parseClass(canonical("NoopModes", "eval", "lazyValue", "sum", "values"))
      val captured = capture(root)
      val result = publicRight(ExistingClassUntypedRewrite(captured, captured.emptyPlan))

      assert(!result.changed)
      assert(result.tree.eq(root))
      assert(result.directMemberIdentities.zip(captured.members).forall { case (actual, expected) =>
        actual.sameObjectAs(expected.identity)
      })
    }
  }

  private def canonical(
      className: String,
      byNameMethod: String,
      byNameParameter: String,
      repeatedMethod: String,
      repeatedParameter: String
  ): String =
    s"class $className:\n" +
      s"  def $byNameMethod($byNameParameter: => AnyVal): Int = 0\n" +
      s"  def $repeatedMethod($repeatedParameter: AnyVal*): Int = $repeatedParameter.size\n" +
      "  val untouched: Int = 7\n"

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def method(captured: Capture, index: Int)(using Context): MethodView =
    publicRight(captured.method(publicRight(captured.member(index)).ref))

  private def byNameMemberIdentity(
      root: untpd.TypeDef,
      index: Int
  )(using Context): ExistingClassUntypedRewrite.ExactIdentity =
    new ExistingClassUntypedRewrite.ExactIdentity(root.rhs.asInstanceOf[untpd.Template].body(index))

  private def assertPreTyperClean(root: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(root)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U061CrossMemberCharacterization.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def assertPublicCode[A](value: Either[Failure, A], expected: String): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
