package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView, Result}
import quasiquotes.definitions.dotty.ExistingUntpdOrdinaryMethodTypeEditPreparation.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingCrossMemberCallingModeOwnerTransactionSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("A by-name-only edit preserves the exact repeated method and untouched value") {
    withContext {
      val root = owner("CrossByNameOnly", "Int", "Int")
      val original = directMembers(root)
      val captured = capture(root)
      val eval = method(captured, 0)
      val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter(eval).ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = directMembers(result.tree)

      assertFreshByName(parameterTree(original(0)), parameterTree(rewritten(0)))
      assert(rewritten(1).eq(original(1)))
      assert(rewritten(2).eq(original(2)))
      assertResultIdentities(result, captured, changed = Set(0))
      assertPreTyperClean(result.tree)
    }
  }

  test("B repeated-only edit preserves the exact by-name method and untouched value") {
    withContext {
      val root = owner("CrossRepeatedOnly", "Int", "Int")
      val original = directMembers(root)
      val captured = capture(root)
      val sum = method(captured, 1)
      val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter(sum).ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = directMembers(result.tree)

      assert(rewritten(0).eq(original(0)))
      assertFreshRepeated(parameterTree(original(1)), parameterTree(rewritten(1)))
      assert(rewritten(2).eq(original(2)))
      assertResultIdentities(result, captured, changed = Set(1))
      assertPreTyperClean(result.tree)
    }
  }

  test("C both cross-member Type edits use one plan and one fresh owner in either order") {
    withContext {
      Vector(true, false).foreach { byNameFirst =>
        val root = owner(if byNameFirst then "CrossBothByNameFirst" else "CrossBothRepeatedFirst", "Int", "Int")
        val original = directMembers(root)
        val captured = capture(root)
        val eval = method(captured, 0)
        val sum = method(captured, 1)
        val plan = publicRight(
          if byNameFirst then
            for
              first <- captured.emptyPlan.replaceParameterType(parameter(eval).ref, IntType)
              both <- first.replaceParameterType(parameter(sum).ref, IntType)
            yield both
          else
            for
              first <- captured.emptyPlan.replaceParameterType(parameter(sum).ref, IntType)
              both <- first.replaceParameterType(parameter(eval).ref, IntType)
            yield both
        )
        assertEquals(plan.editCount, 2)
        val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
        val rewritten = directMembers(result.tree)

        assert(result.changed)
        assert(!result.tree.eq(root))
        assertFreshByName(parameterTree(original(0)), parameterTree(rewritten(0)))
        assertFreshRepeated(parameterTree(original(1)), parameterTree(rewritten(1)))
        assert(rewritten(2).eq(original(2)))
        assertResultIdentities(result, captured, changed = Set(0, 1))
        assertPreTyperClean(result.tree)
      }
    }
  }

  test("D cross-member parameter result and body intents all win in one owner transaction") {
    withContext {
      val root = owner("CrossAllIntents", "AnyVal", "AnyVal")
      val original = directMembers(root)
      val captured = capture(root)
      val eval = method(captured, 0)
      val sum = method(captured, 1)
      val plan = publicRight(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter(eval).ref, IntType)
        p2 <- p1.replaceParameterType(parameter(sum).ref, IntType)
        r1 <- p2.replaceResultType(eval.ref, IntType)
        r2 <- r1.replaceResultType(sum.ref, IntType)
        b1 <- r2.replaceBody(eval.ref, TermShape.Literal("11"))
        b2 <- b1.replaceBody(sum.ref, TermShape.Literal("13"))
      yield b2)
      assertEquals(plan.editCount, 6)
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = directMembers(result.tree)
      val rewrittenEval = rewritten(0).asInstanceOf[untpd.DefDef]
      val rewrittenSum = rewritten(1).asInstanceOf[untpd.DefDef]

      assertFreshByName(parameterTree(original(0)), parameterTree(rewritten(0)))
      assertFreshRepeated(parameterTree(original(1)), parameterTree(rewritten(1)))
      assertFreshIdent(rewrittenEval.tpt, original(0).asInstanceOf[untpd.DefDef].tpt, "Int")
      assertFreshIdent(rewrittenSum.tpt, original(1).asInstanceOf[untpd.DefDef].tpt, "Int")
      assertFreshNumber(rewrittenEval.rhs, original(0).asInstanceOf[untpd.DefDef].rhs, "11")
      assertFreshNumber(rewrittenSum.rhs, original(1).asInstanceOf[untpd.DefDef].rhs, "13")
      assert(rewritten(2).eq(original(2)))
      assertResultIdentities(result, captured, changed = Set(0, 1))
      assertPreTyperClean(result.tree)
    }
  }

  test("foreign stale duplicate and cross-method binder intents fail before owner reconstruction") {
    withContext {
      val root = owner("CrossFailures", "Int", "Int")
      val original = directMembers(root)
      val captured = capture(root)
      val eval = method(captured, 0)
      val sum = method(captured, 1)
      val evalParameter = parameter(eval)
      val sumParameter = parameter(sum)
      val first = publicRight(captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType))
      assertPublicCode(first.replaceParameterType(evalParameter.ref, IntType), "EDIT_CONFLICT")

      val foreign = capture(owner("CrossForeign", "Int", "Int"))
      assertPublicCode(
        captured.emptyPlan.replaceParameterType(parameter(method(foreign, 1)).ref, IntType),
        "SELECTION_FAILED"
      )

      val evalReference = publicRight(eval.parameterScope.reference(evalParameter.ref))
      assertPublicCode(
        captured.emptyPlan.replaceBody(sum.ref, evalReference),
        "SEMANTIC_FRAGMENT_INVALID"
      )

      val both = publicRight(for
        p1 <- captured.emptyPlan.replaceParameterType(evalParameter.ref, IntType)
        p2 <- p1.replaceParameterType(sumParameter.ref, IntType)
      yield p2)
      assertPublicCode(both.replaceBody(sum.ref, evalReference), "SEMANTIC_FRAGMENT_INVALID")
      assert(directMembers(root).zip(original).forall((left, right) => left.eq(right)))

      val rewritten = publicRight(ExistingClassUntypedRewrite(captured, first))
      val recaptured = capture(rewritten.tree)
      assertPublicCode(
        recaptured.emptyPlan.replaceParameterType(evalParameter.ref, IntType),
        "SELECTION_FAILED"
      )
    }
  }

  test("cross-member private preparation evidence cannot be substituted or retargeted") {
    withContext {
      val exact = ExistingUntpdClassMemberFilter.capture(owner("CrossEvidence", "Int", "Int"))
        .fold(problem => fail(problem.message), identity)
      val evalDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0)
        .fold(problem => fail(problem.message), identity)
      val sumDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 1)
        .fold(problem => fail(problem.message), identity)
      val byName = prepareParameterType(
        evalDescriptor,
        evalDescriptor.parameterClauses.head.head,
        IntType
      ).fold(problem => fail(problem.message), identity)
      val repeated = prepareParameterType(
        sumDescriptor,
        sumDescriptor.parameterClauses.head.head,
        IntType
      ).fold(problem => fail(problem.message), identity)

      assertPreparationCode(
        validatePreparedParameterType(byName.copy(evidence = repeated.evidence), evalDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(repeated.copy(evidence = byName.evidence), sumDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(byName, sumDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(repeated, evalDescriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  private def owner(name: String, evalResult: String, sumResult: String)(using Context): untpd.TypeDef =
    parseClass(
      s"class $name:\n" +
        s"  def eval(lazyValue: => AnyVal): $evalResult = 0\n" +
        s"  def sum(values: AnyVal*): $sumResult = values.size\n" +
        "  val untouched: Int = 7\n"
    )

  private def directMembers(root: untpd.TypeDef)(using Context): Vector[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body.toVector

  private def parameterTree(method: untpd.Tree): untpd.ValDef =
    method.asInstanceOf[untpd.DefDef].paramss.head.head.asInstanceOf[untpd.ValDef]

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def method(captured: Capture, index: Int)(using Context): MethodView =
    publicRight(captured.method(publicRight(captured.member(index)).ref))

  private def parameter(method: MethodView): ExistingClassUntypedRewrite.ParameterView =
    method.parameterClauses.head.parameters.head

  private def assertFreshByName(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val originalWrapper = original.tpt.asInstanceOf[untpd.ByNameTypeTree]
    val rewrittenWrapper = rewritten.tpt.asInstanceOf[untpd.ByNameTypeTree]
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!rewrittenWrapper.eq(originalWrapper))
    assertEquals(rewrittenWrapper.source, originalWrapper.source)
    assertEquals(rewrittenWrapper.span, originalWrapper.span)
    assertFreshIdent(rewrittenWrapper.result, originalWrapper.result, "Int")

  private def assertFreshRepeated(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val originalWrapper = original.tpt.asInstanceOf[untpd.PostfixOp]
    val rewrittenWrapper = rewritten.tpt.asInstanceOf[untpd.PostfixOp]
    val (originalElement, originalMarker) = originalWrapper match
      case untpd.PostfixOp(element, marker) => (element, marker)
    val (rewrittenElement, rewrittenMarker) = rewrittenWrapper match
      case untpd.PostfixOp(element, marker) => (element, marker)
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!rewrittenWrapper.eq(originalWrapper))
    assertEquals(rewrittenWrapper.source, originalWrapper.source)
    assertEquals(rewrittenWrapper.span, originalWrapper.span)
    assertFreshIdent(rewrittenElement, originalElement, "Int")
    assert(rewrittenMarker.eq(originalMarker))

  private def assertFreshIdent(actual: untpd.Tree, original: untpd.Tree, expected: String): Unit =
    actual match
      case ident: untpd.Ident =>
        assertEquals(ident.name.toString, expected)
        assert(!ident.eq(original))
        assertEquals(ident.source, original.source)
        assertEquals(ident.span, original.span)
      case other => fail("expected fresh " + expected + " Ident, found " + other)

  private def assertFreshNumber(actual: untpd.Tree, original: untpd.Tree, expected: String): Unit =
    actual match
      case number @ untpd.Number(digits, _) =>
        assertEquals(digits, expected)
        assert(!number.eq(original))
        assertEquals(number.source, original.source)
        assertEquals(number.span, original.span)
      case other => fail("expected fresh numeric literal, found " + other)

  private def assertResultIdentities(
      result: Result,
      captured: Capture,
      changed: Set[Int]
  ): Unit =
    captured.members.indices.foreach { index =>
      assertEquals(
        result.directMemberIdentities(index).sameObjectAs(captured.members(index).identity),
        !changed(index)
      )
    }

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U061CrossMemberSafety.scala", source)
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

  private def assertPreparationCode[A](
      value: Either[ExistingUntpdOrdinaryMethodTypeEditPreparation.Error, A],
      expected: String
  ): Unit =
    value match
      case Left(problem) => assertEquals(problem.code, expected)
      case Right(success) => fail("expected " + expected + ", found success " + success)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
