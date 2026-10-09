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

class ExistingMixedCallingModeParameterTypeEditsSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("A by-name-only edit preserves the exact repeated parameter and wrapper") {
    withContext {
      val root = mixedClass("MixedByNameOnly", resultType = "Int", body = "values.size")
      val originalMethod = firstMethod(root)
      val originalParameters = parameters(originalMethod)
      val originalRepeated = repeatedType(originalParameters(1))
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(
        captured.emptyPlan.replaceParameterType(
          method.parameterClauses.head.parameters.head.ref,
          IntType
        )
      )
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameters = parameters(rewritten)

      assertFreshByName(originalParameters.head, rewrittenParameters.head)
      assert(rewrittenParameters(1).eq(originalParameters(1)))
      assert(rewrittenParameters(1).tpt.eq(originalRepeated))
      assertUntouchedMember(result, captured)
      assertPreTyperClean(result.tree)
    }
  }

  test("B repeated-only edit preserves the exact by-name parameter and wrapper") {
    withContext {
      val root = mixedClass("MixedRepeatedOnly", resultType = "Int", body = "values.size")
      val originalMethod = firstMethod(root)
      val originalParameters = parameters(originalMethod)
      val originalByName = byNameType(originalParameters.head)
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(
        captured.emptyPlan.replaceParameterType(
          method.parameterClauses.head.parameters(1).ref,
          IntType
        )
      )
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameters = parameters(rewritten)

      assert(rewrittenParameters.head.eq(originalParameters.head))
      assert(rewrittenParameters.head.tpt.eq(originalByName))
      assertFreshRepeated(originalParameters(1), rewrittenParameters(1))
      assertUntouchedMember(result, captured)
      assertPreTyperClean(result.tree)
    }
  }

  test("C dual Type edits preserve both wrappers in either immutable intent order") {
    withContext {
      Vector(true, false).foreach { byNameFirst =>
        val root = mixedClass(
          if byNameFirst then "MixedDualByNameFirst" else "MixedDualRepeatedFirst",
          resultType = "Int",
          body = "values.size"
        )
        val originalMethod = firstMethod(root)
        val originalParameters = parameters(originalMethod)
        val captured = capture(root)
        val method = methodView(captured)
        val views = method.parameterClauses.head.parameters
        val plan = publicRight(
          if byNameFirst then
            for
              first <- captured.emptyPlan.replaceParameterType(views.head.ref, IntType)
              both <- first.replaceParameterType(views(1).ref, IntType)
            yield both
          else
            for
              second <- captured.emptyPlan.replaceParameterType(views(1).ref, IntType)
              both <- second.replaceParameterType(views.head.ref, IntType)
            yield both
        )
        assertEquals(plan.editCount, 2)
        val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
        val rewritten = firstMethod(result.tree)
        val rewrittenParameters = parameters(rewritten)

        assertEquals(rewritten.paramss.map(_.size), List(2))
        assertFreshByName(originalParameters.head, rewrittenParameters.head)
        assertFreshRepeated(originalParameters(1), rewrittenParameters(1))
        assertNotEquals(
          byNameType(rewrittenParameters.head).span,
          repeatedType(rewrittenParameters(1)).span
        )
        assert(rewritten.tpt.eq(originalMethod.tpt))
        assert(rewritten.rhs.eq(originalMethod.rhs))
        assertUntouchedMember(result, captured)
        assertPreTyperClean(result.tree)
      }
    }
  }

  test("D dual Type result and body edits all win atomically in either Type-intent order") {
    withContext {
      Vector(true, false).foreach { byNameFirst =>
        val root = mixedClass(
          if byNameFirst then "MixedAtomicByNameFirst" else "MixedAtomicRepeatedFirst",
          resultType = "AnyVal",
          body = "values.size"
        )
        val originalMethod = firstMethod(root)
        val originalParameters = parameters(originalMethod)
        val captured = capture(root)
        val method = methodView(captured)
        val views = method.parameterClauses.head.parameters
        val types =
          if byNameFirst then
            for
              first <- captured.emptyPlan.replaceParameterType(views.head.ref, IntType)
              both <- first.replaceParameterType(views(1).ref, IntType)
            yield both
          else
            for
              second <- captured.emptyPlan.replaceParameterType(views(1).ref, IntType)
              both <- second.replaceParameterType(views.head.ref, IntType)
            yield both
        val plan = publicRight(for
          typePlan <- types
          resultPlan <- typePlan.replaceResultType(method.ref, IntType)
          bodyPlan <- resultPlan.replaceBody(method.ref, TermShape.Literal("7"))
        yield bodyPlan)
        assertEquals(plan.editCount, 4)

        val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
        val rewritten = firstMethod(result.tree)
        val rewrittenParameters = parameters(rewritten)
        assertFreshByName(originalParameters.head, rewrittenParameters.head)
        assertFreshRepeated(originalParameters(1), rewrittenParameters(1))
        rewritten.tpt match
          case ident: untpd.Ident =>
            assertEquals(ident.name.toString, "Int")
            assert(!ident.eq(originalMethod.tpt))
            assertEquals(ident.source, originalMethod.tpt.source)
            assertEquals(ident.span, originalMethod.tpt.span)
          case other => fail("expected fresh result Int Ident, found " + other)
        rewritten.rhs match
          case number @ untpd.Number(digits, _) =>
            assertEquals(digits, "7")
            assert(!number.eq(originalMethod.rhs))
            assertEquals(number.source, originalMethod.rhs.source)
            assertEquals(number.span, originalMethod.rhs.span)
          case other => fail("expected replacement literal body, found " + other)
        assertUntouchedMember(result, captured)
        assertPreTyperClean(result.tree)
      }
    }
  }

  test("duplicate and foreign mixed edit intents fail deterministically") {
    withContext {
      val firstCapture = capture(mixedClass("MixedIntentOne", "Int", "values.size"))
      val firstMethod = methodView(firstCapture)
      val firstParameters = firstMethod.parameterClauses.head.parameters
      val firstEdit = publicRight(
        firstCapture.emptyPlan.replaceParameterType(firstParameters.head.ref, IntType)
      )
      assertPublicCode(
        firstEdit.replaceParameterType(firstParameters.head.ref, IntType),
        "EDIT_CONFLICT"
      )

      val secondCapture = capture(mixedClass("MixedIntentTwo", "Int", "values.size"))
      val secondMethod = methodView(secondCapture)
      val foreignRepeated = secondMethod.parameterClauses.head.parameters(1)
      assertPublicCode(
        firstCapture.emptyPlan.replaceParameterType(foreignRepeated.ref, IntType),
        "SELECTION_FAILED"
      )
    }
  }

  test("by-name and repeated private evidence cannot be cross-substituted") {
    withContext {
      val root = mixedClass("MixedEvidence", "Int", "values.size")
      val exact = ExistingUntpdClassMemberFilter.capture(root)
        .fold(problem => fail(problem.message), identity)
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(exact, 0)
        .fold(problem => fail(problem.message), identity)
      val byName = prepareParameterType(
        descriptor,
        descriptor.parameterClauses.head.head,
        IntType
      ).fold(problem => fail(problem.message), identity)
      val repeated = prepareParameterType(
        descriptor,
        descriptor.parameterClauses.head(1),
        IntType
      ).fold(problem => fail(problem.message), identity)

      assertPreparationCode(
        validatePreparedParameterType(byName.copy(evidence = repeated.evidence), descriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(repeated.copy(evidence = byName.evidence), descriptor),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
      assertPreparationCode(
        validatePreparedParameterType(byName, ExistingUntpdOrdinaryMethodDescriptor
          .capture(
            ExistingUntpdClassMemberFilter.capture(
              mixedClass("MixedEvidenceForeign", "Int", "values.size")
            ).fold(problem => fail(problem.message), identity),
            0
          ).fold(problem => fail(problem.message), identity)),
        "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED"
      )
    }
  }

  private def mixedClass(name: String, resultType: String, body: String)(using Context): untpd.TypeDef =
    parseClass(
      s"class $name:\n  def compute(lazyValue: => AnyVal, values: AnyVal*): $resultType = $body\n  val untouched: Int = 9\n"
    )

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def methodView(captured: Capture)(using Context): MethodView =
    publicRight(captured.method(captured.members.head.ref))

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def parameters(method: untpd.DefDef): List[untpd.ValDef] =
    method.paramss.head.map(_.asInstanceOf[untpd.ValDef])

  private def assertFreshByName(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val originalWrapper = byNameType(original)
    val rewrittenWrapper = byNameType(rewritten)
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!rewrittenWrapper.eq(originalWrapper))
    assertEquals(rewrittenWrapper.source, originalWrapper.source)
    assertEquals(rewrittenWrapper.span, originalWrapper.span)
    assert(!rewrittenWrapper.result.eq(originalWrapper.result))
    rewrittenWrapper.result match
      case ident: untpd.Ident =>
        assertEquals(ident.name.toString, "Int")
        assertEquals(ident.source, originalWrapper.result.source)
        assertEquals(ident.span, originalWrapper.result.span)
      case other => fail("expected by-name inner Int Ident, found " + other)

  private def assertFreshRepeated(original: untpd.ValDef, rewritten: untpd.ValDef): Unit =
    val originalWrapper = repeatedType(original)
    val rewrittenWrapper = repeatedType(rewritten)
    val (originalElement, originalMarker) = repeatedParts(originalWrapper)
    val (rewrittenElement, rewrittenMarker) = repeatedParts(rewrittenWrapper)
    assert(!rewritten.eq(original))
    assert(rewritten.mods.eq(original.mods))
    assertEquals(rewritten.mods.flags, Flags.Param)
    assert(!rewrittenWrapper.eq(originalWrapper))
    assertEquals(rewrittenWrapper.source, originalWrapper.source)
    assertEquals(rewrittenWrapper.span, originalWrapper.span)
    rewrittenElement match
      case ident: untpd.Ident =>
        assertEquals(ident.name.toString, "Int")
        assert(!ident.eq(originalElement))
        assertEquals(ident.source, originalElement.source)
        assertEquals(ident.span, originalElement.span)
      case other => fail("expected repeated element Int Ident, found " + other)
    assert(rewrittenMarker.eq(originalMarker))

  private def byNameType(parameter: untpd.ValDef): untpd.ByNameTypeTree =
    parameter.tpt match
      case value: untpd.ByNameTypeTree => value
      case other => fail("expected ByNameTypeTree, found " + other)

  private def repeatedType(parameter: untpd.ValDef): untpd.PostfixOp =
    parameter.tpt match
      case value: untpd.PostfixOp => value
      case other => fail("expected PostfixOp repeated Type, found " + other)

  private def repeatedParts(repeated: untpd.PostfixOp): (untpd.Tree, untpd.Tree) =
    repeated match
      case untpd.PostfixOp(element, marker) => (element, marker)

  private def assertUntouchedMember(result: Result, captured: Capture): Unit =
    assert(result.changed)
    assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U060MixedCallingModeEdits.scala", source)
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
