package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Capture, Failure, MethodView, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingByNameParameterPublicBoundaryEditSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("body-only replacement preserves the exact by-name parameter and wrapper") {
    withContext {
      val root = parseClass(
        "class BodyOnlyByName:\n  def eval(x: => Int): Int = 0\n  val untouched: Int = 7\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalWrapper = byNameType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val parameter = method.parameterClauses.head.parameters.head
      val reference = publicRight(method.parameterScope.reference(parameter.ref))
      val body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
        List(reference, reference)
      )
      val plan = publicRight(captured.emptyPlan.replaceBody(method.ref, body))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalWrapper))
      assert(byNameType(rewrittenParameter).result.eq(originalWrapper.result))
      assert(rewritten.tpt.eq(originalMethod.tpt))
      assert(!rewritten.rhs.eq(originalMethod.rhs))
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("result-Type-only replacement preserves the exact by-name parameter and wrapper") {
    withContext {
      val root = parseClass(
        "class ResultOnlyByName:\n  def eval(x: => Int): AnyVal = x + x\n  val untouched: Int = 9\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalWrapper = byNameType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(captured.emptyPlan.replaceResultType(method.ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalWrapper))
      rewritten.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected rewritten result Int Ident, found " + other)
      assert(!rewritten.tpt.eq(originalMethod.tpt))
      assert(rewritten.rhs.eq(originalMethod.rhs))
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("current unary parameter-Type replacement drops the by-name wrapper") {
    withContext {
      val root = parseClass(
        "class ParameterTypeByName:\n  def eval(x: => AnyVal): Int = 0\n  val untouched: Int = 11\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalWrapper = byNameType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val parameter = method.parameterClauses.head.parameters.head
      val plan = publicRight(captured.emptyPlan.replaceParameterType(parameter.ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(!rewrittenParameter.eq(originalParameter))
      assertEquals(rewrittenParameter.mods.flags, Flags.Param)
      assert(rewrittenParameter.mods.eq(originalParameter.mods))
      assert(!rewrittenParameter.tpt.eq(originalWrapper))
      assert(!rewrittenParameter.tpt.isInstanceOf[untpd.ByNameTypeTree])
      rewrittenParameter.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected strict replacement Int Ident, found " + other)
      assertEquals(rewrittenParameter.tpt.source, originalWrapper.source)
      assertEquals(rewrittenParameter.tpt.span, originalWrapper.span)
      assert(rewritten.tpt.eq(originalMethod.tpt))
      assert(rewritten.rhs.eq(originalMethod.rhs))
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("the two-parameter public path has the same parameter-Type wrapper loss") {
    withContext {
      val root = parseClass(
        "class TwoParameterByNameEdit:\n  def combine(x: => AnyVal, y: Int): Int = y\n  val untouched: Int = 13\n"
      )
      val originalMethod = firstMethod(root)
      val originalFirst = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
      val originalSecond = originalMethod.paramss.head(1).asInstanceOf[untpd.ValDef]
      val originalWrapper = byNameType(originalFirst)
      val captured = capture(root)
      val method = methodView(captured)
      val first = method.parameterClauses.head.parameters.head
      val plan = publicRight(captured.emptyPlan.replaceParameterType(first.ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenFirst = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
      val rewrittenSecond = rewritten.paramss.head(1).asInstanceOf[untpd.ValDef]

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(!rewrittenFirst.eq(originalFirst))
      assert(!rewrittenFirst.tpt.eq(originalWrapper))
      assert(!rewrittenFirst.tpt.isInstanceOf[untpd.ByNameTypeTree])
      rewrittenFirst.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected strict first-parameter Int Ident, found " + other)
      assert(rewrittenSecond.eq(originalSecond))
      assert(rewrittenSecond.tpt.eq(originalSecond.tpt))
      assertPreTyperClean(result.tree)
    }
  }

  test("safe body and result edits compose without changing by-name topology") {
    withContext {
      val root = parseClass(
        "class ComposedSafeByName:\n  def eval(x: => Int): AnyVal = 0\n  val untouched: Int = 15\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalWrapper = byNameType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val parameter = method.parameterClauses.head.parameters.head
      val reference = publicRight(method.parameterScope.reference(parameter.ref))
      val plan = publicRight(for
        p1 <- captured.emptyPlan.replaceResultType(method.ref, IntType)
        p2 <- p1.replaceBody(method.ref, TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "addExact"),
          List(reference, reference)
        ))
      yield p2)
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalWrapper))
      assertPreTyperClean(result.tree)
    }
  }

  private def capture(root: untpd.TypeDef)(using Context): Capture =
    publicRight(ExistingClassUntypedRewrite.capture(root))

  private def methodView(captured: Capture)(using Context): MethodView =
    publicRight(captured.method(captured.members.head.ref))

  private def firstMethod(root: untpd.TypeDef)(using Context): untpd.DefDef =
    root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def firstParameter(method: untpd.DefDef): untpd.ValDef =
    method.paramss.head.head.asInstanceOf[untpd.ValDef]

  private def byNameType(parameter: untpd.ValDef): untpd.ByNameTypeTree =
    parameter.tpt match
      case value: untpd.ByNameTypeTree => value
      case other => fail("expected ByNameTypeTree, found " + other)

  private def assertFreshMethodShell(
      result: Result,
      originalRoot: untpd.TypeDef,
      originalMethod: untpd.DefDef,
      rewritten: untpd.DefDef
  ): Unit =
    assert(result.changed)
    assert(!result.tree.eq(originalRoot))
    assert(!rewritten.eq(originalMethod))
    assert(rewritten.mods.eq(originalMethod.mods))
    assertEquals(rewritten.source, originalMethod.source)
    assertEquals(rewritten.span, originalMethod.span)

  private def assertPreTyperClean(tree: untpd.Tree)(using Context): Unit =
    val trees = ExistingUntpdClassMemberFilter.allTrees(tree)
    assert(trees.forall(_.symbol == NoSymbol))
    assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U056ExistingByNameEdit.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
