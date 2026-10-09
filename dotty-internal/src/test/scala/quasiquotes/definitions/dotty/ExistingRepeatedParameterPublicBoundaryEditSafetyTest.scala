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

class ExistingRepeatedParameterPublicBoundaryEditSafetyTest extends munit.FunSuite:
  private val IntType = TypeNormalForm.STypeIdent("Int")

  test("body-only replacement preserves the exact repeated parameter and PostfixOp") {
    withContext {
      val root = parseClass(
        "class BodyOnlyRepeated:\n  def sum(xs: Int*): Int = 0\n  val untouched: Int = 7\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalRepeated = repeatedType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(
        captured.emptyPlan.replaceBody(method.ref, TermShape.Literal("7"))
      )
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalRepeated))
      val rewrittenRepeated = repeatedType(rewrittenParameter)
      val (originalElement, originalMarker) = repeatedParts(originalRepeated)
      val (rewrittenElement, rewrittenMarker) = repeatedParts(rewrittenRepeated)
      assert(rewrittenElement.eq(originalElement))
      assert(rewrittenMarker.eq(originalMarker))
      assert(rewritten.tpt.eq(originalMethod.tpt))
      assert(!rewritten.rhs.eq(originalMethod.rhs))
      assertEquals(rewritten.rhs.source, originalMethod.rhs.source)
      assertEquals(rewritten.rhs.span, originalMethod.rhs.span)
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("result-Type-only replacement preserves the exact repeated parameter and PostfixOp") {
    withContext {
      val root = parseClass(
        "class ResultOnlyRepeated:\n  def sum(xs: Int*): AnyVal = xs.sum\n  val untouched: Int = 9\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalRepeated = repeatedType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(captured.emptyPlan.replaceResultType(method.ref, IntType))
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalRepeated))
      rewritten.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected rewritten result Int Ident, found " + other)
      assert(!rewritten.tpt.eq(originalMethod.tpt))
      assertEquals(rewritten.tpt.source, originalMethod.tpt.source)
      assertEquals(rewritten.tpt.span, originalMethod.tpt.span)
      assert(rewritten.rhs.eq(originalMethod.rhs))
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("current unary parameter-Type replacement drops repeated mode") {
    withContext {
      val root = parseClass(
        "class ParameterTypeRepeated:\n  def sum(xs: AnyVal*): Int = 0\n  val untouched: Int = 11\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalRepeated = repeatedType(originalParameter)
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
      assert(!rewrittenParameter.tpt.eq(originalRepeated))
      assert(!rewrittenParameter.tpt.isInstanceOf[untpd.PostfixOp])
      rewrittenParameter.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected scalar replacement Int Ident, found " + other)
      assertEquals(rewrittenParameter.tpt.source, originalRepeated.source)
      assertEquals(rewrittenParameter.tpt.span, originalRepeated.span)
      assert(rewritten.tpt.eq(originalMethod.tpt))
      assert(rewritten.rhs.eq(originalMethod.rhs))
      assert(result.directMemberIdentities(1).sameObjectAs(captured.members(1).identity))
      assertPreTyperClean(result.tree)
    }
  }

  test("the two-parameter public path has the same repeated-last mode loss") {
    withContext {
      val root = parseClass(
        "class TwoParameterRepeatedEdit:\n  def combine(prefix: Int, xs: AnyVal*): Int = prefix\n  val untouched: Int = 13\n"
      )
      val originalMethod = firstMethod(root)
      val originalFirst = originalMethod.paramss.head.head.asInstanceOf[untpd.ValDef]
      val originalSecond = originalMethod.paramss.head(1).asInstanceOf[untpd.ValDef]
      val originalRepeated = repeatedType(originalSecond)
      val captured = capture(root)
      val method = methodView(captured)
      val repeated = method.parameterClauses.head.parameters(1)
      val plan = publicRight(
        captured.emptyPlan.replaceParameterType(repeated.ref, IntType)
      )
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenFirst = rewritten.paramss.head.head.asInstanceOf[untpd.ValDef]
      val rewrittenSecond = rewritten.paramss.head(1).asInstanceOf[untpd.ValDef]

      assertFreshMethodShell(result, root, originalMethod, rewritten)
      assert(rewrittenFirst.eq(originalFirst))
      assert(rewrittenFirst.tpt.eq(originalFirst.tpt))
      assert(!rewrittenSecond.eq(originalSecond))
      assert(!rewrittenSecond.tpt.eq(originalRepeated))
      assert(!rewrittenSecond.tpt.isInstanceOf[untpd.PostfixOp])
      rewrittenSecond.tpt match
        case untpd.Ident(name) => assertEquals(name.toString, "Int")
        case other => fail("expected scalar repeated-slot replacement Int Ident, found " + other)
      assertEquals(rewrittenSecond.tpt.source, originalRepeated.source)
      assertEquals(rewrittenSecond.tpt.span, originalRepeated.span)
      assertPreTyperClean(result.tree)
    }
  }

  test("safe body and result edits compose without changing repeated topology") {
    withContext {
      val root = parseClass(
        "class ComposedSafeRepeated:\n  def sum(xs: Int*): AnyVal = 0\n  val untouched: Int = 15\n"
      )
      val originalMethod = firstMethod(root)
      val originalParameter = firstParameter(originalMethod)
      val originalRepeated = repeatedType(originalParameter)
      val captured = capture(root)
      val method = methodView(captured)
      val plan = publicRight(for
        p1 <- captured.emptyPlan.replaceResultType(method.ref, IntType)
        p2 <- p1.replaceBody(method.ref, TermShape.Literal("7"))
      yield p2)
      val result = publicRight(ExistingClassUntypedRewrite(captured, plan))
      val rewritten = firstMethod(result.tree)
      val rewrittenParameter = firstParameter(rewritten)
      assert(rewrittenParameter.eq(originalParameter))
      assert(rewrittenParameter.tpt.eq(originalRepeated))
      assertEquals(rewritten.tpt.source, originalMethod.tpt.source)
      assertEquals(rewritten.tpt.span, originalMethod.tpt.span)
      assertEquals(rewritten.rhs.source, originalMethod.rhs.source)
      assertEquals(rewritten.rhs.span, originalMethod.rhs.span)
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

  private def repeatedType(parameter: untpd.ValDef): untpd.PostfixOp =
    parameter.tpt match
      case value: untpd.PostfixOp => value
      case other => fail("expected PostfixOp repeated Type, found " + other)

  private def repeatedParts(repeated: untpd.PostfixOp): (untpd.Tree, untpd.Tree) =
    repeated match
      case untpd.PostfixOp(elementType, repeatedMarker) => (elementType, repeatedMarker)

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
    val unit = CompilationUnit("U057ExistingRepeatedEdit.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def publicRight[A](value: Either[Failure, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
