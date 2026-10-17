package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.types.TypeNormalForm

final class ExistingClassUntypedRewriteOptionAppliedTypeFeasibilityTest extends munit.FunSuite:
  test("foreign package observes the exact current public Option[String] failure without a private bypass"):
    withContext:
      val root = parseClass(
        """class OptionService:
          |  val untouchedBefore: Int = 1
          |  def echo(value: String): String = value
          |  val untouchedAfter: Int = 2
          |""".stripMargin
      )
      val captured = right(ExistingClassUntypedRewrite.capture(root))
      val method = right(captured.method(captured.members(1).ref))
      val parameter = method.parameterClauses.head.parameters.head
      val body = right(method.parameterScope.reference(parameter.ref))
      val optionString = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Option"),
        List(TypeNormalForm.STypeIdent("String"))
      )
      val plan = right(for
        p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, optionString)
        p2 <- p1.replaceResultType(method.ref, optionString)
        p3 <- p2.replaceBody(method.ref, body)
      yield p3)

      assertEquals(plan.editCount, 3)
      ExistingClassUntypedRewrite(captured, plan) match
        case Left(problem) =>
          assertEquals(problem.code, "UNSUPPORTED_STRUCTURE")
          assert(problem.detail.contains("U040 SEMANTIC_TYPE_FAMILY_UNSUPPORTED"), clues(problem))
          assert(problem.detail.contains("STypeApply"), clues(problem))
        case Right(value) => fail("Option[String] unexpectedly became a public capability: " + value)

  private def right[A](result: Either[Failure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("ExternalU071OptionAppliedType.scala", source)
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
    run(using (new ContextBase).initialCtx)
