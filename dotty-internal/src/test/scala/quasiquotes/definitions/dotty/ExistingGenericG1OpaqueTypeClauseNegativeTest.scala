package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

final class ExistingGenericG1OpaqueTypeClauseNegativeTest extends munit.FunSuite:
  private final case class Rejected(
      label: String,
      method: String,
      publicCode: String = "UNSUPPORTED_STRUCTURE"
  )

  private val rejected = Vector(
    Rejected("two Type parameters", "def keep[A, B](x: Int): Int = x"),
    Rejected("bounded Type parameter", "def keep[A <: AnyVal](x: Int): Int = x"),
    Rejected("exact G2 dependent method", "def keep[A](x: A): A = x"),
    Rejected("dependent parameter Type", "def keep[A](x: A): Int = 1"),
    Rejected("dependent result Type", "def keep[A](x: Int): A = ???"),
    Rejected("Int binder shadows admitted primitive", "def keep[Int](x: Int): Int = x"),
    Rejected("String binder shadows admitted primitive", "def keep[String](x: String): String = x"),
    Rejected("Boolean binder shadows admitted primitive", "def keep[Boolean](x: Boolean): Boolean = x"),
    Rejected("AnyVal binder shadows admitted primitive", "def keep[AnyVal](x: AnyVal): AnyVal = x"),
    Rejected("contextual term clause", "def keep[A](using x: Int): Int = x"),
    Rejected("implicit term clause", "def keep[A](implicit x: Int): Int = x"),
    Rejected("defaulted term parameter", "def keep[A](x: Int = 1): Int = x"),
    Rejected("by-name term parameter", "def keep[A](x: => Int): Int = x"),
    Rejected("repeated term parameter", "def keep[A](x: Int*): Int = x.size"),
    Rejected("wide term clause", "def keep[A](x: Int, y: Int): Int = x"),
    Rejected("two term clauses", "def keep[A](x: Int)(y: Int): Int = x"),
    Rejected("parameterless generic", "def keep[A]: Int = 1", "INTERNAL_INVARIANT_FAILED"),
    Rejected("ordinary plus contextual", "def keep[A](x: Int)(using y: Int): Int = x"),
    Rejected("nonprimitive independent Type", "def keep[A](x: List[Int]): Int = x.size")
  )

  test("adjacent generic families remain publicly unsupported") {
    withContext {
      rejected.foreach { fixture =>
        val root = parseClass(
          s"class RejectedGeneric:\n  ${fixture.method}\n",
          fixture.label
        )
        val captured = ExistingClassUntypedRewrite.capture(root).fold(
          problem => fail(s"${fixture.label}: ${problem.message}"),
          identity
        )
        val failure = captured.method(captured.members.head.ref).left.toOption
          .getOrElse(fail(s"${fixture.label}: unexpectedly admitted"))
        assertEquals(failure.code, fixture.publicCode, fixture.label)
      }
    }
  }

  private def parseClass(source: String, label: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U068GenericNegative.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil, label)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
