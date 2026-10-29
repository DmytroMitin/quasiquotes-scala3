package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.terms.TermShapeBindingView
import scala.meta.*

/** Public final-plan control only: this does not claim a shipped quote-view/topology bridge. */
final class QuotedExistingMethodFinalPublicPlanConsumerTest extends munit.FunSuite:
  test("foreign consumer proves output own binder and executes the current public final plan"):
    given Context = (new ContextBase).initialCtx
    val root = parseClass("class ForeignService { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val after: Int = 2 }")
    val c = right(ExistingClassUntypedRewrite.capture(root))
    val member = c.members.filter(_.diagnosticName.contains("echo")).head
    val method = right(c.method(member.ref))
    val parameter = method.parameterClauses.head.parameters.head
    val replacement = q"def echo(value: Int): Int = Math.abs(value)"
    val projected = ScalametaDefinitionProjection.project(replacement).fold(p => fail(p.message), identity)
    val output = projected.definition.asMethod.getOrElse(fail("expected method"))
    val outputBinder = output.parameterScope.binder(0, 0).fold(p => fail(p.message), identity)
    val (function, argument) = output.body.getOrElse(fail("expected body")) match
      case TermShape.Apply(function, List(argument)) => (function, argument)
      case other => fail(s"expected unary call, found $other")
    val argumentView = TermShapeBindingView.inspect(argument).fold(p => fail(p.message), identity)
    val bound = argumentView.boundReference.getOrElse(fail("output argument must be bound"))
    assertEquals(bound.binder, outputBinder)
    val originalReference = right(method.parameterScope.reference(parameter.ref))
    val mappedBody = TermShape.Apply(function, List(originalReference))
    val plan = right(for
      p1 <- c.emptyPlan.replaceParameterType(parameter.ref, output.parameterClauses.head.parameters.head.declaredType)
      p2 <- p1.replaceResultType(method.ref, output.resultType)
      p3 <- p2.replaceBody(method.ref, mappedBody)
    yield p3)
    assertEquals(plan.editCount, 3)
    val result = right(ExistingClassUntypedRewrite(c, plan))
    assert(result.changed)
    assert(!result.tree.eq(root))
    assert(result.directMemberIdentities(0).sameObjectAs(c.members(0).identity))
    assert(!result.directMemberIdentities(1).sameObjectAs(c.members(1).identity))
    assert(result.directMemberIdentities(2).sameObjectAs(c.members(2).identity))
    val rewritten = result.tree.rhs.asInstanceOf[untpd.Template].body(1).asInstanceOf[untpd.DefDef]
    assertEquals(rewritten.tpt.asInstanceOf[untpd.Ident].name.toString, "Int")
    rewritten.rhs match
      case untpd.Apply(untpd.Select(untpd.Ident(qualifier), selected), List(untpd.Ident(argumentName))) =>
        assertEquals(qualifier.toString, "Math")
        assertEquals(selected.toString, "abs")
        assertEquals(argumentName.toString, "value")
      case other => fail(s"expected transferred qualified call, found $other")
    assert(rewritten.symbol == NoSymbol)

  private def right[A](result: Either[Failure, A]): A = result.fold(p => fail(p.message), identity)
  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val unit = CompilationUnit("ForeignQuotedFinalPlan.scala", source)
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assert(reporter.pendingMessages.isEmpty)
    parsed.stats.head.asInstanceOf[untpd.TypeDef]
