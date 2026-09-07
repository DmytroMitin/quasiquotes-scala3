package external.consumer

// snippet:semantic-term-origin:start
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermParameterSpec, TermShapeBindings}
import quasiquotes.terms.dotty.TermGeneratedOriginLowering
import quasiquotes.types.TypeNormalForm

object SemanticTermOriginExample:
  def check(): Unit =
    given Context = new ContextBase().initialCtx
    val semantic = TermShapeBindings.lambda(
      Vector(TermParameterSpec("x", TypeNormalForm.STypeIdent("Int")))
    ) { scope =>
      scope.reference(scope.parameterBinders.head.head)
    }.fold(error => sys.error(error.message), identity)

    val result = TermGeneratedOriginLowering
      .lower(semantic, "generated/Identity.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    assert(result.tree.isInstanceOf[untpd.Function])
    assert(result.generatedSource == "(x: Int) => x")
    assert(result.virtualSourceName == "generated/Identity.scala")
    assert(result.sourceFile.content.mkString == result.generatedSource)
    assert(result.tree.source eq result.sourceFile)
    assert(result.tree.span.start == 0)
    assert(result.tree.span.end == result.generatedSource.length)
// snippet:semantic-term-origin:end

final class SemanticTermOriginFirstUseTest extends munit.FunSuite:
  test("documented semantic Term origin example executes") {
    SemanticTermOriginExample.check()
  }
