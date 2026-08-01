package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource

import quasiquotes.definitions.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class DefinitionQuasiquoteBackendCompositionTest extends munit.FunSuite:
  import DefinitionQuasiquotes.*
  import DefinitionQuasiquoteTestFixtures.*

  test("dqr returns a completed value that callers lower explicitly through Phase 48") {
    val inserted = bodyTerm("1")
    val result = dqr"def answer: Int = $inserted".toOption.get
    val raw = ConstructedDefinitionUntypedBackend
      .lower(result.constructed)
      .fold(error => fail(error.message), identity)

    assert(raw.isInstanceOf[untpd.DefDef])
    assertEquals(raw.source, NoSource)
    assert(!raw.span.exists)
  }

  test("dqr returns a completed value that callers lower explicitly through Phase 49") {
    val inserted = bodyTerm("1")
    val result = dqr"val answer: Int = $inserted".toOption.get

    withContext {
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(result.constructed, "<phase52-dqr-generated-origin>")
        .fold(error => fail(error.message), identity)

      assertEquals(generated.generatedSource, "val answer: Int = 1")
      assertEquals(generated.tree.span.start, 0)
      assertEquals(generated.tree.span.end, generated.generatedSource.length)
      assert(
        GeneratedOriginFragmentSupport
          .allTrees(generated.tree)
          .forall(tree =>
            tree.source == generated.sourceFile &&
              tree.span.exists &&
              tree.span.start >= 0 &&
              tree.span.end <= generated.generatedSource.length &&
              tree.symbol == NoSymbol
          )
      )
    }
  }

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    body
