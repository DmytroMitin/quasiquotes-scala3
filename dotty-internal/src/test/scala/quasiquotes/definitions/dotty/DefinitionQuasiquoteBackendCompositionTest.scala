package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource

import quasiquotes.definitions.*
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

class DefinitionQuasiquoteBackendCompositionTest extends munit.FunSuite:
  test("a completed core value lowers explicitly through the raw-tree backend SPI") {
    val raw = ConstructedDefinitionUntypedBackend
      .lower(methodFixture)
      .fold(error => fail(error.message), identity)

    assert(raw.isInstanceOf[untpd.DefDef])
    assertEquals(raw.source, NoSource)
    assert(!raw.span.exists)
  }

  test("a completed core value lowers explicitly through the generated-origin backend SPI") {
    withContext {
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(valueFixture, "<phase52-dqr-generated-origin>")
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

  private def methodFixture: ConstructedDefinition =
    ConstructedDefinition
      .parameterlessDef(
        DefinitionName.plain("answer").toOption.get,
        quasiquotes.types.TypeNormalForm.STypeIdent("Int"),
        quasiquotes.terms.ConstructedTerm
          .fromShape(quasiquotes.parser.TermShape.Literal("1"))
          .toOption
          .get
      )
      .toOption
      .get

  private def valueFixture: ConstructedDefinition =
    ConstructedDefinition
      .immutableVal(
        DefinitionName.plain("answer").toOption.get,
        quasiquotes.types.TypeNormalForm.STypeIdent("Int"),
        quasiquotes.terms.ConstructedTerm
          .fromShape(quasiquotes.parser.TermShape.Literal("1"))
          .toOption
          .get
      )
      .toOption
      .get

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    body
