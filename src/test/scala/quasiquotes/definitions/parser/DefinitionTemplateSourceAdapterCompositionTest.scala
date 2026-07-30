package quasiquotes.definitions.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.dotty.{
  ConstructedDefinitionGeneratedOriginAdapter,
  GeneratedOriginDefinitionResult
}
import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

class DefinitionTemplateSourceAdapterCompositionTest
    extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*

  private def occurrence(
      name: String,
      category: DefinitionTemplateHoleCategory
  ) =
    CategorizedDefinitionHoleOccurrence(name, category)

  private def term(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).toOption.get

  test("categorized source completes and lowers through the Phase 49 generated-origin adapter") {
    val located =
      DefinitionTemplateSourceAdapter
        .parseLocated(
          "def `type`: List[$T] = if $condition then ($left: Option[$T]) else $right",
          Vector(
            occurrence("T", DefinitionType),
            occurrence("condition", BodyTerm),
            occurrence("left", BodyTerm),
            occurrence("T", BodyType),
            occurrence("right", BodyTerm)
          )
        )
        .fold(error => fail(error.diagnostic.message), identity)
    val completed =
      located
        .complete(
          Map(
            "condition" -> term(
              TermShape.Identifier("ready", false)
            ),
            "left" -> term(TermShape.Literal("1")),
            "right" -> term(TermShape.Literal("2"))
          ),
          Map("T" -> TypeNormalForm.STypeIdent("String"))
        )
        .fold(error => fail(error.diagnostic.message), identity)

    withContext {
      val result =
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(completed, "<phase50-definition-composition>")
          .fold(error => fail(error.message), identity)

      assertEquals(
        result.generatedSource,
        "def `type`: List[String] = if ready then ((1): Option[String]) else 2"
      )
      assertEquals(
        result.sourceFile.content().mkString,
        result.generatedSource
      )
      assertComplete(result)
      assert(
        GeneratedOriginFragmentSupport
          .allTrees(result.tree)
          .forall(_.symbol == NoSymbol)
      )
      assert(
        !GeneratedOriginFragmentSupport
          .allTrees(result.tree)
          .exists(_.isInstanceOf[untpd.TypedSplice])
      )
    }
  }

  private def assertComplete(
      result: GeneratedOriginDefinitionResult
  )(using Context): Unit =
    val trees =
      GeneratedOriginFragmentSupport.allTrees(result.tree)
    assertEquals(result.tree.span.start, 0)
    assertEquals(result.tree.span.end, result.generatedSource.length)
    trees.foreach { tree =>
      assert(tree.source.exists)
      assertEquals(tree.source, result.sourceFile)
      assert(!tree.span.isSynthetic)
      assert(tree.span.start >= 0)
      assert(tree.span.end <= result.generatedSource.length)
    }

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    body
