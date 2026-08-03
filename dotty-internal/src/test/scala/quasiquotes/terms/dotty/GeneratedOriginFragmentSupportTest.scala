package quasiquotes.terms.dotty

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.util.SourceFile

import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class GeneratedOriginFragmentSupportTest extends munit.FunSuite:
  import TypeNormalForm.*

  test("positions a term fragment at a nonzero caller-owned base offset") {
    withContext {
      val constructed =
        ConstructedTerm
          .fromShape(
            TinyTermParser
              .parseOrThrow("if ready then left + 1 else -(-1)")
              .shape
          )
          .toOption
          .get
      val fragment =
        GeneratedOriginFragmentSupport.planTerm(constructed).toOption.get
      val prefix = "val value: Int = "
      val source =
        SourceFile.virtual(
          "<generated-fragment-term-offset>",
          prefix + fragment.source
        )
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val positioned =
        GeneratedOriginFragmentSupport
          .positionTerm(raw, fragment, source, prefix.length)
          .toOption
          .get

      assertEquals(positioned.span.start, prefix.length)
      assertEquals(positioned.span.end, prefix.length + fragment.source.length)
      GeneratedOriginFragmentSupport.allTrees(positioned).foreach { tree =>
        assertEquals(tree.source.path, source.path)
        assert(tree.span.start >= prefix.length)
        assert(tree.span.end <= prefix.length + fragment.source.length)
      }
    }
  }

  test("positions a nested type fragment at a nonzero caller-owned base offset") {
    withContext {
      val normalForm =
        STypeApply(
          STypeIdent("List"),
          List(
            STypeFunction(
              List(STypeIdent("Int"), STypeIdent("String")),
              STypeIdent("Boolean")
            )
          )
        )
      val fragment =
        GeneratedOriginFragmentSupport.planType(normalForm).toOption.get
      val prefix = "def value: "
      val source =
        SourceFile.virtual(
          "<generated-fragment-type-offset>",
          prefix + fragment.source + " = body"
        )
      val raw = CompletedTypeUntypedLowerer.lower(normalForm).toOption.get
      val positioned =
        GeneratedOriginFragmentSupport
          .positionType(raw, fragment, source, prefix.length)
          .toOption
          .get

      assertEquals(positioned.span.start, prefix.length)
      assertEquals(positioned.span.end, prefix.length + fragment.source.length)
      GeneratedOriginFragmentSupport.allTrees(positioned).foreach { tree =>
        assertEquals(tree.source.path, source.path)
        assert(tree.span.start >= prefix.length)
        assert(tree.span.end <= prefix.length + fragment.source.length)
      }
    }
  }

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body
