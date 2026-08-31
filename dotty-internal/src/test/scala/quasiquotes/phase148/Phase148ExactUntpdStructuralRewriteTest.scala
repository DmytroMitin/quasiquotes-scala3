package quasiquotes.phase148

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

class Phase148ExactUntpdStructuralRewriteTest extends munit.FunSuite:
  private val Source =
    """@marker
      |class C:
      |  def keep: Int = 1
      |  def change: Int = 2
      |  val opaque: String = "opaque"
      |  def call: Int = service.invoke(oldArg, keptArg)
      |""".stripMargin

  test("reconstructs a class body while preserving exact holes and honest provenance") {
    withContext {
      val result = ExactUntpdStructuralRewriteProbe.rewrite(Source)

      assertEquals(result.originalClass.name.toString, "C")
      assertEquals(result.bodyNames, List("keep", "change", "opaque", "call"))
      assertEquals(result.rebuiltBodyNames, result.bodyNames)

      assert(!result.rebuiltClass.eq(result.originalClass))
      assert(!result.rebuiltTemplate.eq(result.originalTemplate))
      assert(!result.replacementChange.eq(result.originalChange))

      assert(result.rebuiltKeep.eq(result.originalKeep))
      assert(result.rebuiltOpaque.eq(result.originalOpaque))
      assert(result.rebuiltCall.eq(result.originalCall))
      assert(result.rebuiltAnnotation.eq(result.originalAnnotation))
      assert(result.rebuiltClass.mods.eq(result.originalClass.mods))
      assert(result.replacementChange.mods.eq(result.originalChange.mods))

      List[untpd.Tree](
        result.originalKeep,
        result.originalOpaque,
        result.originalCall,
        result.originalAnnotation
      ).foreach { preserved =>
        assert(preserved.source.exists, clues(preserved.getClass.getSimpleName))
        assert(preserved.span.exists, clues(preserved.getClass.getSimpleName))
      }

      List[untpd.Tree](
        result.rebuiltClass,
        result.rebuiltTemplate,
        result.replacementChange,
        result.replacementBody
      ).foreach { reconstructed =>
        assert(!reconstructed.source.exists, clues(reconstructed.getClass.getSimpleName))
        assert(!reconstructed.span.exists, clues(reconstructed.getClass.getSimpleName))
      }

      assertEquals(
        result.provenanceKinds,
        List("preserved", "reconstructed", "opaque-preserved")
      )
    }
  }

  test("captures and splices the repeated body without copying untouched members") {
    withContext {
      val result = ExactUntpdStructuralRewriteProbe.rewrite(Source)

      assertEquals(result.prefix.size, 1)
      assert(result.prefix.head.eq(result.originalKeep))
      assertEquals(result.suffix.size, 2)
      assert(result.suffix.head.eq(result.originalOpaque))
      assert(result.suffix(1).eq(result.originalCall))

      result.rebuiltTemplate.body match
        case keep :: change :: opaque :: call :: Nil =>
          assert(keep.eq(result.originalKeep))
          assert(change.eq(result.replacementChange))
          assert(opaque.eq(result.originalOpaque))
          assert(call.eq(result.originalCall))
        case other => fail(s"unexpected rebuilt body: $other")
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
