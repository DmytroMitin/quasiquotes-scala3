package quasiquotes.q001

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.matching.QuasiPattern
import quasiquotes.parser.TinyTermParser

private object Q001SequenceExtractorTypingProbeScope:
  private def combine(left: Int, right: Int): Int = left + right
  private val first = 20
  private val second = 22

  val captured: (Int, List[Int]) =
    Q001SequenceExtractorTypingProbe.captureTypesAndValues(combine(first, second))
  val scalarCaptured: (Int, Int) =
    Q001SequenceExtractorTypingProbe.scalarCaptureTypesAndValues(first + second)

class Q001SequenceExtractorTypingProbeTest extends munit.FunSuite:
  test("rank-aware product extraction keeps scalar and sequence capture types distinct"):
    val (functionRenderingLength, arguments) = Q001SequenceExtractorTypingProbeScope.captured
    assert(functionRenderingLength > 0)
    assertEquals(arguments, List(20, 22))

  test("the existing scalar qq extractor retains scalar Term capture types"):
    assertEquals(Q001SequenceExtractorTypingProbeScope.scalarCaptured, (20, 22))

  test("current rank text fails before compilation while marker consumption yields an ordinary Apply tree"):
    assert(QuasiPattern.term("$fun(..$args)").isLeft)
    assertEquals(
      TinyTermParser
        .parse("__qqhole_fun(__qqhole_args)")
        .map(_.shape.render),
      Right("Apply(Ident(__qqhole_fun), [Ident(__qqhole_args)])")
    )

  test("the sequence capture does not typecheck as a scalar Term"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q001.Q001RankAwareProbe.*

        def rejectSequenceAsScalar(using q: Quotes)(term: q.reflect.Term): Unit =
          term match
            case qq"$fun(..$args)" =>
              val _: q.reflect.Term = args
            case _ => ()
      }"""
    )

    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("q.reflect.Term")))
