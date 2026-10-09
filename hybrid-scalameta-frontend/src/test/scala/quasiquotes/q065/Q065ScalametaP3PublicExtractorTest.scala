package quasiquotes.q065

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.scalameta.TermFrontend
import Q065ScalametaP3PublicExtractorMacros.*

private object Q065ScalametaP3PublicExtractorFixtures:
  val exact = intEvidence {
    def id(value: Int): Int = value
    id(41)
  }

  val alphaRenamed = intEvidence {
    def different(x: Int): Int = x
    different(42)
  }

  val ambient = intEvidence {
    def different(x: Int): Int = x
    different(Q065ScalametaP3Targets.ambient)
  }

  val stringIdentity = captureString {
    def different(x: String): String = x
    different("value")
  }

  val booleanIdentity = captureBoolean {
    def different(x: Boolean): Boolean = x
    different(true)
  }

  val nestedLambda = nestedLambdaMatches {
    def renamed(v: Int): Int = v
    renamed((((m: Int) => renamed(m)), 41)._2)
  }

  val collisionSafe = captureCollisionSafe {
    def renamed(v: Int): Int = v
    renamed((Q065ScalametaP3Targets.__qqhole_argument, 43)._2)
  }

final class Q065ScalametaP3PublicExtractorTest extends munit.FunSuite:
  import Q065ScalametaP3PublicExtractorFixtures.*

  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("public typed-Scalameta qq matches exact alpha-renamed and ambient Int identity methods"):
    List(exact, alphaRenamed, ambient).foreach { evidence =>
      assert(evidence.value > 0, evidence)
      assert(evidence.exactArgumentIdentity, evidence)
      assert(evidence.distinctBinderSymbols, evidence)
      assert(evidence.bodyUsesParameterSymbol, evidence)
      assert(evidence.resultUsesMethodSymbol, evidence)
    }
    assertEquals(exact.value, 41)
    assertEquals(alphaRenamed.value, 42)
    assertEquals(ambient.value, 41)

  test("public extractor supports equal fixed String and Boolean Types"):
    assertEquals(stringIdentity, "value")
    assertEquals(booleanIdentity, true)

  test("public extractor preserves collision-safe transport and Q063R nested Lambda1 matching"):
    assertEquals(collisionSafe, 43)
    assertEquals(nestedLambda, true)

  test("same semantic hole repetitions retain equality and reject unequal targets atomically"):
    assert(repeatedHoleMatches)
    assert(repeatedHoleMismatchRejects)

  test("wrong target bodies calls Types topology and final calls do not bind"):
    negativeTargetMatches.zipWithIndex.foreach { case (matched, index) =>
      assert(!matched, s"negative target $index unexpectedly matched")
    }
    assertEquals(escapedParameterMatches, false)

  test("semantic rejection stays Scalameta-owned and never falls back to current Dotty"):
    val rejected = List(
      "{ def id(value: Int): Int = 1; id($argument) }",
      "{ def id(value: Int): Int = value; identity($argument) }",
      "{ def id(value: Int): String = value.toString; id($argument) }",
      "{ def id(value: Double): Double = value; id($argument) }",
      "{ val seed: Int = 1; def id(value: Int): Int = value; id($argument) }",
      "{ def first(value: Int): Int = value; def second(value: Int): Int = value; first($argument) }",
      "{ def id[A](value: Int): Int = value; id[Int]($argument) }",
      "{ def id(value: Int) = value; id($argument) }",
      "{ def id(): Int = 1; id() }",
      "{ def id(left: Int, right: Int): Int = left; id($argument, 1) }",
      "{ def id()(value: Int): Int = value; id()($argument) }",
      "{ def id(value: Int)(other: Int): Int = value; id($argument)(1) }",
      "{ def id(using value: Int): Int = value; id(using $argument) }",
      "{ implicit def id(value: Int): Int = value; id($argument) }",
      "{ def id(value: => Int): Int = value; id($argument) }",
      "{ def id(value: Int*): Int = value.head; id($argument) }",
      "{ def id(value: Int = 1): Int = value; id($argument) }",
      "{ inline def id(value: Int): Int = value; id($argument) }",
      "{ @deprecated(\"unsupported\", \"Q065\") def id(value: Int): Int = value; id($argument) }",
      "{ def id(@deprecated(\"unsupported\", \"Q065\") value: Int): Int = value; id($argument) }",
      "{ def id(value: Int): Int = { def nested(x: Int): Int = x; nested(value) }; id($argument) }",
      "foo({ def id(value: Int): Int = value; id($argument) })"
    )
    rejected.foreach { source =>
      val failure = TermFrontend.compile(source).swap.toOption.getOrElse(fail(s"admitted: $source"))
      assertEquals(failure.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED", source)
    }

  test("P3 requires exactly one consumed semantic scalar hole and ignores generated-looking identifiers"):
    val zero = TermFrontend.compile(
      "{ def id(value: Int): Int = value; id(__qqhole_argument) }"
    ).swap.toOption.getOrElse(fail("zero-hole P3 source was admitted"))
    val two = TermFrontend.compile(
      "{ def id(value: Int): Int = value; id(($first, $second)._1) }"
    ).swap.toOption.getOrElse(fail("two-hole P3 source was admitted"))
    assertEquals(zero.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED")
    assertEquals(two.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED")

  test("dynamic StringContext retains the existing scalar extractor type"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.{ScalametaQuasiPattern, ScalametaTermPatternExtractor}
        def probe(using q: Quotes)(context: StringContext) =
          val extractor: ScalametaTermPatternExtractor[q.reflect.Term] =
            ScalametaQuasiPattern.qq(context)(using q)
          extractor"""
    )
    assertEquals(errors, Nil)
