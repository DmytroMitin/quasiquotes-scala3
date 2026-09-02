package quasiquotes.q012rr

import scala.compiletime.testing.typeCheckErrors

final class Q012RRScalableDefinitionPatternExtractorTest extends munit.FunSuite:
  test("static exact-two dqq exposes the non-arity-bound DefinitionPatternExtractor type"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{DefinitionPattern, DefinitionPatternExtractor}

        def compile(using q: Quotes): Unit =
          val pattern: DefinitionPatternExtractor = DefinitionPattern.dqq(
            StringContext("def first(left: Int, right: String): Int = ", "")
          )(using q)
          val _: q.reflect.DefDef => Option[q.reflect.Term] = pattern.unapply(using q)
      }"""
    )
    assertEquals(errors, Nil)
