package quasiquotes.q071

import scala.compiletime.testing.typeCheckErrors

final class Q071RepeatedSelectedNameLanguageBlockerTest extends munit.FunSuite:
  test("Scala rejects a repeated interpolation pattern binder before extractor matching"):
    val errors = typeCheckErrors(
      """{
        final class DeduplicatedCaptureExtractor:
          def unapply(value: Int): Option[(Int, String)] = None

        extension (sc: StringContext)
          def q071stub: DeduplicatedCaptureExtractor =
            new DeduplicatedCaptureExtractor

        def probe(value: Int) = value match
          case q071stub"$receiver.$same.$same" => (receiver, same)
          case _ => (value, "")
      }"""
    )

    assert(
      errors.exists(_.message.contains("duplicate pattern variable: same")),
      clues(errors)
    )

  test("the exact repeated public qq spelling remains native-E005 compile-negative"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*

        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => throw new IllegalArgumentException("no match")
      }"""
    )

    assert(
      errors.exists(_.message.contains("duplicate pattern variable: same")),
      clues(errors)
    )
    assert(!errors.exists(_.message.contains("Invalid qq term-pattern template")), clues(errors))
