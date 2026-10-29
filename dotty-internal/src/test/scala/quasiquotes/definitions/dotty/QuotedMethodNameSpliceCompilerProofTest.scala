package quasiquotes.definitions.dotty

import scala.compiletime.testing.typeCheckErrors

final class QuotedMethodNameSpliceCompilerProofTest extends munit.FunSuite:
  test("literal quote Name cannot splice as Term while explicit Term.Name narrowing compiles"):
    val errors = typeCheckErrors("""
      import scala.meta.*
      def replacement(tree: Defn.Def): Defn.Def = tree match {
        case q"def echo($value: AnyVal): AnyVal = $body" =>
          q"def echo($value: Int): Int = Math.abs($value)"
      }
    """)
    assert(errors.nonEmpty)
    assert(errors.exists(e => e.message.contains("Term") && e.message.contains("Name")), clues(errors))
    val accepted = typeCheckErrors("""
      import scala.meta.*
      def replacement(tree: Defn.Def): Defn.Def = tree match {
        case q"def echo($value: AnyVal): AnyVal = $body" => value match {
          case term: Term.Name => q"def echo($term: Int): Int = Math.abs($term)"
          case _ => throw new IllegalArgumentException()
        }
        case _ => throw new IllegalArgumentException()
      }
    """)
    assertEquals(accepted, Nil)
