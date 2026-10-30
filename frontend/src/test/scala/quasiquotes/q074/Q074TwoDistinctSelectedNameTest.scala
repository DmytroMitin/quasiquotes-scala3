package quasiquotes.q074

import scala.compiletime.testing.typeCheckErrors

final class Q074TwoDistinctSelectedNameTest extends munit.FunSuite:
  test("public standard qq captures two distinct selected-name binders in source order"):
    assertEquals(
      Q074TwoDistinctSelectedNameProbe.observation,
      "inner:outer:inner:outer:same:same:true:true:true:true"
    )

  test("target topology name decoding and null failures are atomic"):
    assertEquals(
      Q074TwoDistinctSelectedNameProbe.negativeObservation,
      List.fill(6)("true").mkString(":")
    )

  test("Q target lexing and generated-looking literal names remain target data"):
    assertEquals(
      Q074TwoDistinctSelectedNameProbe.lexicalObservation,
      "type:safe spaced name:inner:__qqhole_first__selected_1:+:type"
    )

  test("same source binder remains native E005 and is not reinterpreted as equality"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    assert(errors.exists(_.message.contains("duplicate pattern variable: same")), clues(errors))
    assert(!errors.exists(_.message.contains("Invalid qq term-pattern template")), clues(errors))

  test("category collisions and ranked layouts remain negative; direct three-name now compiles"):
    val categoryCollision = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$receiver.$second" => (receiver, second)
          case _ => ()"""
    )
    val threeNames = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second.$third" => (receiver, first, second, third)
          case _ => ()"""
    )
    val rankedOverlap = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second(..$arguments)" => ()
          case _ => ()"""
    )
    assert(categoryCollision.exists(_.message.contains("used by both an ordinary Term hole")),
      clues(categoryCollision))
    assertEquals(threeNames, Nil)
    assert(rankedOverlap.nonEmpty, clues(rankedOverlap))
