package quasiquotes.q075

import scala.compiletime.testing.typeCheckErrors

final class Q075ScalametaTwoDistinctSelectedNameTest extends munit.FunSuite:
  test("public typed-Scalameta qq captures two distinct selected-name binders in source order"):
    assertEquals(
      Q075ScalametaTwoDistinctSelectedNameProbe.observation,
      "inner:outer:inner:outer:same:same:true:true:true:true"
    )

  test("target topology name decoding and null failures are atomic"):
    assertEquals(
      Q075ScalametaTwoDistinctSelectedNameProbe.negativeObservation,
      List.fill(6)("true").mkString(":")
    )

  test("Q target lexing and generated-looking literal names remain target data"):
    assertEquals(
      Q075ScalametaTwoDistinctSelectedNameProbe.lexicalObservation,
      "type:safe spaced name:inner:__qqhole_first__selected_1:+:type"
    )

  test("public extractor result types remain the existing heterogeneous product"):
    val errors = typeCheckErrors(
      """import scala.quoted.*
        import quasiquotes.construct.SelectedMemberName
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def direct(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" =>
            val result: (q.reflect.Term, SelectedMemberName, SelectedMemberName) =
              (receiver, first, second)
            result
          case _ => throw new IllegalArgumentException("no match")
        def unary(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" =>
            val r: q.reflect.Term = receiver
            val f: SelectedMemberName = first
            val s: SelectedMemberName = second
            val a: q.reflect.Term = argument
            (r, f, s, a)
          case _ => throw new IllegalArgumentException("no match")
      """
    )
    assertEquals(errors, Nil)

  test("same source binder remains native E005 and is not reinterpreted as equality"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    assert(errors.exists(_.message.contains("duplicate pattern variable: same")), clues(errors))
    assert(!errors.exists(_.message.contains("Invalid Scalameta qq term-pattern template")), clues(errors))

  test("unsupported wider and marker layouts stay fail-closed"):
    val threeNames = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second.$third" => (receiver, first, second, third)
          case _ => ()"""
    )
    val repeatedMarker = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.#repeat" => (receiver, first)
          case _ => ()"""
    )
    assertEquals(threeNames, Nil)
    assert(repeatedMarker.exists(_.message.contains("Invalid Scalameta qq term-pattern template")),
      clues(repeatedMarker))
