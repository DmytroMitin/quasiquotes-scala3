package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q070ScalametaReflectedBridgeTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("Scalameta-primary two-name plan extracts typed names and exact reflected Terms"):
    assertEquals(
      Q070ScalametaReflectedBridgeProbe.unaryObservation,
      "inner:outer:true:true"
    )

  test("direct unary and repeated plans remain Scalameta-primary with no fallback"):
    assertEquals(
      Q070ScalametaReflectedBridgeProbe.planObservation,
      "Scalameta:true:Scalameta:true:Scalameta:true"
    )

  test("inline source helper loses provenance and Q075 candidates fail closed"):
    val direct = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    val repeated = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    Vector(direct, unary).foreach { errors =>
      assert(errors.exists(_.contains("Invalid Scalameta qq term-pattern template")), clues(errors))
      assert(errors.exists(_.contains("Selected source occurrence index")), clues(errors))
      assert(!errors.exists(_.contains("selected-name capture is supported only")), clues(errors))
    }
    assert(repeated.exists(_.contains("duplicate pattern variable: same")), clues(repeated))
    assert(repeated.exists(_.contains("Selected source occurrence index")), clues(repeated))
