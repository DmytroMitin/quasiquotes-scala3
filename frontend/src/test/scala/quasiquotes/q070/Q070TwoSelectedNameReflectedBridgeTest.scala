package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q070TwoSelectedNameReflectedBridgeTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("test-only reflected bridge returns two typed names and the original receiver"):
    assertEquals(
      Q070ReflectedBridgeProbe.distinctDirectObservation,
      "inner:outer:true"
    )

  test("unary bridge returns source-ordered typed names and exact Term identities"):
    assertEquals(
      Q070ReflectedBridgeProbe.distinctUnaryObservation,
      "inner:outer:true:true"
    )

  test("repeated selected occurrences enforce equality and return one logical binding"):
    assertEquals(
      Q070ReflectedBridgeProbe.repeatedObservation,
      "same:true:true"
    )

  test("source plan keeps lexical order while nested Select ordinals are root-first"):
    assertEquals(
      Q070ReflectedBridgeProbe.sourcePlanObservation,
      "receiver,first,second,argument|first:1,second:0|true|true"
    )

  test("source and reflected target failures are deterministic and atomic"):
    assertEquals(
      Q070ReflectedBridgeProbe.negativeObservation,
      List.fill(14)("true").mkString(":")
    )

  test("Q target lexing stays wider than the neutral occurrence transport policy"):
    assertEquals(
      Q070ReflectedBridgeProbe.lexicalObservation,
      "true:true:true:true:true:true"
    )

  test("current public standard qq continues to reject all modeled two-name lines"):
    val direct = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    val repeated = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    Vector(direct, unary, repeated).foreach { errors =>
      assert(errors.exists(_.contains("selected-name capture is supported only")), clues(errors))
    }
