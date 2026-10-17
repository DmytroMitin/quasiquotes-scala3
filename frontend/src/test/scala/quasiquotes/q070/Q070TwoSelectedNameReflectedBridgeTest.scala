package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q070TwoSelectedNameReflectedBridgeTest extends munit.FunSuite:
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

  test("Q074 productionizes distinct lines while repeated source binders retain native E005"):
    val direct = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    val repeated = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.*
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    assertEquals(direct, Nil)
    assertEquals(unary, Nil)
    assert(repeated.exists(_.message.contains("duplicate pattern variable: same")), clues(repeated))
