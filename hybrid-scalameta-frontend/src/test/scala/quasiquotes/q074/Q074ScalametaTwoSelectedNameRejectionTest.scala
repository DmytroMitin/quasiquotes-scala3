package quasiquotes.q074

import scala.compiletime.testing.typeCheckErrors

final class Q074ScalametaTwoSelectedNameRejectionTest extends munit.FunSuite:
  test("Q075 admits the typed-Scalameta direct and unary forms left unsupported by Q074"):
    val direct = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    assertEquals(direct, Nil)
    assertEquals(unary, Nil)
