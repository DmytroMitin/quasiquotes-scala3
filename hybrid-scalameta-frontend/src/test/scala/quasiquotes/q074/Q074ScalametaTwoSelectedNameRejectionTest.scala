package quasiquotes.q074

import scala.compiletime.testing.typeCheckErrors

final class Q074ScalametaTwoSelectedNameRejectionTest extends munit.FunSuite:
  test("typed-Scalameta direct and unary two-name forms retain their classifier rejection"):
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
    Vector(direct, unary).foreach { errors =>
      assert(errors.exists(_.message.contains("Invalid Scalameta qq term-pattern template")),
        clues(errors))
      assert(errors.exists(_.message.contains("selected-name capture is supported only")),
        clues(errors))
    }
