package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q073ScalametaInfixSelectedNameFeasibilityTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("actual hybrid qq sources separate legal interpolation from selected infix role"):
    val fixed = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left + $right" => (left, right)
          case _ => ()"""
    )
    val candidate = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left $operatorName $right" => (left, operatorName, right)
          case _ => ()"""
    )
    val directComparator = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left.$operatorName($right)" => (left, operatorName, right)
          case _ => ()"""
    )
    assertEquals(fixed, Nil)
    assertEquals(candidate, Nil)
    assertEquals(directComparator, Nil)
    assert(Q073ScalametaInfixSelectedNameProbe.publicFixedMatches)
    assert(!Q073ScalametaInfixSelectedNameProbe.publicDynamicMatches)

  test("selected dialect parse and exact compiler validation admit the test-only operator transport"):
    assertEquals(
      Q073ScalametaInfixSelectedNameProbe.sourceObservation,
      List.fill(4)("true").mkString(":")
    )

  test("Scalameta-primary plan returns typed selected name and exact reflected operands"):
    assertEquals(
      Q073ScalametaInfixSelectedNameProbe.reflectedObservation,
      "+:true:true:true"
    )

  test("right-associated operator remains outside the bounded hybrid bridge"):
    assert(Q073ScalametaInfixSelectedNameProbe.rightAssociatedRejected)
