package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q073SingleSelectedInfixOperatorNameFeasibilityTest extends munit.FunSuite:
  import Q073OperandFixture.{context, left, right}

  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("actual standard qq sources separate legal interpolation from selected infix role"):
    val fixed = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left + $right" => (left, right)
          case _ => ()"""
    )
    val candidate = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left $operatorName $right" => (left, operatorName, right)
          case _ => ()"""
    )
    val directComparator = messages(
      """import scala.quoted.*; import quasiquotes.matching.QuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$left.$operatorName($right)" => (left, operatorName, right)
          case _ => ()"""
    )
    assertEquals(fixed, Nil)
    assertEquals(candidate, Nil)
    assertEquals(directComparator, Nil)
    assert(Q073InfixSelectedNameConsumers.publicFixedInfixMatches(left, right))
    assert(!Q073InfixSelectedNameConsumers.publicDynamicInfixMatches(left, right))

  test("test-only infix role returns a static Term SelectedMemberName Term product"):
    assertEquals(
      Q073InfixSelectedNameConsumers.selectedPlus(left, right),
      "+:true:true:true:true:true"
    )
    assertEquals(
      Q073InfixSelectedNameConsumers.explicitPlus(left, right),
      "+:true:true:true:true:true"
    )
    assertEquals(
      Q073InfixSelectedNameConsumers.selectedPlus(right, left),
      "+:true:true:true:true:true"
    )
    assertEquals(
      Q073InfixSelectedNameConsumers.selectedMinus(left, right),
      "-:true"
    )

  test("accepted direct selected call is a semantic comparator with the same reflected target"):
    assertEquals(
      Q073InfixSelectedNameConsumers.directSelectedComparator(left, right),
      "+:true:true"
    )

  test("source plan records exact selected position while public classifier keeps it scalar"):
    assertEquals(
      Q073InfixSelectedNameProbe.sourceRoleObservation,
      "left,operatorName,right|left:0-5,operatorName:6-19,right:20-26|true|" +
        "Infix(Hole($q073Left), +++, Hole($q073Right))|true"
    )

  test("typed target evidence distinguishes raw Apply Select from bounded normalized Infix"):
    assertEquals(
      Q073InfixSelectedNameConsumers.targetShapeObservation(left, right, context),
      List.fill(10)("true").mkString(":")
    )

  test("right association clauses TypeApply named args and nonselected names fail atomically"):
    assertEquals(
      Q073InfixSelectedNameConsumers.strictNegativeObservation(left, right, context),
      List.fill(13)("true").mkString(":")
    )

  test("Q decoded selected-name grammar is wider than N neutral plain-name intersection"):
    assertEquals(
      Q073InfixSelectedNameProbe.lexicalObservation,
      List.fill(7)("true").mkString(":")
    )

  test("accepted direct unary rank2 and P3 classifier paths do not collide with infix parts"):
    val infix = SelectedMemberPatternSource.classifyIncludingRanked(List("", " ", " ", ""))
    val direct = SelectedMemberPatternSource.classifyIncludingRanked(List("", ".", ""))
    val unary = SelectedMemberPatternSource.classifyIncludingRanked(List("", ".", "(", ")"))
    val ranked = SelectedMemberPatternSource.classifyIncludingRanked(List("", ".", "(..", ")"))
    val p3 = SelectedMemberPatternSource.classifyIncludingRanked(
      List("{ def local(value: Int): Int = value; local(", ") }")
    )
    assertEquals(infix, Right(None))
    assert(direct.toOption.flatten.nonEmpty)
    assert(unary.toOption.flatten.nonEmpty)
    assert(ranked.toOption.flatten.nonEmpty)
    assertEquals(p3, Right(None))
