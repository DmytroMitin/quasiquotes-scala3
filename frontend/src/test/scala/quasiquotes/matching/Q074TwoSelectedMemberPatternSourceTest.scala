package quasiquotes.matching

final class Q074TwoSelectedMemberPatternSourceTest extends munit.FunSuite:
  private val directParts = List("", ".", ".", "")
  private val unaryParts = List("", ".", ".", "(", ")")

  test("source planner preserves distinct lexical roles and reverse root-first ordinals"):
    val direct = TwoSelectedMemberPatternSource
      .classify(directParts, "$receiver.$first.$second")
      .toOption
      .flatten
      .get
    val unary = TwoSelectedMemberPatternSource
      .classify(unaryParts, "$receiver.$first.$second($argument)")
      .toOption
      .flatten
      .get

    assertEquals(direct.layout, TwoSelectedMemberPatternLayout.Direct)
    assertEquals(direct.receiverName, "receiver")
    assertEquals(direct.argumentName, None)
    assertEquals(
      direct.pattern,
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_first__selected_1"),
        "__qqhole_second__selected_0"
      )
    )
    assertEquals(unary.layout, TwoSelectedMemberPatternLayout.Unary)
    assertEquals(unary.receiverName, "receiver")
    assertEquals(unary.argumentName, Some("argument"))

  test("planner rejects missing roles and Term-name category conflicts"):
    assert(TwoSelectedMemberPatternSource.classify(directParts, ".$first.$second").isLeft)
    assert(
      TwoSelectedMemberPatternSource
        .classify(directParts, "$receiver.$receiver.$second")
        .isLeft
    )

  test("duplicate selected source spelling reaches Scala's native E005 stage"):
    val plan = TwoSelectedMemberPatternSource
      .classify(directParts, "$receiver.$same.$same")
      .toOption
      .flatten
    assert(plan.nonEmpty)

  test("unselected layouts stay outside the standard-only early route"):
    Vector(
      List("", ".", ""),
      List("", ".", "()"),
      List("", ".", "(", ")"),
      List("", ".", ".", ".", ""),
      List("", ".", ".", "(..", ")")
    ).foreach(parts =>
      assertEquals(TwoSelectedMemberPatternSource.classify(parts, "$unused"), Right(None))
    )
