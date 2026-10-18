package quasiquotes.matching

final class Q075ScalametaTwoSelectedMemberPatternSourceTest extends munit.FunSuite:
  private val directParts = List("", ".", ".", "")
  private val unaryParts = List("", ".", ".", "(", ")")

  test("Scalameta planner preserves source roles and reverse root-first ordinals"):
    val direct = ScalametaTwoSelectedMemberPatternSource
      .classify(directParts, "$receiver.$first.$second")
      .toOption
      .flatten
      .get
    val unary = ScalametaTwoSelectedMemberPatternSource
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

  test("Scalameta planner rejects missing roles and category conflicts"):
    assert(ScalametaTwoSelectedMemberPatternSource.classify(directParts, ".$first.$second").isLeft)
    assert(
      ScalametaTwoSelectedMemberPatternSource
        .classify(directParts, "$receiver.$receiver.$second")
        .isLeft
    )
    assert(
      ScalametaTwoSelectedMemberPatternSource
        .classify(unaryParts, "$receiver.$first.$second()")
        .isLeft
    )

  test("unsupported source binder lexemes fail before target-name decoding"):
    Vector(
      "$receiver.$type.$second",
      "$receiver.$+.$second",
      "$receiver.$naïve.$second"
    ).foreach(source =>
      assert(
        ScalametaTwoSelectedMemberPatternSource.classify(directParts, source).isLeft,
        source
      )
    )

  test("noncandidate layouts neither compile nor request source"):
    Vector(
      List("", ".", ""),
      List("", ".", "()"),
      List("", ".", "(", ")"),
      List("", ".", ".", ".", ""),
      List("", ".", ".", "(..", ")"),
      List("", " + ", "")
    ).foreach { parts =>
      var sourceRequested = false
      val classified = ScalametaTwoSelectedMemberPatternSource.classify(
        parts,
        {
          sourceRequested = true
          "$unused"
        }
      )
      assertEquals(classified, Right(None), parts)
      assertEquals(sourceRequested, false, parts)
    }
