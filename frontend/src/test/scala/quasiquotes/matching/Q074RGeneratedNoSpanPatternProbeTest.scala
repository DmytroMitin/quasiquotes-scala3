package quasiquotes.matching

final class Q074RGeneratedNoSpanPatternProbeTest extends munit.FunSuite:
  test("generated source-free legacy scalar pattern bypasses two-name source recovery"):
    assertEquals(
      Q074RGeneratedNoSpanPatternProbe.legacyScalarRouting,
      "source=none;static=true;legacy-routed"
    )

  test("generated source-free two-name candidate fails with a categorized diagnostic"):
    assert(
      Q074RGeneratedNoSpanPatternProbe.twoNameRouting.contains(
        "two-selected-name source-role planning requires recoverable original interpolation source"
      ),
      Q074RGeneratedNoSpanPatternProbe.twoNameRouting
    )
