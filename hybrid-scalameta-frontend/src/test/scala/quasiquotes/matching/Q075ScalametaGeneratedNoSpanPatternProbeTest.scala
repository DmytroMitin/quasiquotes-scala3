package quasiquotes.matching

final class Q075ScalametaGeneratedNoSpanPatternProbeTest extends munit.FunSuite:
  test("generated no-span noncandidate remains on the legacy Scalameta route"):
    assertEquals(
      Q075ScalametaGeneratedNoSpanPatternProbe.legacyScalarRouting,
      "source=none;static=true;legacy-routed"
    )

  test("generated no-span true candidate fails with the selected-name source category"):
    val observation = Q075ScalametaGeneratedNoSpanPatternProbe.twoNameRouting
    assert(observation.startsWith("source=none;static=true;failure:"), observation)
    assert(
      observation.contains(
        "two-selected-name source-role planning requires recoverable original interpolation source"
      ),
      observation
    )
