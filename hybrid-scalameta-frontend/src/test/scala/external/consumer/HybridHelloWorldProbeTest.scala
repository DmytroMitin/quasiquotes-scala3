package external.consumer

final class HybridHelloWorldProbeTest extends munit.FunSuite:
  test("current and Scalameta routes compose in both same-route and mixed-route forms"):
    assertEquals(HybridHelloWorldProbeMacros.currentBuildCurrentMatch(20, 22), 42)
    assertEquals(HybridHelloWorldProbeMacros.scalametaBuildScalametaMatch(20, 22), 42)
    assertEquals(HybridHelloWorldProbeMacros.currentBuildScalametaMatch(20, 22), 42)
    assertEquals(HybridHelloWorldProbeMacros.scalametaBuildCurrentMatch(20, 22), 42)
    assertEquals(HybridHelloWorldProbeMacros.scalametaFacadeBuildMatch(20, 22), 42)
