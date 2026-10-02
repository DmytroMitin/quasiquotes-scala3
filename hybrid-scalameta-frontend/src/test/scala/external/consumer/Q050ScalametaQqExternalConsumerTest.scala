package external.consumer

final class Q050ScalametaQqExternalConsumerTest extends munit.FunSuite:
  test("external consumers use the direct same-spelling ranked qq selector"):
    assertEquals(Q050ScalametaQqExternalConsumerMacros.direct(2), List(1, 2, 3))

  test("external consumers use the umbrella same-spelling ranked qq selector"):
    assertEquals(Q050ScalametaQqExternalConsumerMacros.umbrella(2), List(1, 2, 3))
