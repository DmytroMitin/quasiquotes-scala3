package external.consumer

final class Q047RuntimeTypeApplicationParityTest extends munit.FunSuite:
  test("opt-in runtime-sequence tqr has standard semantic, structural, and identity parity"):
    assert(Q047RuntimeTypeApplicationParityMacros.direct)

  test("the opt-in umbrella export exposes the runtime-sequence overload"):
    assert(Q047RuntimeTypeApplicationParityMacros.umbrella)
