package external.consumer

final class Q048TermSequenceParityTest extends munit.FunSuite:
  test("opt-in qr shares standard Apply/New sequence semantics, topology, identity, and order"):
    assert(Q048TermSequenceParityMacros.direct(2))

  test("the opt-in umbrella export exposes the sequence-capable qr overload"):
    assert(Q048TermSequenceParityMacros.umbrella(2))
