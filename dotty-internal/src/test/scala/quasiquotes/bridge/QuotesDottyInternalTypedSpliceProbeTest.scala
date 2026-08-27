package quasiquotes.bridge

final class QuotesDottyInternalTypedSpliceProbeTest extends munit.FunSuite:
  test("typed Expr leaves survive an untpd TypedSplice shell and public reflection conversion"):
    assertEquals(QuotesDottyInternalTypedSpliceProbe.add(20, 22), 42)
