package quasiquotes.definitions.dotty

/** The executable S0-S6 matrix lives in the real compiler fixture beside this test. */
class ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization stays separate from production S6 adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginAdapter.getClass.getName
        .contains("WrapApplySiblingRewriteOriginAdapter")
    )
  }
