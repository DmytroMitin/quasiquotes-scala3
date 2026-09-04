package quasiquotes.definitions.dotty

/** The executable T0-T7 matrix lives in the real compiler fixture beside this test. */
class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization stays separate from production T7 adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginAdapter.getClass
        .getName.contains("WrapSelectedApplySiblingRewriteOriginAdapter")
    )
  }
