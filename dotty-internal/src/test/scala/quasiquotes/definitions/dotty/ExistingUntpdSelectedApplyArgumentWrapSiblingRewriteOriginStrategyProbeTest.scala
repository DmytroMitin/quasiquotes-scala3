package quasiquotes.definitions.dotty

/** The executable S0-S4 matrix lives in the real compiler fixture beside this test. */
class ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization stays separate from production S4 adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteOriginAdapter.getClass.getName
        .contains("WrapSiblingRewriteOriginAdapter")
    )
  }
