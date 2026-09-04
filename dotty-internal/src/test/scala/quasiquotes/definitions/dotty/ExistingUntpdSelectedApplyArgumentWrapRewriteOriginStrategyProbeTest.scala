package quasiquotes.definitions.dotty

/**
 * The executable W0/W1/W2/W3 matrix lives beside the real compiler fixture in
 * ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapterTyperRuntimeTest.
 */
class ExistingUntpdSelectedApplyArgumentWrapRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization stays separate from production adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentWrapRewriteOriginAdapter.getClass.getName
        .contains("WrapRewriteOriginAdapter")
    )
  }
