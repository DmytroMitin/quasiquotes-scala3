package quasiquotes.definitions.dotty

/**
 * The executable P0/P1/P2/P3 strategy matrix lives beside the real compiler
 * fixture in ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapterTyperRuntimeTest.
 * This named suite locks the U015 strategy-owner boundary in the focused matrix.
 */
class ExistingUntpdSelectedApplyArgumentApplyRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization is kept separate from production adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentApplyRewriteOriginAdapter.getClass.getName
        .contains("ApplyRewriteOriginAdapter")
    )
  }
