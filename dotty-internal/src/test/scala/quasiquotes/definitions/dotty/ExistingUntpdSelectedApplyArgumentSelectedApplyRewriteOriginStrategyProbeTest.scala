package quasiquotes.definitions.dotty

/**
 * The executable P0/P1/P2/P3/P4 strategy matrix lives beside the real compiler
 * fixture in ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapterTyperRuntimeTest.
 * This named suite locks the U016 strategy-owner boundary in the focused matrix.
 */
class ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginStrategyProbeTest
    extends munit.FunSuite:
  test("strategy characterization is kept separate from production adaptation") {
    assert(
      ExistingUntpdSelectedApplyArgumentSelectedApplyRewriteOriginAdapter.getClass.getName
        .contains("SelectedApplyRewriteOriginAdapter")
    )
  }
