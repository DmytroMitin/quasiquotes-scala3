package quasiquotes.definitions.dotty

import quasiquotes.neutral.NeutralProjectionError

class ExtensionModulePeerBridgeFailureMappingTest extends munit.FunSuite:
  test("unexpected neutral plan failures remain internal") {
    val classifier = ExtensionModulePeerBridge.getClass.getDeclaredMethod(
      "classifyProjectionFailure",
      classOf[NeutralProjectionError]
    )
    classifier.setAccessible(true)

    val failure = classifier
      .invoke(
        ExtensionModulePeerBridge,
        NeutralProjectionError(
          "NEUTRAL_EXTENSION_MODULE_PLAN_UNSUPPORTED",
          "BINDER_IDENTITY_EXHAUSTED: exhausted"
        )
      )
      .asInstanceOf[ExtensionModulePeerBridge.Failure]

    assertEquals(failure.code, "INTERNAL_INVARIANT_FAILED")
    assert(failure.detail.contains("BINDER_IDENTITY_EXHAUSTED"))
  }
