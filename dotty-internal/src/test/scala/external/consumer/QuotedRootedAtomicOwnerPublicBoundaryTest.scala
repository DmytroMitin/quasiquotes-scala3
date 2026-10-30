package external.consumer

import scala.compiletime.testing.typeCheckErrors

final class QuotedRootedAtomicOwnerPublicBoundaryTest extends munit.FunSuite:
  test("foreign public consumer cannot obtain the test-local rooted atomic transaction oracle"):
    val errors = typeCheckErrors("""
      val model = _root_.quasiquotes.definitions.dotty.QuotedRootedAtomicOwnerModel
    """)
    assert(errors.exists(_.message.contains("cannot be accessed")),clues(errors))
