package external.consumer

import scala.compiletime.testing.typeCheckErrors

final class PrivateNamedInvocationBoundaryTest extends munit.FunSuite:
  test("foreign Scala callers cannot access private role or entrypoints"):
    val errors=typeCheckErrors("quasiquotes.neutral.ScalametaNamedInvocation.project(null)")
    assert(errors.nonEmpty)
    assert(errors.exists(e => e.message.contains("cannot be accessed") || e.message.contains("Access")), clues(errors))
