package quasiquotes.terms.dotty

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm

class Lambda1BackendBoundaryTest extends munit.FunSuite:
  test("exact untyped backend keeps Lambda1 explicitly deferred") {
    val binderId = BinderId(0)
    val lambda = ConstructedTerm.fromShape(
      TermShape.Lambda1(
        binderId,
        "x",
        "Int",
        TermShape.BoundReference(binderId, "x")
      )
    ).toOption.get

    val failure = ConstructedTermUntypedBackend.lower(lambda).swap.toOption.get

    assertEquals(
      failure,
      ConstructedTermUntypedBackendError.UnsupportedTermNode("Lambda1")
    )
    assert(failure.message.contains("exact-version untyped backend boundary"))
  }
