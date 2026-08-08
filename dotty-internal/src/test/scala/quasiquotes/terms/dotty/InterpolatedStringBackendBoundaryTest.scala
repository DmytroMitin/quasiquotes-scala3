package quasiquotes.terms.dotty

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm

class InterpolatedStringBackendBoundaryTest extends munit.FunSuite:
  private val shape = TermShape.InterpolatedString(
    "s",
    List("hello ", ""),
    List(TermShape.Identifier("name", isPlaceholder = false))
  )
  private val constructed = ConstructedTerm.fromShape(shape).toOption.get

  test("exact untyped backend rejects the new shape deterministically") {
    assertEquals(
      ConstructedTermUntypedBackend.lower(constructed),
      Left(ConstructedTermUntypedBackendError.UnsupportedTermNode("InterpolatedString"))
    )
  }

  test("generated-origin planner rejects the new shape deterministically") {
    assertEquals(
      GeneratedOriginFragmentSupport.planTerm(constructed),
      Left(ConstructedTermGeneratedOriginError.UnsupportedTermNode("InterpolatedString"))
    )
  }
