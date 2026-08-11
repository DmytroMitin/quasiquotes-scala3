package quasiquotes.terms.dotty

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm

class ConstructorNewBackendBoundaryTest extends munit.FunSuite:
  private val constructed = ConstructedTerm.fromShape(
    TermShape.New(
      "java.lang.StringBuilder",
      List(TermShape.Literal("16"))
    )
  ).toOption.get

  test("source-free exact backend rejects constructor new deterministically") {
    assertEquals(
      ConstructedTermUntypedBackend.lower(constructed),
      Left(ConstructedTermUntypedBackendError.UnsupportedTermNode("New"))
    )
  }

  test("generated-origin exact backend rejects constructor new deterministically") {
    val base = new ContextBase
    given Context = base.initialCtx
    assertEquals(
      ConstructedTermGeneratedOriginAdapter.lower(
        constructed,
        "<constructor-new-phase75-boundary>"
      ),
      Left(ConstructedTermGeneratedOriginError.UnsupportedTermNode("New"))
    )
  }
