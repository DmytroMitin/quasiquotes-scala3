package quasiquotes.terms.dotty

class CoreTermShapeUntypedLowererTyperProbeTest extends munit.FunSuite:
  test("exact Typer accepts the source-free integer/infix trees as Int expressions") {
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.onePlusOne, 2)
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.nestedPrecedence, 7)
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.negativePlusTwo, 1)
  }
