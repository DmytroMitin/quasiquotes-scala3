package quasiquotes.terms.dotty

class CoreTermShapeUntypedLowererTyperProbeTest extends munit.FunSuite:
  test("exact Typer accepts the source-free integer/infix trees as Int expressions") {
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.onePlusOne, 2)
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.nestedPrecedence, 7)
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.negativePlusTwo, 1)
  }

  test("ordinary Typer accepts source-free Identifier Select and one-list Apply over declared fixtures") {
    assert(CoreTermShapeUntypedLowererTyperProbe.identifierViable)
    assert(CoreTermShapeUntypedLowererTyperProbe.selectViable)
    assert(CoreTermShapeUntypedLowererTyperProbe.emptyApplyViable)
    assert(CoreTermShapeUntypedLowererTyperProbe.oneArgumentApplyViable)
    assert(CoreTermShapeUntypedLowererTyperProbe.multiArgumentApplyViable)
  }

  test("ordinary pre-Typer flow accepts nested source-free If Tuple Boolean String and Unary") {
    assert(CoreTermShapeUntypedLowererTyperProbe.nestedU004Viable)
  }

  test("ordinary pre-Typer flow accepts a nested source-free P1 Block and returns its result") {
    assertEquals(CoreTermShapeUntypedLowererTyperProbe.p1BlockResult, 4)
  }
