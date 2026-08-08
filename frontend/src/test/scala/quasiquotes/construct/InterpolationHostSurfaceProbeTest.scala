package quasiquotes.construct

import InterpolationHostSurfaceProbe.*

private object InterpolationHostSurfaceExamples:
  private val x = 1

  val wholeArgument = hostProbe"""s"hello $x""""
  val literalGuestIdentifier = hostProbe"""s"hello $$name""""
  val guestExpression = hostProbe"""s"value = $${foo(x)}""""
  val nestedOuterHole = hostProbe"""s"value = $${foo($x)}""""
  val literalGuestDoubleDollar = hostProbe"""s"literal $$$$ dollar""""

class InterpolationHostSurfaceProbeTest extends munit.FunSuite:
  test("outer interpolation exposes exact parts for an entire guest argument hole") {
    assertEquals(InterpolationHostSurfaceExamples.wholeArgument.parts, List("s\"hello ", "\""))
    assertEquals(InterpolationHostSurfaceExamples.wholeArgument.argumentCount, 1)
    assertEquals(InterpolationHostSurfaceExamples.wholeArgument.argumentSource, List("x"))
  }

  test("doubled dollars survive the outer layer as one literal guest dollar") {
    assertEquals(
      InterpolationHostSurfaceExamples.literalGuestIdentifier,
      InterpolationHostSurfaceEvidence(List("s\"hello $name\""), 0, Nil)
    )
    assertEquals(
      InterpolationHostSurfaceExamples.guestExpression,
      InterpolationHostSurfaceEvidence(List("s\"value = ${foo(x)}\""), 0, Nil)
    )
  }

  test("a nested outer hole remains separately ordered inside guest braces") {
    assertEquals(
      InterpolationHostSurfaceExamples.nestedOuterHole.parts,
      List("s\"value = ${foo(", ")}\"")
    )
    assertEquals(InterpolationHostSurfaceExamples.nestedOuterHole.argumentCount, 1)
    assertEquals(InterpolationHostSurfaceExamples.nestedOuterHole.argumentSource, List("x"))
  }

  test("four outer dollars preserve a guest doubled-dollar literal spelling") {
    assertEquals(
      InterpolationHostSurfaceExamples.literalGuestDoubleDollar,
      InterpolationHostSurfaceEvidence(List("s\"literal $$ dollar\""), 0, Nil)
    )
  }
