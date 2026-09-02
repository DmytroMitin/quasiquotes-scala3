package external.consumer

import scala.quoted.staging.{Compiler, withQuotes}
import scala.meta.dialects

import quasiquotes.scalameta.TermFrontend

class ScalametaOptInSurfaceTest extends munit.FunSuite:
  test("distinct Scalameta import host constructs literals and preserves reflected holes"):
    assertEquals(ScalametaOptInMacros.constructed, (42, 1))

  test("distinct Scalameta pattern host preserves ordered reflected captures"):
    assertEquals(ScalametaOptInMacros.matched, (20, 22))

  test("Scalameta pattern host preserves a generated NoSpan subtree by identity"):
    assert(ScalametaOptInMacros.generatedCaptureIsOriginal)

  test("Scalameta opt-in block construction preserves caller-owned prefix and result children"):
    assert(ScalametaOptInMacros.blockConstructionPreservesChildren)

  test("Scalameta opt-in block matching returns original generated prefix and result children"):
    assert(ScalametaOptInMacros.blockCapturesAreOriginal)

  test("programmatic opt-in reports parse-only fallback without global state"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      TermFrontend.build(Seq("if true then 1 else 2"), Nil, dialects.Scala213)
    assertEquals(result.map(_.engine), Right(TermFrontend.Engine.CurrentDottyFallback))
    assert(result.toOption.flatMap(_.primaryFailure).exists(_.category == "SCALAMETA_PARSE_FAILURE"))

  test("existing current-Dotty qr and qq imports remain independently callable"):
    assertEquals(ScalametaOptInMacros.currentDefaultControl, (42, (20, 22)))

  test("Scalameta selected-call overlap applies explicit nullary calls exactly once"):
    val receiver = new ScalametaNullarySelectedCallTarget(40)
    assertEquals(ScalametaOptInMacros.constructorCapacity(16), 16)
    assertEquals(ScalametaOptInMacros.ordinaryNullary(receiver), 41)
    assertEquals(ScalametaOptInMacros.dynamicNullary(receiver), 41)

  test("external opt-in Definition construction and matching preserve owners and body identity"):
    assert(ScalametaOptInMacros.definitionRoundTrip)
    assert(ScalametaOptInMacros.definitionMismatchFallsThrough)
