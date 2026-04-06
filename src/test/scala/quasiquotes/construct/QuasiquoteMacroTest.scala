package quasiquotes.construct

class QuasiquoteMacroTest extends munit.FunSuite:
  test("qr can emit an integer literal as a Term") {
    assertEquals(QuasiquoteMacroExamples.emitIntLiteral, 1)
  }

  test("qr can emit a string literal as a Term") {
    assertEquals(QuasiquoteMacroExamples.emitStringLiteral, "abc")
  }

  test("qr can lower selection plus application with a qualifier hole") {
    assertEquals(QuasiquoteMacroExamples.callSelectedMethodViaHole(2), 3)
  }

  test("qr can use a hole in function position") {
    assertEquals(QuasiquoteMacroExamples.callFunctionHole(2), 3)
  }

  test("qr can use a hole as the qualifier of a selection") {
    assertEquals(QuasiquoteMacroExamples.stringLength("abcd"), 4)
  }

  test("unsupported syntax fails clearly") {
    assert(QuasiquoteMacroExamples.unsupportedSyntaxMessage.contains("Unsupported"))
  }
