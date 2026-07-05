package quasiquotes.construct

private object NamedInfixScope:
  private val foo = 2
  private val bar = 5
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.namedInfixSummary

private object NamedSelectInfixScope:
  private object foo:
    val bar = 4
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.namedSelectInfixSummary(3)

private object NestedNamedApplicationScope:
  private def foo(value: Int): Int = value + 10
  private def bar(value: Int): Int = value * 2
  private val baz = 3
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedNamedApplicationSummary

private object NestedSelectApplicationScope:
  private object foo:
    def bar(value: Int): Int = value + 4
  private def baz(value: Int): Int = value * 3
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedSelectApplicationSummary(2)

private object ParenthesizedNamedScope:
  private val foo = 11
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.parenthesizedNamedSummary

private object ParenthesizedSelectedHoleScope:
  private object foo:
    def bar(value: Int): Int = value + 6
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.parenthesizedSelectedHoleSummary(3)

private object NestedParenHoleScope:
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedParenHoleSummary(7)

private object TupleApplicationScope:
  private def foo(value: (Int, Int)): Int = value._1 + value._2
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.tupleApplicationSummary(2, 3)

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

  test("qr can construct an infix expression from two holes") {
    assertEquals(QuasiquoteMacroExamples.addHoles(2, 3), 5)
  }

  test("qr can construct nested applications with holes") {
    assertEquals(QuasiquoteMacroExamples.nestedFunctionHoles(2), 5)
  }

  test("qr accepts parenthesized infix expressions") {
    assertEquals(QuasiquoteMacroExamples.parenthesizedAdd(2, 3), 5)
  }

  test("qr can construct a typed hole expression") {
    assertEquals(QuasiquoteMacroExamples.typedHole(2), 2)
  }

  test("qr can construct an application with a typed hole argument") {
    assertEquals(QuasiquoteMacroExamples.typedHoleApplication(2), 3)
  }

  test("qr can construct a tuple expression from holes") {
    assertEquals(QuasiquoteMacroExamples.tupleHoles(2, 3), (2, 3))
  }

  test("qr can construct a nested tuple expression") {
    assertEquals(QuasiquoteMacroExamples.nestedTupleHoles(2, 3, 4), (2, (3, 4)))
  }

  test("qr can construct an application with a tuple argument") {
    assertEquals(TupleApplicationScope.demo.input, "foo(($x, $y))")
    assertEquals(TupleApplicationScope.demo.placeholderSource, "foo((__hole0, __hole1))")
    assert(TupleApplicationScope.demo.treeStructure.contains("Apply"))
    assertEquals(TupleApplicationScope.demo.substitutedResult, "5")
  }

  test("demo summary for hole infix expressions is usable") {
    val demo = QuasiquoteMacroExamples.holeInfixSummary(2, 3)
    assertEquals(demo.input, "$x + $y")
    assertEquals(demo.placeholderSource, "__hole0 + __hole1")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "5")
  }

  test("demo summary for named infix expressions resolves caller scope identifiers") {
    val demo = NamedInfixScope.demo
    assertEquals(demo.input, "foo + bar")
    assertEquals(demo.placeholderSource, "foo + bar")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "7")
  }

  test("demo summary for select plus infix expressions supports mixed named and hole input") {
    val demo = NamedSelectInfixScope.demo
    assertEquals(demo.input, "foo.bar + $x")
    assertEquals(demo.placeholderSource, "foo.bar + __hole0")
    assert(demo.treeStructure.contains("Select"))
    assertEquals(demo.substitutedResult, "7")
  }

  test("demo summary for nested named applications stays usable") {
    val demo = NestedNamedApplicationScope.demo
    assertEquals(demo.input, "foo(bar(baz))")
    assertEquals(demo.placeholderSource, "foo(bar(baz))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "16")
  }

  test("demo summary for nested select applications stays usable") {
    val demo = NestedSelectApplicationScope.demo
    assertEquals(demo.input, "foo.bar(baz($x))")
    assertEquals(demo.placeholderSource, "foo.bar(baz(__hole0))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "10")
  }

  test("demo summary for parenthesized named identifiers stays usable") {
    val demo = ParenthesizedNamedScope.demo
    assertEquals(demo.input, "(foo)")
    assertEquals(demo.placeholderSource, "(foo)")
    assert(demo.treeStructure.contains("Ident"))
    assertEquals(demo.substitutedResult, "11")
  }

  test("demo summary for parenthesized selected holes stays usable") {
    val demo = ParenthesizedSelectedHoleScope.demo
    assertEquals(demo.input, "(foo.bar($x))")
    assertEquals(demo.placeholderSource, "(foo.bar(__hole0))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "9")
  }

  test("demo summary for nested parenthesized hole expressions stays usable") {
    val demo = NestedParenHoleScope.demo
    assertEquals(demo.input, "$f(($x))")
    assertEquals(demo.placeholderSource, "__hole0((__hole1))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "8")
  }

  test("demo summary for parenthesized infix expressions stays usable") {
    val demo = QuasiquoteMacroExamples.parenthesizedInfixSummary(4, 5)
    assertEquals(demo.input, "($x + $y)")
    assertEquals(demo.placeholderSource, "(__hole0 + __hole1)")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "9")
  }

  test("unsupported syntax fails clearly") {
    assert(QuasiquoteMacroExamples.unsupportedSyntaxMessage.trim.nonEmpty)
  }

  test("unsupported complex type ascriptions fail clearly") {
    assert(QuasiquoteMacroExamples.unsupportedComplexTypeAscriptionMessage.contains("Unsupported type ascription"))
  }
