package quasiquotes.matching

private object MatchAnyScope:
  private def foo(value: Int): Int = value + 1
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x", foo(1))

private object MatchFooApplicationScope:
  private def foo(value: Int): Int = value + 10
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("foo($x)", foo(1))

private object MatchFunctionHoleScope:
  private def bar(value: Int): Int = value + 1
  private val baz = 2
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$f($x)", bar(baz))

private object MatchSelectionApplicationScope:
  private object foo:
    def bar(value: Int): Int = value + 5
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("foo.bar($x)", foo.bar(3))

private object MatchInfixScope:
  private val a = 2
  private val b = 3
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x + $y", a + b)
  val repeatedSuccess: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x + $x", a + a)
  val repeatedFailure: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x + $x", a + b)
  val negative: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("$x + $y", a)

private object MatchNestedScope:
  private def f(value: Int): Int = value + 1
  private def g(value: Int): Int = value * 2
  private val h = 3
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("f(g($x))", f(g(h)))

private object MatchParenScope:
  private val z = 7
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("(($x))", ((z)))

private object MatchUnsupportedScope:
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatch("if $x then $y else $z", 1)

private object MatchMacroProofScope:
  private val a = 2
  private val b = 3
  private def f(value: Int): Int = value + 1
  private def g(value: Int): Int = value * 2
  private val h = 3
  val infix: String = QuasiquoteMatchExamples.classifyInfix(a + b)
  val nested: String = QuasiquoteMatchExamples.classifyNested(f(g(h)))

class QuasiPatternTest extends munit.FunSuite:
  test("qq term pattern parses a hole pattern") {
    val pattern = QuasiPattern.termOrThrow("$x + $y")
    assertEquals(pattern.placeholderSource, "__qqhole_x + __qqhole_y")
    assertEquals(pattern.shape, "Infix(Ident(__qqhole_x), +, Ident(__qqhole_y))")
    assertEquals(pattern.pattern.render, "Infix(Hole($x), +, Hole($y))")
  }

  test("qq $x matches an arbitrary target and binds it") {
    val demo = MatchAnyScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("qq foo($x) matches foo(1)") {
    val demo = MatchFooApplicationScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("qq $f($x) matches application with function hole") {
    val demo = MatchFunctionHoleScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$f = ")))
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("qq foo.bar($x) matches selection plus application") {
    val demo = MatchSelectionApplicationScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("qq $x + $y matches infix targets") {
    val demo = MatchInfixScope.demo
    assert(demo.success)
    assertEquals(demo.bindings.size, 2)
  }

  test("qq f(g($x)) matches nested targets") {
    val demo = MatchNestedScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("qq (($x)) matches parenthesized targets") {
    val demo = MatchParenScope.demo
    assert(demo.success)
    assert(demo.bindings.exists(_.startsWith("$x = ")))
  }

  test("repeated hole names require structural equality") {
    assert(MatchInfixScope.repeatedSuccess.success)
    assert(!MatchInfixScope.repeatedFailure.success)
    assert(MatchInfixScope.repeatedFailure.detail.contains("Repeated hole"))
  }

  test("shape mismatch fails clearly") {
    assert(!MatchInfixScope.negative.success)
    assert(MatchInfixScope.negative.detail.contains("Pattern shape mismatch"))
  }

  test("unsupported pattern syntax fails clearly") {
    assert(!MatchUnsupportedScope.demo.success)
    assert(MatchUnsupportedScope.demo.detail.contains("Unsupported pattern tree shape"))
  }

  test("matching API works inside real macros") {
    assert(MatchMacroProofScope.infix.startsWith("infix-match("))
    assert(MatchMacroProofScope.nested.startsWith("nested-match("))
  }
