package quasiquotes.matching

private object MatchAnyScope:
  private def foo(value: Int): Int = value + 1
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x", foo(1))

private object MatchFooApplicationScope:
  private def foo(value: Int): Int = value + 10
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("foo($x)", foo(1))

private object MatchLiteralScope:
  val booleanLiteral: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("true", true)

private object MatchFunctionHoleScope:
  private def bar(value: Int): Int = value + 1
  private val baz = 2
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$f($x)", bar(baz))

private object MatchSelectionApplicationScope:
  private object foo:
    def bar(value: Int): Int = value + 5
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("foo.bar($x)", foo.bar(3))

private object MatchInfixScope:
  private val a = 2
  private val b = 3
  private val c = 4
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $y", a + b)
  val sameIdentifierEquality: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + a)
  val differentIdentifierInequality: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + b)
  val repeatedSuccess: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + a)
  val repeatedFailure: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", a + b)
  val repeatedParenSuccess: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", (a) + a)
  val repeatedNestedParenSuccess: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", ((a)) + (a))
  val commutativeRejection: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + b", b + a)
  val associativeRejection: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", (a + b) + (a + (b + c)))
  val algebraicSimplificationRejection: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", (a + 0) + a)
  val semanticEqualityRejection: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", (1 + 1) + 2)
  val explicitMethodCallShape: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $y", a.+(b))
  val negative: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $y", a)
  val normalization: QuasiquoteMatchExamples.NormalizationDemo = QuasiquoteMatchExamples.summarizeNormalization("$x + $y", a + b)

private object MatchNestedScope:
  private def f(value: Int): Int = value + 1
  private def g(value: Int): Int = value * 2
  private val h = 3
  private val i = 4
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("f(g($x))", f(g(h)))
  val repeated: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("pair($x, $x)", pair(h, h))
  val repeatedFunctionSuccess: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("f($x, $x)", f(h, h))
  val repeatedFunctionFailure: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("f($x, $x)", f(h, i))

  private def f(left: Int, right: Int): Int = left + right
  private def pair(left: Int, right: Int): Int = left + right

private object MatchParenScope:
  private val z = 7
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("(($x))", ((z)))
  val singleParenEquality: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", z + (z))
  val nestedParenEquality: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("$x + $x", z + ((z)))
  val normalization: QuasiquoteMatchExamples.NormalizationDemo = QuasiquoteMatchExamples.summarizeNormalization("(($x))", ((z)))

private object MatchUnsupportedScope:
  val demo: QuasiquoteMatchExamples.MatchDemo = QuasiquoteMatchExamples.summarizeMatchNormalized("value match { case x => x }", 1)

private object MatchTypedScope:
  private val a = 2
  private val b = "two"
  private def foo(value: Int): Int = value + 1

  val typedPattern: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("$x: Int", (a: Int))
  val typedPatternWrongType: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("$x: Int", (b: String))
  val typedPatternPlainTarget: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("$x: Int", a)
  val typedApplicationArgument: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("foo($x: Int)", foo(a: Int))

private object MatchTupleScope:
  private val a = 2
  private val b = 3
  private val c = 4
  private def foo(value: (Int, Int)): Int = value._1 + value._2

  val simple: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("($x, $y)", (a, b))
  val repeatedSuccess: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("($x, $x)", (a, a))
  val repeatedFailure: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("($x, $x)", (a, b))
  val applicationArgument: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("foo(($x, $y))", foo((a, b)))
  val nested: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("($x, ($y, $z))", (a, (b, c)))

private object MatchIfScope:
  private val cond = true
  private val a = 2
  private val b = 3
  private def foo(value: Int): Int = value + 10

  val simple: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("if $c then $t else $e", if cond then a else b)
  val repeatedSuccess: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("if $c then $x else $x", if cond then a else a)
  val repeatedFailure: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("if $c then $x else $x", if cond then a else b)
  val applicationArgument: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("foo(if $c then $t else $e)", foo(if cond then a else b))
  val tupleBranches: QuasiquoteMatchExamples.MatchDemo =
    QuasiquoteMatchExamples.summarizeMatchNormalized("if $c then ($a, $b) else ($d, $e)", if cond then (a, b) else (b, a))

private object MatchUnaryScope:
  private val a = 2
  private val b = 3
  private val flag = true

  val plus = QuasiquoteMatchExamples.summarizeMatchNormalized("+$x", +a)
  val minus = QuasiquoteMatchExamples.summarizeMatchNormalized("-$x", -a)
  val not = QuasiquoteMatchExamples.summarizeMatchNormalized("!$x", !flag)
  val complement = QuasiquoteMatchExamples.summarizeMatchNormalized("~$x", ~a)
  val operatorMismatch = QuasiquoteMatchExamples.summarizeMatchNormalized("-$x", +a)
  val nested = QuasiquoteMatchExamples.summarizeMatchNormalized("-(-$x)", -(-a))
  val tuple = QuasiquoteMatchExamples.summarizeMatchNormalized("(-$x, !$b)", (-a, !flag))
  val conditional = QuasiquoteMatchExamples.summarizeMatchNormalized(
    "if !$c then -$x else +$y",
    if !flag then -a else +b
  )
  val wholeCapture = QuasiquoteMatchExamples.summarizeMatchNormalized("$x", -a)
  val repeatedSame = QuasiquoteMatchExamples.summarizeMatchNormalized("(-$x, -$x)", (-a, -a))
  val repeatedDifferent = QuasiquoteMatchExamples.summarizeMatchNormalized("(-$x, -$x)", (-a, -b))
  val repeatedMixed = QuasiquoteMatchExamples.summarizeMatchNormalized("(-$x, +$x)", (-a, +a))
  val repeatedMixedFailure = QuasiquoteMatchExamples.summarizeMatchNormalized("(-$x, +$x)", (-a, +b))
  val foldedPlusLiteral = QuasiquoteMatchExamples.summarizeMatchNormalized("+$x", +1)
  val foldedNotLiteral = QuasiquoteMatchExamples.summarizeMatchNormalized("!$x", !true)
  val foldedComplementLiteral = QuasiquoteMatchExamples.summarizeMatchNormalized("~$x", ~1)
  val signedLiteral = QuasiquoteMatchExamples.summarizeMatchNormalized("-1", -1)

private object UnaryCanonicalEqualityScope:
  private val a = 2
  private val b = 3
  private val flag = true
  val sameMinus = QuasiquoteMatchExamples.compareEquality(-a, -a)
  val differentOperand = QuasiquoteMatchExamples.compareEquality(-a, -b)
  val differentOperator = QuasiquoteMatchExamples.compareEquality(-a, +a)
  val parens = QuasiquoteMatchExamples.compareEquality(-(a), -a)
  val nestedVsPlain = QuasiquoteMatchExamples.compareEquality(-(-a), a)
  val doubleNotVsPlain = QuasiquoteMatchExamples.compareEquality(!(!flag), flag)
  val plusVsPlain = QuasiquoteMatchExamples.compareEquality(+a, a)

private object MatchMacroProofScope:
  private val a = 2
  private val b = 3
  private def f(value: Int): Int = value + 1
  private def g(value: Int): Int = value * 2
  private val h = 3
  val infixRaw: String = QuasiquoteMatchExamples.classifyInfixRaw(a + b)
  val infix: String = QuasiquoteMatchExamples.classifyInfix(a + b)
  val nested: String = QuasiquoteMatchExamples.classifyNested(f(g(h)))
  val duplicatedSuccess: String = QuasiquoteMatchExamples.classifyRepeatedOperand(a + a)
  val duplicatedFailure: String = QuasiquoteMatchExamples.classifyRepeatedOperand(a + b)

private object CanonicalEqualityScope:
  private val a = 2
  private val b = 3
  private val c = 4
  private def one: Int = 1
  private def two: Int = 2

  val sameIdentifier: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, a)
  val differentIdentifiers: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, b)
  val sameBooleanLiteral: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(true, true)
  val differentBooleanLiteral: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(true, false)
  val singleParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, (a))
  val nestedParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, ((a)))
  val methodCallOperatorShape: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a + b, a.+(b))
  val repeatedInfixOperands: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, a)
  val repeatedParenOperands: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a), a)
  val repeatedFunctionArguments: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a, a)
  val commutativity: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a + b, b + a)
  val associativity: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a + b) + c, a + (b + c))
  val algebraicSimplification: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(a + 0, a)
  val semanticEquality: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(one + one, two)
  val typedSameType: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a: Int), (a: Int))
  val typedNestedParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(((a: Int)), (a: Int))
  val typedDifferentType: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a: Int), (a: AnyVal))
  val typedVsPlain: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a: Int), a)
  val tupleSame: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a, b), (a, b))
  val tupleNestedParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(((a, b)), (a, b))
  val tupleOrder: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a, b), (b, a))
  val tupleNesting: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a, (b, c)), ((a, b), c))
  val tupleVsPlain: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality((a, b), a)
  private def trueCondition: Boolean = true
  val ifSame: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if trueCondition then a else b, if trueCondition then a else b)
  val ifConditionParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if (trueCondition) then a else b, if trueCondition then a else b)
  val ifBranchParens: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if trueCondition then (a) else b, if trueCondition then a else b)
  val ifBranchOrder: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if trueCondition then a else b, if trueCondition then b else a)
  val ifSameBranchesVsPlain: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if trueCondition then a else a, a)
  val ifTrueConditionVsPlain: QuasiquoteMatchExamples.EqualityComparisonDemo =
    QuasiquoteMatchExamples.compareEquality(if trueCondition then a else b, a)
  val lambda: QuasiquoteMatchExamples.CanonicalDemo =
    QuasiquoteMatchExamples.summarizeCanonical((x: Int) => x)
  val block: QuasiquoteMatchExamples.CanonicalDemo =
    QuasiquoteMatchExamples.summarizeCanonical {
      val local = a
      local
    }

class QuasiPatternTest extends munit.FunSuite:
  test("match result binding lookup accepts bare and dollar-prefixed hole names") {
    val result = MatchResult(Map("x" -> "bound"))

    assertEquals(result.binding("x"), Some("bound"))
    assertEquals(result.binding("$x"), Some("bound"))
    assertEquals(result.binding("missing"), None)
  }

  test("qq term pattern parses a hole pattern") {
    val pattern = QuasiPattern.termOrThrow("$x + $y")
    assertEquals(pattern.placeholderSource, "__qqhole_x + __qqhole_y")
    assertEquals(pattern.shape, "Infix(Ident(__qqhole_x), +, Ident(__qqhole_y))")
    assertEquals(pattern.pattern.render, "Infix(Hole($x), +, Hole($y))")
  }

  test("Phase 26 constructed-type splice syntax does not add type-hole capture to term matching") {
    val pattern = QuasiPattern.termOrThrow("($x: $t)")
    assertEquals(pattern.pattern.render, "Parens(Typed(Hole($x), Type(__qqhole_t)))")
    assertEquals(pattern.placeholderSource, "(__qqhole_x: __qqhole_t)")
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

  test("literal patterns match boolean literal targets structurally") {
    assert(MatchLiteralScope.booleanLiteral.success)
    assertEquals(MatchLiteralScope.booleanLiteral.bindings, Nil)
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

  test("equality contract treats the same identifier as equal") {
    assert(MatchInfixScope.sameIdentifierEquality.success)
  }

  test("equality contract treats different identifiers as not equal") {
    assert(!MatchInfixScope.differentIdentifierInequality.success)
    assert(MatchInfixScope.differentIdentifierInequality.detail.contains("Repeated hole"))
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

  test("parentheses normalization fixes raw matching failure") {
    val demo = MatchParenScope.normalization
    assert(!demo.before.success)
    assert(demo.after.success)
    assert(demo.after.bindings.exists(_.startsWith("$x = ")))
  }

  test("equality contract ignores supported parentheses around repeated holes") {
    assert(MatchParenScope.singleParenEquality.success)
    assert(MatchParenScope.nestedParenEquality.success)
    assert(MatchInfixScope.repeatedParenSuccess.success)
    assert(MatchInfixScope.repeatedNestedParenSuccess.success)
  }

  test("repeated hole names require normalized equality") {
    assert(MatchInfixScope.repeatedSuccess.success)
    assert(!MatchInfixScope.repeatedFailure.success)
    assert(MatchInfixScope.repeatedFailure.detail.contains("Repeated hole"))
  }

  test("nested repeated holes match equal arguments") {
    val demo = MatchNestedScope.repeated
    assert(demo.success)
    assertEquals(demo.bindings.count(_.startsWith("$x = ")), 1)
  }

  test("function-call repeated holes enforce equality") {
    assert(MatchNestedScope.repeatedFunctionSuccess.success)
    assert(!MatchNestedScope.repeatedFunctionFailure.success)
    assert(MatchNestedScope.repeatedFunctionFailure.detail.contains("Repeated hole"))
  }

  test("equality contract rejects commutativity") {
    assert(!MatchInfixScope.commutativeRejection.success)
  }

  test("equality contract rejects associativity") {
    assert(!MatchInfixScope.associativeRejection.success)
    assert(MatchInfixScope.associativeRejection.detail.contains("Repeated hole"))
  }

  test("equality contract rejects algebraic simplification") {
    assert(!MatchInfixScope.algebraicSimplificationRejection.success)
    assert(MatchInfixScope.algebraicSimplificationRejection.detail.contains("Repeated hole"))
  }

  test("equality contract rejects semantic equality") {
    assert(!MatchInfixScope.semanticEqualityRejection.success)
  }

  test("explicit method-call operator syntax uses only the existing limited infix shape normalization") {
    assert(MatchInfixScope.explicitMethodCallShape.success)
    assertEquals(MatchInfixScope.explicitMethodCallShape.bindings.size, 2)
  }

  test("shape mismatch fails clearly") {
    assert(!MatchInfixScope.negative.success)
    assert(MatchInfixScope.negative.detail.contains("Pattern shape mismatch"))
  }

  test("infix normalization fixes a real raw mismatch") {
    val demo = MatchInfixScope.normalization
    assert(!demo.before.success)
    assert(demo.after.success)
    assertEquals(demo.after.bindings.size, 2)
  }

  test("unsupported pattern syntax fails clearly") {
    assert(!MatchUnsupportedScope.demo.success)
    assert(MatchUnsupportedScope.demo.detail.contains("Unsupported pattern tree shape"))
  }

  test("unsupported lambda and block patterns remain outside the equality contract") {
    assert(QuasiPattern.term("(x => x)").isLeft)
    assert(QuasiPattern.term("(y => y)").isLeft)
    assert(QuasiPattern.term("{ val x = 1; x }").isLeft)
  }

  test("typed expression patterns match supported ascriptions structurally") {
    assert(MatchTypedScope.typedPattern.success)
    assert(MatchTypedScope.typedPattern.bindings.exists(_.startsWith("$x = ")))
    assert(MatchTypedScope.typedApplicationArgument.success)
  }

  test("typed expression patterns reject mismatched or missing ascriptions") {
    assert(!MatchTypedScope.typedPatternWrongType.success)
    assert(MatchTypedScope.typedPatternWrongType.detail.contains("Pattern shape mismatch"))
    assert(!MatchTypedScope.typedPatternPlainTarget.success)
    assert(MatchTypedScope.typedPatternPlainTarget.detail.contains("Pattern shape mismatch"))
  }

  test("tuple expression patterns match supported tuple targets structurally") {
    assert(MatchTupleScope.simple.success)
    assertEquals(MatchTupleScope.simple.bindings.size, 2)
    assert(MatchTupleScope.applicationArgument.success)
    assert(MatchTupleScope.nested.success)
  }

  test("tuple repeated-hole patterns use normalized structural equality") {
    assert(MatchTupleScope.repeatedSuccess.success)
    assert(!MatchTupleScope.repeatedFailure.success)
    assert(MatchTupleScope.repeatedFailure.detail.contains("Repeated hole"))
  }

  test("if expression patterns match supported if targets structurally") {
    assert(MatchIfScope.simple.success)
    assertEquals(MatchIfScope.simple.bindings.size, 3)
    assert(MatchIfScope.applicationArgument.success)
    assert(MatchIfScope.tupleBranches.success)
  }

  test("if repeated-hole branch patterns use normalized structural equality") {
    assert(MatchIfScope.repeatedSuccess.success)
    assert(!MatchIfScope.repeatedFailure.success)
    assert(MatchIfScope.repeatedFailure.detail.contains("Repeated hole"))
  }

  test("unary patterns match the exact four-operator structural tranche") {
    assert(MatchUnaryScope.plus.success)
    assert(MatchUnaryScope.minus.success)
    assert(MatchUnaryScope.not.success)
    assert(MatchUnaryScope.complement.success)
    assert(!MatchUnaryScope.operatorMismatch.success)
    assert(MatchUnaryScope.nested.success)
    assert(MatchUnaryScope.tuple.success)
    assert(MatchUnaryScope.conditional.success)
    assert(MatchUnaryScope.wholeCapture.success)
  }

  test("unary repeated holes use normalized structural equality") {
    assert(MatchUnaryScope.repeatedSame.success)
    assert(!MatchUnaryScope.repeatedDifferent.success)
    assert(MatchUnaryScope.repeatedMixed.success)
    assert(!MatchUnaryScope.repeatedMixedFailure.success)
  }

  test("folded typed literals are not reconstructed as source unary syntax") {
    assert(!MatchUnaryScope.foldedPlusLiteral.success)
    assert(!MatchUnaryScope.foldedNotLiteral.success)
    assert(!MatchUnaryScope.foldedComplementLiteral.success)
    assert(MatchUnaryScope.signedLiteral.success)
  }

  test("matching API works inside real macros") {
    assert(MatchMacroProofScope.infix.startsWith("infix-match("))
    assert(MatchMacroProofScope.nested.startsWith("nested-match("))
  }

  test("macro demo shows normalization improvement explicitly") {
    assert(MatchMacroProofScope.infixRaw.startsWith("raw-no-infix-match("))
    assert(MatchMacroProofScope.infix.startsWith("infix-match("))
  }

  test("macro repeated-hole demo detects duplicated operands") {
    assert(MatchMacroProofScope.duplicatedSuccess.startsWith("duplicated-operand("))
    assert(MatchMacroProofScope.duplicatedFailure.startsWith("not-duplicated("))
  }

  test("canonical equality matches normalized equality for supported identifiers and parentheses") {
    assert(CanonicalEqualityScope.sameIdentifier.normalizedEqual)
    assert(CanonicalEqualityScope.sameIdentifier.canonicalEqual)
    assert(!CanonicalEqualityScope.differentIdentifiers.normalizedEqual)
    assert(!CanonicalEqualityScope.differentIdentifiers.canonicalEqual)
    assert(CanonicalEqualityScope.sameBooleanLiteral.normalizedEqual)
    assert(CanonicalEqualityScope.sameBooleanLiteral.canonicalEqual)
    assert(!CanonicalEqualityScope.differentBooleanLiteral.normalizedEqual)
    assert(!CanonicalEqualityScope.differentBooleanLiteral.canonicalEqual)
    assert(CanonicalEqualityScope.singleParens.normalizedEqual)
    assert(CanonicalEqualityScope.singleParens.canonicalEqual)
    assert(CanonicalEqualityScope.nestedParens.normalizedEqual)
    assert(CanonicalEqualityScope.nestedParens.canonicalEqual)
  }

  test("canonical equality preserves existing limited infix operator shape normalization") {
    assert(CanonicalEqualityScope.methodCallOperatorShape.normalizedEqual)
    assert(CanonicalEqualityScope.methodCallOperatorShape.canonicalEqual)
  }

  test("canonical equality agrees with normalized equality for repeated-hole success operands") {
    assert(CanonicalEqualityScope.repeatedInfixOperands.normalizedEqual)
    assert(CanonicalEqualityScope.repeatedInfixOperands.canonicalEqual)
    assert(CanonicalEqualityScope.repeatedParenOperands.normalizedEqual)
    assert(CanonicalEqualityScope.repeatedParenOperands.canonicalEqual)
    assert(CanonicalEqualityScope.repeatedFunctionArguments.normalizedEqual)
    assert(CanonicalEqualityScope.repeatedFunctionArguments.canonicalEqual)
  }

  test("canonical equality preserves rejected equality boundaries") {
    assert(!CanonicalEqualityScope.commutativity.normalizedEqual)
    assert(!CanonicalEqualityScope.commutativity.canonicalEqual)
    assert(!CanonicalEqualityScope.associativity.normalizedEqual)
    assert(!CanonicalEqualityScope.associativity.canonicalEqual)
    assert(!CanonicalEqualityScope.algebraicSimplification.normalizedEqual)
    assert(!CanonicalEqualityScope.algebraicSimplification.canonicalEqual)
    assert(!CanonicalEqualityScope.semanticEquality.normalizedEqual)
    assert(!CanonicalEqualityScope.semanticEquality.canonicalEqual)
  }

  test("unary canonical and normalized equality are operator-sensitive and non-algebraic") {
    assert(UnaryCanonicalEqualityScope.sameMinus.normalizedEqual)
    assert(UnaryCanonicalEqualityScope.sameMinus.canonicalEqual)
    assert(!UnaryCanonicalEqualityScope.differentOperand.normalizedEqual)
    assert(!UnaryCanonicalEqualityScope.differentOperand.canonicalEqual)
    assert(!UnaryCanonicalEqualityScope.differentOperator.normalizedEqual)
    assert(!UnaryCanonicalEqualityScope.differentOperator.canonicalEqual)
    assert(UnaryCanonicalEqualityScope.parens.normalizedEqual)
    assert(UnaryCanonicalEqualityScope.parens.canonicalEqual)
    assert(!UnaryCanonicalEqualityScope.nestedVsPlain.normalizedEqual)
    assert(!UnaryCanonicalEqualityScope.nestedVsPlain.canonicalEqual)
    assert(!UnaryCanonicalEqualityScope.doubleNotVsPlain.normalizedEqual)
    assert(!UnaryCanonicalEqualityScope.doubleNotVsPlain.canonicalEqual)
    assert(!UnaryCanonicalEqualityScope.plusVsPlain.normalizedEqual)
    assert(!UnaryCanonicalEqualityScope.plusVsPlain.canonicalEqual)
  }

  test("typed expression equality preserves ascription boundaries") {
    assert(CanonicalEqualityScope.typedSameType.normalizedEqual)
    assert(CanonicalEqualityScope.typedSameType.canonicalEqual)
    assert(CanonicalEqualityScope.typedNestedParens.normalizedEqual)
    assert(CanonicalEqualityScope.typedNestedParens.canonicalEqual)
    assert(!CanonicalEqualityScope.typedDifferentType.normalizedEqual)
    assert(!CanonicalEqualityScope.typedDifferentType.canonicalEqual)
    assert(!CanonicalEqualityScope.typedVsPlain.normalizedEqual)
    assert(!CanonicalEqualityScope.typedVsPlain.canonicalEqual)
  }

  test("tuple expression equality preserves order and nesting boundaries") {
    assert(CanonicalEqualityScope.tupleSame.normalizedEqual)
    assert(CanonicalEqualityScope.tupleSame.canonicalEqual)
    assert(CanonicalEqualityScope.tupleNestedParens.normalizedEqual)
    assert(CanonicalEqualityScope.tupleNestedParens.canonicalEqual)
    assert(!CanonicalEqualityScope.tupleOrder.normalizedEqual)
    assert(!CanonicalEqualityScope.tupleOrder.canonicalEqual)
    assert(!CanonicalEqualityScope.tupleNesting.normalizedEqual)
    assert(!CanonicalEqualityScope.tupleNesting.canonicalEqual)
    assert(!CanonicalEqualityScope.tupleVsPlain.normalizedEqual)
    assert(!CanonicalEqualityScope.tupleVsPlain.canonicalEqual)
  }

  test("if expression equality preserves branch and condition structure") {
    assert(CanonicalEqualityScope.ifSame.normalizedEqual)
    assert(CanonicalEqualityScope.ifSame.canonicalEqual)
    assert(CanonicalEqualityScope.ifConditionParens.normalizedEqual)
    assert(CanonicalEqualityScope.ifConditionParens.canonicalEqual)
    assert(CanonicalEqualityScope.ifBranchParens.normalizedEqual)
    assert(CanonicalEqualityScope.ifBranchParens.canonicalEqual)
    assert(!CanonicalEqualityScope.ifBranchOrder.normalizedEqual)
    assert(!CanonicalEqualityScope.ifBranchOrder.canonicalEqual)
    assert(!CanonicalEqualityScope.ifSameBranchesVsPlain.normalizedEqual)
    assert(!CanonicalEqualityScope.ifSameBranchesVsPlain.canonicalEqual)
    assert(!CanonicalEqualityScope.ifTrueConditionVsPlain.normalizedEqual)
    assert(!CanonicalEqualityScope.ifTrueConditionVsPlain.canonicalEqual)
  }

  test("unsupported control-flow and binder-like patterns remain unsupported") {
    assert(QuasiPattern.term("value match { case x => x }").isLeft)
    assert(QuasiPattern.term("while cond do a").isLeft)
    assert(QuasiPattern.term("for x <- xs yield x").isLeft)
  }

  test("tuple destructuring and binder-like patterns remain unsupported") {
    assert(QuasiPattern.term("case ($x, $y) => $x").isLeft)
    assert(QuasiPattern.term("(($x, $y) => $x)").isLeft)
  }

  test("canonicalization keeps lambdas and local blocks unsupported") {
    assert(!CanonicalEqualityScope.lambda.success)
    assert(CanonicalEqualityScope.lambda.detail.contains("Unsupported"))
    assert(!CanonicalEqualityScope.block.success)
    assert(CanonicalEqualityScope.block.detail.contains("Unsupported"))
  }
