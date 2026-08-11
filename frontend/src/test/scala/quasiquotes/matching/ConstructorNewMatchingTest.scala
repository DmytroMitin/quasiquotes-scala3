package quasiquotes.matching

private object ConstructorNewMatchingScope:
  private val capacity = 16
  private val other = 32

  val capture = QuasiquoteMatchExamples.summarizeMatchNormalized(
    "new java.lang.StringBuilder($capacity)",
    new java.lang.StringBuilder(capacity)
  )
  val identityMismatch = QuasiquoteMatchExamples.summarizeMatchNormalized(
    "new java.lang.RuntimeException($capacity)",
    new java.lang.StringBuilder(capacity)
  )
  val repeatedSuccess = QuasiquoteMatchExamples.summarizeMatchNormalized(
    "(new java.lang.StringBuilder($capacity), new java.lang.StringBuilder($capacity))",
    (new java.lang.StringBuilder(capacity), new java.lang.StringBuilder(capacity))
  )
  val repeatedFailure = QuasiquoteMatchExamples.summarizeMatchNormalized(
    "(new java.lang.StringBuilder($capacity), new java.lang.StringBuilder($capacity))",
    (new java.lang.StringBuilder(capacity), new java.lang.StringBuilder(other))
  )
  val equality = QuasiquoteMatchExamples.compareEquality(
    new java.lang.StringBuilder(capacity),
    new java.lang.StringBuilder(capacity)
  )

class ConstructorNewMatchingTest extends munit.FunSuite:
  test("constructor patterns match exact identity and capture ordered arguments") {
    assert(ConstructorNewMatchingScope.capture.success)
    assert(ConstructorNewMatchingScope.capture.bindings.exists(_.startsWith("$capacity = ")))
    assert(!ConstructorNewMatchingScope.identityMismatch.success)
  }

  test("constructor repeated holes reuse normalized structural equality") {
    assert(ConstructorNewMatchingScope.repeatedSuccess.success)
    assert(!ConstructorNewMatchingScope.repeatedFailure.success)
  }

  test("constructor canonical equality preserves constructor identity and arguments") {
    assert(ConstructorNewMatchingScope.equality.normalizedEqual)
    assert(ConstructorNewMatchingScope.equality.canonicalEqual)
    assert(ConstructorNewMatchingScope.equality.leftCanonical.startsWith("CNew(java.lang.StringBuilder"))
  }

  test("constructor pattern boundaries are controlled") {
    List(
      "new StringBuilder($x)",
      "new java.lang.StringBuilder[Int]($x)",
      "new java.lang.StringBuilder($x)($y)",
      "new java.lang.StringBuilder(capacity = $x)",
      "new java.lang.StringBuilder($x) { }"
    ).foreach(source => assert(QuasiPattern.term(source).isLeft))
  }
