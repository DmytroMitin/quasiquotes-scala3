package quasiquotes.matching

private object Lambda1MatchingFixtures:
  private val free = 10

  object First:
    val same = 1

  object Second:
    val same = 2

  val alphaIdentity =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(x: Int) => x", (y: Int) => y)
  val boundVsFree =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(x: Int) => x", (y: Int) => free)
  val alphaAdd =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(x: Int) => x + 1", (renamed: Int) => renamed + 1)
  val bodyHole =
    QuasiquoteMatchExamples.summarizeMatchNormalized("(x: Int) => $body", (y: Int) => y + 1)
  val repeatedCorrespondingBound =
    QuasiquoteMatchExamples.summarizeMatchNormalized(
      "((x: Int) => $body, (y: Int) => $body)",
      ((left: Int) => left, (right: Int) => right)
    )
  val repeatedFreeVsBound =
    QuasiquoteMatchExamples.summarizeMatchNormalized(
      "((x: Int) => $body, (y: Int) => $body)",
      ((left: Int) => left, (_: Int) => free)
    )
  val repeatedDifferentFreeIdentities =
    QuasiquoteMatchExamples.summarizeMatchNormalized(
      "((x: Int) => $body, (y: Int) => $body)",
      ((_: Int) => First.same, (_: Int) => Second.same)
    )
  val alphaCanonicalEquality =
    QuasiquoteMatchExamples.compareEquality((left: Int) => left, (right: Int) => right)
  val freeCanonicalInequality =
    QuasiquoteMatchExamples.compareEquality((left: Int) => left, (_: Int) => free)
  val constructorBinder =
    QuasiquoteMatchExamples.summarizeMatchNormalized(
      "(x: Int) => new java.lang.StringBuilder(x)",
      (renamed: Int) => new java.lang.StringBuilder(renamed)
    )

class Lambda1MatchingTest extends munit.FunSuite:
  test("typed Lambda1 matching is alpha-equivalent for bound names") {
    assert(Lambda1MatchingFixtures.alphaIdentity.success)
    assert(Lambda1MatchingFixtures.alphaAdd.success)
  }

  test("typed Lambda1 matching distinguishes a free reference from its binder") {
    assert(!Lambda1MatchingFixtures.boundVsFree.success)
  }

  test("Lambda1 complete-body holes preserve the original reflected subtree") {
    val result = Lambda1MatchingFixtures.bodyHole
    assert(result.success)
    assert(result.bindings.exists(_.startsWith("$body = ")))
    assert(result.bindings.exists(_.contains("y")))
    assert(!result.bindings.exists(_.contains("Ident(\"x\")")))
  }

  test("repeated holes compare corresponding bound references under ambient scopes") {
    assert(Lambda1MatchingFixtures.repeatedCorrespondingBound.success)
  }

  test("repeated holes distinguish free from bound and different free identities") {
    assert(!Lambda1MatchingFixtures.repeatedFreeVsBound.success)
    assert(!Lambda1MatchingFixtures.repeatedDifferentFreeIdentities.success)
  }

  test("canonical Lambda1 equality is alpha-aware and distinguishes free references") {
    assert(Lambda1MatchingFixtures.alphaCanonicalEquality.canonicalEqual)
    assert(!Lambda1MatchingFixtures.freeCanonicalInequality.canonicalEqual)
  }

  test("binder scope propagates through constructor arguments") {
    assert(Lambda1MatchingFixtures.constructorBinder.success)
  }

  test("pattern entrypoint rejects every excluded Lambda1 variant with a source location") {
    val cases = Vector(
      "(x: Int, y: Int) => x + y" -> "exactly one parameter",
      "x => x" -> "explicit parameter type",
      "(x: Int) => ((y: Int) => y)" -> "nested lambdas",
      "(x: Int) ?=> x" -> "context functions"
    )

    cases.foreach { case (source, detail) =>
      QuasiPattern.termLocated(source) match
        case Left(diagnostic) =>
          assert(diagnostic.diagnostic.message.contains(detail), clues(source, diagnostic))
          assert(diagnostic.location.nonEmpty, clues(source, diagnostic))
        case Right(pattern) => fail(s"expected controlled Lambda1 rejection for $source, got $pattern")
    }
  }
