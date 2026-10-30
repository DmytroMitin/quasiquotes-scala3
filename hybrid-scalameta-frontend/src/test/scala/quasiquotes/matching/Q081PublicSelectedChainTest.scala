package quasiquotes.matching

final class Q081PublicSelectedChainTest extends munit.FunSuite:
  test("foreign-package public source patterns infer all positional types at every depth 1 through 8"):
    val names = List("alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta")
    assertEquals(hybridchainconsumer.Q081PublicChainConsumer.observations,
      (1 to 8).map(n => s"n$n:${names.take(n).mkString(",")}:identity=true:recursive=true").toList)

  test("public chain matches suffixes, decoded names and rejects unsupported target shells atomically"):
    assertEquals(Q081PublicChainBoundaryProbe.boundaries, List(
      "wrapped", "long-suffix", "anchored-positive", "anchored-root", "equal-targets", "lexical", "unicode", "short",
      "apply", "typeapply", "named", "named-apply", "contextual", "right-associative", "nullary", "curried", "null"
    ).map(_ + ":true"))

  test("public chain preserves the original receiver wrappers and stable local reference"):
    assertEquals(Q081ReceiverIdentityProbe.observations,
      List("inner-inlined:true", "inner-empty-block:true", "local-stable:true"))
