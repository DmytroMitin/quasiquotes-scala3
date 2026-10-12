package quasiquotes.construct.q068

final class Q068P3ProvenanceTest extends munit.FunSuite:
  test("four-origin explicit-Type provenance matrix via both public qq routes"):
    val evidence = Q068P3ProvenanceMacros.fourOrigins(
      { def id(value: Int): Int = value; id(41) },
      { def id(value: Int) = value; id(41) },
      41
    )
    evidence.foreach(println)
    def row(origin: String, category: String): String =
      evidence.find(_.startsWith(s"$origin|$category|")).getOrElse(fail(s"missing $origin $category"))
    val explicit = row("source-explicit", "match")
    assert(explicit.contains("publicStandard=true"), explicit)
    assert(explicit.contains("publicHybrid=true"), explicit)
    val inferred = row("source-inferred", "match")
    assert(inferred.contains("publicStandard=false"), inferred)
    assert(inferred.contains("publicHybrid=false"), inferred)
    for origin <- List("standard-synthetic", "hybrid-synthetic") do
      val matched = row(origin, "match")
      assert(matched.contains("publicStandard=false"), matched)
      assert(matched.contains("publicHybrid=false"), matched)
      assert(matched.contains("P3 requires explicit parameter and result Types"), matched)
      val symbols = row(origin, "symbols")
      assert(symbols.contains("bodyIsParam=true"), symbols)
      assert(symbols.contains("callIsMethod=true"), symbols)
      assert(symbols.contains("argIdentity=true"), symbols)
      assert(row(origin, "types").contains("equal=true"))
