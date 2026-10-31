package quasiquotes.matching

import rankedselectedconsumer.Q083PublicRankedConsumer

final class Q083PublicRankedTest extends munit.FunSuite:
  test("public mixed extractor preserves raw Terms from a genuinely generated NoSpan target"):
    assertEquals(Q083GeneratedTargetProbe.observation, "noSpan=true:inner:outer:1:identity=true")

  test("public mixed captures agree with nested public composition and the independent Q076 model"):
    assertEquals(Q083RankedEquivalenceProbe.observations, List(
      "zero:nested=true:model=true", "one:nested=true:model=true", "many:nested=true:model=true"))

  test("foreign public source preserves exact receiver and 0/1/3 argument objects, suffixes and explicit anchors"):
    assertEquals(Q083PublicRankedConsumer.observations, List(
      "zero:first:second:0:identity=true", "one:first:second:1:identity=true",
      "many:first:second:3:identity=true", "local:first:second:1:identity=true",
      "local-receiver:first:second:1:identity=true",
      "suffix:first:second:1:identity=true", "fixed-root-positive:true", "fixed-root-rejected:true",
      "wrapped:first:second:1:identity=true"))

  test("parenthesized ordinary selectors preserve identity and hidden apply cannot shift source roles"):
    assertEquals(Q083PublicRankedConsumer.reviewObservations, List(
      "qualifier-parens:identity=true", "function-parens:identity=true", "both-parens:identity=true",
      "explicit-apply:identity=true", "explicit-equal-apply:identity=true",
      "implicit-apply-rejected:true", "implicit-equal-apply-rejected:true"))

  test("two distinct binders accept equal target names and conservative decoded lexical names"):
    assertEquals(Q083PublicRankedConsumer.lexicalObservations, List(
      "equal:same:same", "operator:first:+", "keyword:first:type", "spaced:first:safe spaced name",
      "generated:first:__qqhole_first__selected_rank2_1", "unicode-first:fallthrough", "unicode-second:fallthrough"))

  test("nonordinary target shells and argument clauses fall through atomically"):
    assertEquals(Q083PublicRankedConsumer.negativeObservations, List(
      "bare:true", "curried:true", "contextual:true", "multi:true", "type-apply:true", "named:true",
      "infix:true", "right-infix:true", "inline-ident:true", "block:true", "typed:true", "null:true"))
