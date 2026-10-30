package quasiquotes.matching

import quasiquotes.scalameta.TermFrontend

final class Q081SelectedChainSourceTest extends munit.FunSuite:
  test("every new depth compiles Scalameta-primary with exact source positions and roles"):
    for n <- 3 to 8 do
      val names = (1 to n).map(i => s"slot$i")
      val source = "$receiver" + names.map(n => s".$$${n}").mkString
      val parts = List("") ++ List.fill(n)(".") ++ List("")
      val plan = ScalametaSelectedChainPatternSource.classify(parts, source).toOption.flatten.get
      val compiled = TermFrontend.compile(plan.effectiveSource).toOption.get
      assertEquals(compiled.engine, TermFrontend.Engine.Scalameta)
      assertEquals(compiled.primaryFailure, None)
      assertEquals(plan.pattern, compiled.pattern)
      assertEquals(plan.selectedOccurrences.map(_.selectOrdinal), (0 until n).reverse.toVector)
      assertEquals(plan.mapped.occurrences.map(o => source.slice(o.originalSpan.start,o.originalSpan.end)),
        Vector("$receiver") ++ names.map("$"+_))
      assertEquals(SelectedChainPatternSource.validate(plan.pattern,plan.receiverName,plan.selectedOccurrences),Right(()))

  test("legacy noncandidates and capacity classification never request original source"):
    List(List("", ""),List("", ".", ""),List("", ".", ".", ""),List("", "(..", ")"),List("", ".", "(..", ")"),List("{ def id(x: Int): Int = x; id(", ") }")).foreach(p =>
      assertEquals(ScalametaSelectedChainPatternSource.classify(p,fail("unexpected source lookup")),Right(None)))
    assert(ScalametaSelectedChainPatternSource.classify(List("")++List.fill(9)(".")++List(""),fail("unexpected source lookup")).isLeft)

  test("new source-free candidate fails safely; actual legacy NoSpan contexts retain routing"):
    val bad = Q081NoSpanProbe.route(List("", ".", ".", ".", ""))
    assert(bad.contains("selected-chain source-role planning requires recoverable original interpolation source"),bad)
    assert(!bad.contains("end of NoSpan"),bad)
    List(List("", " + ", ""),List("", ".", ""),List("", "(..", ")"),List("", ".", "(..", ")"),List("{ def id(x: Int): Int = x; id(", ") }")).foreach(p =>
      assertEquals(Q081NoSpanProbe.route(p),"source=none;static=true;legacy-routed"))

  test("malformed and conflicting original source rejects before frontend compilation"):
    val parts=List("", ".", ".", ".", "")
    List(null,"$r.$a..$b.$c","prefix.$r.$a.$b.$c","$r.$a.$a.$c","$r.$r.$b.$c","$r.$a.$b.$naïve").foreach(s =>
      assert(ScalametaSelectedChainPatternSource.classify(parts,s).isLeft))
