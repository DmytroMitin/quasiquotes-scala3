package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName

final class Q080SelectedChainSourceTest extends munit.FunSuite:
  private val parts = List("", ".", ".", ".", "")
  private val source = "$origin.$first.$second.$third"
  private def plan = SelectedChainPatternSource.planSource(source, parts).fold(fail(_), identity)

  test("source roles retain exact original spans, reverse Select ordinals and disjoint transports"):
    val result = plan
    assertEquals(result.receiverName, "origin")
    assertEquals(result.selectedOccurrences.map(_.name), Vector("first", "second", "third"))
    assertEquals(result.selectedOccurrences.map(_.selectOrdinal), Vector(2, 1, 0))
    assertEquals(result.mapped.occurrences.map(o => source.slice(o.originalSpan.start, o.originalSpan.end)),
      Vector("$origin", "$first", "$second", "$third"))
    assert(result.selectedOccurrences.forall(o =>
      !result.mapped.generatedHoleIndex.generatedNames(o.transport) &&
        !result.mapped.occurrences.exists(_.name == o.transport)))
    assertEquals(QuasiPattern.term(result.effectiveSource).map(_.pattern), Right(result.pattern))

  test("noncandidates and over-capacity candidates never demand original positions"):
    val legacy = List(List("", ""), List("", " + ", ""), List("", ".", ""),
      List("", ".", "()"), List("", ".", "(", ")"), List("", ".", ".", ""),
      List("", ".", ".", "(", ")"), List("", "(..", ")"), List("", ".", "(..", ")"),
      List("{ def id(x: Int): Int = x; id(", ") }"))
    legacy.foreach(p => assertEquals(SelectedChainPatternSource.classify(p, fail("source requested")), Right(None)))
    assert(SelectedChainPatternSource.classify(List("") ++ List.fill(9)(".") ++ List(""),
      fail("capacity classification requested source")).left.toOption.exists(_.contains("capacity")))

  test("missing, malformed and category-conflicting original source fails closed"):
    List(null, "prefix.$origin.$first.$second.$third", "$origin.$first.$second.$third()",
      "$origin.$first..$second.$third", "$origin.$first $second.$third",
      "$origin.$first.$first.$third", "$origin.$origin.$second.$third",
      "$origin.$first.$second.$naïve", "$naïve.$first.$second.$third").foreach(s =>
      assert(SelectedChainPatternSource.planSource(s, parts).isLeft, String.valueOf(s)))
    assert(SelectedChainPatternSource.planSource(source, List("", ".", ".", "")).isLeft)

  test("compiled plan validation rejects duplicate, malformed and category transports"):
    val p = plan
    def invalid(roles: Vector[SelectedNamePatternOccurrence]) =
      assert(SelectedChainPatternSource.validate(p.pattern, p.receiverName, roles).isLeft)
    invalid(p.selectedOccurrences.updated(0, p.selectedOccurrences(0).copy(selectOrdinal = 0)))
    invalid(p.selectedOccurrences.updated(0, p.selectedOccurrences(0).copy(transport = p.selectedOccurrences(1).transport)))
    invalid(p.selectedOccurrences.updated(0, p.selectedOccurrences(0).copy(transport = "bad.transport")))
    invalid(p.selectedOccurrences.updated(0, p.selectedOccurrences(0).copy(transport = p.receiverName)))
    assert(SelectedChainPatternSource.validate(TermPattern.Hole(p.receiverName), p.receiverName, p.selectedOccurrences).isLeft)

  test("transport allocation remains disjoint from generated-looking source binders"):
    val collision = "$origin.$first.$__qqhole_first__selected_chain_2.$third"
    val p = SelectedChainPatternSource.planSource(collision, parts).fold(fail(_), identity)
    assert(p.selectedOccurrences.forall(o => !p.mapped.occurrences.exists(_.name == o.transport)))
    assertEquals(p.selectedOccurrences.map(_.transport).distinct.size, 3)

  test("static tuple cardinality rejects short and surplus names before indexed reads"):
    type Roles = ProductTermCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: ProductSelectedNameCapture *: EmptyTuple
    val name = SelectedMemberName.from("same").fold(e => fail(e.message), identity)
    assertEquals(SelectedChainTupleFactory.checkedTuple[String, Roles]("origin", Vector.empty), None)
    assertEquals(SelectedChainTupleFactory.checkedTuple[String, Roles]("origin", Vector(name)), None)
    assertEquals(SelectedChainTupleFactory.checkedTuple[String, Roles]("origin", Vector.fill(4)(name)), None)
    val typed: Option[(String, SelectedMemberName, SelectedMemberName, SelectedMemberName)] =
      SelectedChainTupleFactory.checkedTuple[String, Roles]("origin", Vector.fill(3)(name))
    assertEquals(typed, Some(("origin", name, name, name)))

  test("actual generated public N3 NoSpan candidate reports missing source without an assertion"):
    val result = Q080NoSpanProbe.route(parts)
    assert(result.startsWith("source=none;static=true;"), result)
    assert(result.contains("selected-chain source-role planning requires recoverable original interpolation source"), result)
    assert(!result.contains("end of NoSpan"), result)

  test("actual generated legacy scalar, one-name, rank2, mixed and P3 contexts preserve routing"):
    List(List("", " + ", ""), List("", ".", ""), List("", "(..", ")"),
      List("", ".", "(..", ")"), List("{ def id(x: Int): Int = x; id(", ") }")).foreach(parts =>
      assertEquals(Q080NoSpanProbe.route(parts), "source=none;static=true;legacy-routed"))
