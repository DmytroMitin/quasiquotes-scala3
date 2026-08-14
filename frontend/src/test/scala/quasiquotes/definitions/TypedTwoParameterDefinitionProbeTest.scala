package quasiquotes.definitions

private object TypedTwoParameterDefinitionExamples:
  val evidence = TypedTwoParameterDefinitionProbe.inspect {
    def first(x: Int, y: String): Int = x
    def second(x: Int, y: String): String = y
    def plus(x: Int, y: Int): Int = x + y
    (first(1, "a"), second(1, "b"), plus(1, 2))
  }

class TypedTwoParameterDefinitionProbeTest extends munit.FunSuite:
  private val evidence = TypedTwoParameterDefinitionExamples.evidence

  test("typed exact-two definitions retain one ordered clause of distinct method-owned parameters") {
    assertEquals(evidence.map(_.name), List("first", "second", "plus"))
    assertEquals(evidence.map(_.parameterClauseSizes), List.fill(3)(List(2)))
    assertEquals(evidence.map(_.parameterNames), List.fill(3)(List("x", "y")))
    assert(evidence.forall(_.parameterSymbolsDistinct))
    assert(evidence.flatMap(_.parameterOwnersAreDefinition).forall(identity))
    assert(evidence.flatMap(_.parameterTypes).forall(value => value.contains("Int") || value.contains("String")))
    assert(evidence.map(_.resultType).forall(value => value.contains("Int") || value.contains("String")))
  }

  test("typed body references resolve to the exact corresponding parameter symbols") {
    assertEquals(
      evidence.map(_.boundReferenceNames),
      List(List("x"), List("y"), List("x", "y"))
    )
    assertEquals(
      evidence.map(_.boundReferenceParameterIndices),
      List(List(0), List(1), List(0, 1))
    )
    assert(evidence.forall(_.bodyKind.nonEmpty))
    assert(evidence.forall(_.treeStructure.contains("DefDef")))
    assert(evidence.forall(_.treeStructure.contains("ValDef")))
  }
