package quasiquotes.definitions

private object TypedOneParameterDefinitionExamples:
  val evidence = TypedOneParameterDefinitionProbe.inspect {
    def id(x: Int): Int = x
    def inc(x: Int): Int = x + 1
    (id(1), inc(1))
  }

class TypedOneParameterDefinitionProbeTest extends munit.FunSuite:
  private val evidence = TypedOneParameterDefinitionExamples.evidence

  test("typed one-parameter definitions retain method-owned parameter symbols") {
    assertEquals(evidence.map(_.name), List("id", "inc"))
    assertEquals(evidence.map(_.parameterClauseSizes), List(List(1), List(1)))
    assertEquals(evidence.map(_.parameterNames), List(List("x"), List("x")))
    assert(evidence.flatMap(_.parameterTypes).forall(_.contains("Int")))
    assert(evidence.map(_.resultType).forall(_.contains("Int")))
    assert(evidence.flatMap(_.parameterOwnersAreDefinition).forall(identity))
  }

  test("typed body references resolve to the declared parameter symbol") {
    assertEquals(evidence.map(_.boundReferenceNames), List(List("x"), List("x")))
    assert(evidence.forall(_.boundReferencesUseParameterSymbol))
    assert(evidence.head.bodyKind.contains("Ident"))
    assert(evidence(1).bodyKind.nonEmpty)
    assert(evidence.forall(_.treeStructure.contains("DefDef")))
    assert(evidence.forall(_.treeStructure.contains("ValDef")))
  }
