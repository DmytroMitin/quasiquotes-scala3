package quasiquotes.phase74

private object TypedBlockLambdaExamples:
  private var free: Int = 40
  private def foo(): Unit = ()

  val lambdaIdentity = TypedBlockLambdaProbe.inspect((x: Int) => x)
  val lambdaAdd = TypedBlockLambdaProbe.inspect((x: Int) => x + 1)
  val lambdaTwo = TypedBlockLambdaProbe.inspect((x: Int, y: Int) => x + y)
  val lambdaNested = TypedBlockLambdaProbe.inspect((x: Int) => ((y: Int) => x + y))
  val lambdaShadowed = TypedBlockLambdaProbe.inspect((x: Int) => ((x: Int) => x))
  val lambdaFree = TypedBlockLambdaProbe.inspect((x: Int) => free + x)

  val singleExpressionBlock = TypedBlockLambdaProbe.inspect({ 1 })
  val expressionStatBlock = TypedBlockLambdaProbe.inspect({ foo(); 1 })
  val localValBlock = TypedBlockLambdaProbe.inspect({ val x: Int = 1; x })
  val localValUseBlock = TypedBlockLambdaProbe.inspect({ val x: Int = 1; x + 1 })
  val shadowedValBlock = TypedBlockLambdaProbe.inspect {
    val x: Int = 1
    { val x: Int = 2; x }
  }
  val freeValBlock = TypedBlockLambdaProbe.inspect({ val x: Int = 1; free + x })
  val localDefBlock = TypedBlockLambdaProbe.inspect {
    def f(x: Int): Int = x + 1
    f(1)
  }

class TypedBlockLambdaProbeTest extends munit.FunSuite:
  private val lambdas = Vector(
    TypedBlockLambdaExamples.lambdaIdentity,
    TypedBlockLambdaExamples.lambdaAdd,
    TypedBlockLambdaExamples.lambdaTwo,
    TypedBlockLambdaExamples.lambdaNested,
    TypedBlockLambdaExamples.lambdaShadowed,
    TypedBlockLambdaExamples.lambdaFree
  )

  private val blocks = Vector(
    TypedBlockLambdaExamples.singleExpressionBlock,
    TypedBlockLambdaExamples.expressionStatBlock,
    TypedBlockLambdaExamples.localValBlock,
    TypedBlockLambdaExamples.localValUseBlock,
    TypedBlockLambdaExamples.shadowedValBlock,
    TypedBlockLambdaExamples.freeValBlock,
    TypedBlockLambdaExamples.localDefBlock
  )

  test("typed lambda extractor exposes parameter symbols, owners, bodies, and source spans") {
    lambdas.foreach { evidence =>
      println(s"PHASE74_TYPED_LAMBDA source=${evidence.source} root=${evidence.rootKind} tree=${evidence.treeStructure}")
      assert(evidence.lambdaCount >= 1)
      assert(evidence.lambdaParameterNames.nonEmpty)
      assert(evidence.lambdaParameterTypes.forall(_.contains("Int")))
      assert(evidence.lambdaParameterOwnersAreMethods.forall(identity))
      assert(evidence.boundReferenceNames.nonEmpty)
      assert(evidence.sourceSpans.exists(_.startsWith("lambda:")))
      assert(evidence.sourceSpans.exists(_.startsWith("lambda-param-0:")))
      assert(evidence.sourceSpans.exists(_.startsWith("lambda-body:")))
    }
  }

  test("typed nested lambdas distinguish captured outer parameters and same-name shadowing") {
    val nested = TypedBlockLambdaExamples.lambdaNested
    assertEquals(nested.lambdaCount, 2)
    assertEquals(nested.lambdaParameterNames, List("x", "y"))
    assert(nested.boundReferenceNames.contains("x"))
    assert(nested.boundReferenceNames.contains("y"))

    val shadowed = TypedBlockLambdaExamples.lambdaShadowed
    assertEquals(shadowed.lambdaCount, 2)
    assertEquals(shadowed.lambdaParameterNames, List("x", "x"))
    assert(shadowed.sameNameShadowingUsesDistinctSymbols)
  }

  test("typed free references remain distinct from lambda parameter symbols") {
    val evidence = TypedBlockLambdaExamples.lambdaFree
    assertEquals(evidence.freeReferenceNames, List("free"))
    assert(evidence.boundReferenceNames.contains("x"))
  }

  test("single-expression braces collapse while sequencing survives as a typed Block") {
    blocks.foreach { evidence =>
      println(
        s"PHASE74_TYPED_BLOCK source=${evidence.source} root=${evidence.rootKind} " +
          s"stats=${evidence.blockStatKinds.mkString("[", ",", "]")} " +
          s"results=${evidence.blockResultKinds.mkString("[", ",", "]")} tree=${evidence.treeStructure}"
      )
    }
    val single = TypedBlockLambdaExamples.singleExpressionBlock
    val sequenced = TypedBlockLambdaExamples.expressionStatBlock
    assertEquals(single.regularBlockCount, 0)
    assert(!single.rootKind.contains("Block"))
    assertEquals(sequenced.regularBlockCount, 1)
    assertEquals(sequenced.blockStatKinds.size, 1)
    assertEquals(sequenced.blockResultKinds.size, 1)
  }

  test("typed local val and def blocks retain symbol-linked references") {
    val localVal = TypedBlockLambdaExamples.localValUseBlock
    assertEquals(localVal.localValueNames, List("x"))
    assert(localVal.localBinderReferenceNames.contains("x"))
    assert(localVal.localBinderOwnersExist.forall(identity))

    val localDef = TypedBlockLambdaExamples.localDefBlock
    assertEquals(localDef.localDefNames, List("f"))
    assert(localDef.localBinderReferenceNames.contains("f"))
    assert(localDef.localBinderOwnersExist.forall(identity))
  }

  test("typed local shadowing creates distinct scopes while the result resolves inward") {
    val evidence = TypedBlockLambdaExamples.shadowedValBlock
    assertEquals(evidence.localValueNames, List("x", "x"))
    assert(evidence.localBinderReferenceNames.contains("x"))
    assert(evidence.regularBlockCount >= 2)
  }

  test("Lambda builder preserves an external same-text identifier instead of capturing it") {
    val x = 40
    val generated = TypedBlockLambdaProbe.hygienicAdder(x)
    assertEquals(generated(2), 42)
  }
