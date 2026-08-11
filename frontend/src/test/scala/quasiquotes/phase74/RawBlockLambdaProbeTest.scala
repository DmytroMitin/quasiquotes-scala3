package quasiquotes.phase74

class RawBlockLambdaProbeTest extends munit.FunSuite:
  private val lambdaSources = Vector(
    "(x: Int) => x",
    "(x: Int) => x + 1",
    "(x: Int, y: Int) => x + y",
    "(x: Int) => ((y: Int) => x + y)",
    "(x: Int) => ((x: Int) => x)",
    "(x: Int) => free + x"
  )

  lambdaSources.foreach { source =>
    test(s"raw lambda records ordered parameters, body, spans, and NoSymbol: $source") {
      val evidence = RawBlockLambdaProbe.expression(source).fold(errors => fail(errors.mkString("; ")), identity)
      println(s"PHASE74_RAW_LAMBDA source=${RawBlockLambdaProbe.quoted(source)} tree=${evidence.root.compact}")

      val nodes = evidence.allNodes
      assertEquals(evidence.root.kind, "Function")
      assertEquals(evidence.root.start, 0)
      assertEquals(evidence.root.end, source.length)
      assert(evidence.arrowStart.exists(_ >= 0))
      assertEquals(evidence.arrowEnd, evidence.arrowStart.map(_ + 2))
      assert(nodes.forall(_.noSymbol))
      assert(nodes.exists(_.kind == "ValDef"))
      assert(nodes.exists(node => node.kind == "Ident" && node.detail.contains("name=Int")))
    }
  }

  private val blockSources = Vector(
    "{ 1 }",
    "{ foo(); 1 }",
    "{ val x: Int = 1; x }",
    "{ val x: Int = 1; x + 1 }",
    "{ val x: Int = 1; { val x: Int = 2; x } }",
    "{ val x: Int = 1; free + x }",
    "{ def f(x: Int): Int = x + 1; f(1) }"
  )

  blockSources.foreach { source =>
    test(s"raw block records ordered stats, result, binders, and spans: $source") {
      val evidence = RawBlockLambdaProbe.expression(source).fold(errors => fail(errors.mkString("; ")), identity)
      println(s"PHASE74_RAW_BLOCK source=${RawBlockLambdaProbe.quoted(source)} tree=${evidence.root.compact}")

      val nodes = evidence.allNodes
      assertEquals(evidence.root.kind, "Block")
      assertEquals(evidence.root.start, 0)
      assertEquals(evidence.root.end, source.length)
      assert(nodes.forall(_.noSymbol))
      if source.contains("val x") then assert(nodes.exists(_.kind == "ValDef"))
      if source.contains("def f") then assert(nodes.exists(_.kind == "DefDef"))
    }
  }

  test("raw block child order distinguishes expression stats from the final result") {
    val evidence = RawBlockLambdaProbe.expression("{ foo(); 1 }").toOption.get
    assertEquals(evidence.root.children.map(_.kind), Vector("Apply", "Number"))
    assertEquals(evidence.root.detail, "stats=1")
  }

  test("raw nested shadowing retains two distinct ordered ValDef nodes") {
    val evidence = RawBlockLambdaProbe
      .expression("{ val x: Int = 1; { val x: Int = 2; x } }")
      .toOption.get
    val values = evidence.allNodes.filter(_.kind == "ValDef")
    assertEquals(values.size, 2)
    assert(values.forall(_.detail.contains("name=x")))
    assert(values.head.start < values(1).start)
  }
