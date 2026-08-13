package quasiquotes.definitions

class RawDefinitionShapeProbeTest extends munit.FunSuite:
  test("top-level parameterless def and immutable val have stable raw shapes") {
    val source =
      """def answer: Int = 42
        |val value: Int = 42
        |""".stripMargin
    val summaries = RawDefinitionProbe.compilationUnit(source).toOption.get

    assertEquals(
      summaries.map(_.stableShape),
      Vector(
        "DefDef(name=answer,sourceName=answer,paramss=[],type=Ident(Int),body=Number(42),mutable=false,lazy=false)",
        "ValDef(name=value,sourceName=value,paramss=[],type=Ident(Int),body=Number(42),mutable=false,lazy=false)"
      )
    )
    assertEquals(summaries.map(_.placement), Vector(ProbePlacement.TopLevel, ProbePlacement.TopLevel))
  }

  test("class and object members retain the same definition structure") {
    val source =
      """class C:
        |  def answer: Int = 42
        |  val value: Int = 42
        |
        |object O:
        |  def answer: Int = 42
        |  val value: Int = 42
        |""".stripMargin
    val summaries = RawDefinitionProbe.compilationUnit(source).toOption.get

    assertEquals(summaries.map(_.kind), Vector("DefDef", "ValDef", "DefDef", "ValDef"))
    assertEquals(summaries.map(_.name), Vector("answer", "value", "answer", "value"))
    assertEquals(summaries.map(_.placement), Vector.fill(4)(ProbePlacement.Member))
    assert(summaries.forall(_.typeTree == "Ident(Int)"))
    assert(summaries.forall(_.bodyTree == "Number(42)"))
  }

  test("block definitions are local but structurally unchanged") {
    val source =
      """{
        |  def answer: Int = 42
        |  val value: Int = 42
        |  answer
        |}""".stripMargin
    val summaries = RawDefinitionProbe.expression(source).toOption.get

    assertEquals(summaries.map(_.kind), Vector("DefDef", "ValDef"))
    assertEquals(summaries.map(_.placement), Vector(ProbePlacement.Local, ProbePlacement.Local))
    assertEquals(summaries.map(_.parameterClauseSizes), Vector(Nil, Nil))
  }

  test("excluded forms expose deliberate structural reasons") {
    val sources = Vector(
      "def answer = 42",
      "def answer(x: Int): Int = x",
      "val answer = 42",
      "var answer: Int = 42",
      "lazy val answer: Int = 42",
      "type Answer = Int"
    )
    val summaries = sources.map(source => RawDefinitionProbe.compilationUnit(source).toOption.get.head)

    assertEquals(summaries(0).typeTree, "InferredTypeTree")
    assertEquals(summaries(1).parameterClauseSizes, List(1))
    assertEquals(summaries(2).typeTree, "InferredTypeTree")
    assertEquals(summaries(3).isMutable, true)
    assertEquals(summaries(4).isLazy, true)
    assertEquals(summaries(5).kind, "TypeDef")
  }

  test("one ordinary parameter precedes the explicit result type and body") {
    val summaries = RawDefinitionProbe
      .compilationUnit(
        """def id(x: Int): Int = x
          |def inc(x: Int): Int = x + 1
          |""".stripMargin
      )
      .toOption
      .get

    assertEquals(summaries.map(_.parameterClauseSizes), Vector(List(1), List(1)))
    assertEquals(
      summaries.map(_.parameters),
      Vector.fill(2)(
        List(
          RawDefinitionParameterSummary(
            "x",
            "ValDef",
            "Ident(Int)",
            isContextual = false
          )
        )
      )
    )
    assertEquals(
      summaries.map(_.childOrder),
      Vector.fill(2)(List("parameter-0", "result-type", "body"))
    )
    assertEquals(summaries.map(_.typeTree), Vector("Ident(Int)", "Ident(Int)"))
    assertEquals(summaries.head.bodyTree, "Ident(x)")
    assert(summaries(1).bodyTree.contains("Infix"))
  }

  test("plain and backticked fixed names retain decoded and source spellings") {
    val sources = Vector(
      "def answer: Int = 42",
      "def `type`: Int = 42",
      "val answer: Int = 42",
      "val `match`: Int = 42"
    )
    val summaries = sources.map(source => RawDefinitionProbe.compilationUnit(source).toOption.get.head)

    assertEquals(summaries.map(_.name), Vector("answer", "type", "answer", "match"))
    assertEquals(summaries.map(_.sourceName), Vector("answer", "`type`", "answer", "`match`"))
  }
