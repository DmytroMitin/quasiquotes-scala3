package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedTwoParameterDefProjectionTest extends munit.FunSuite:
  test("projects canonical parameters as ordered BinderIds zero and one with a truthful root span"):
    val source = "def pair(x: Int, y: Int): (Int, Int) = (x, y)"
    val result = project(parsed(source))
    val method = two(result)

    assertEquals(method.name.decoded, "pair")
    assertEquals(method.firstParameterBinderId, BinderId(0))
    assertEquals(method.firstParameterName.decoded, "x")
    assertEquals(method.firstParameterType, TypeShape.Identifier("Int"))
    assertEquals(method.secondParameterBinderId, BinderId(1))
    assertEquals(method.secondParameterName.decoded, "y")
    assertEquals(method.secondParameterType, TypeShape.Identifier("Int"))
    assertEquals(
      method.resultType,
      TypeShape.Tuple(List(TypeShape.Identifier("Int"), TypeShape.Identifier("Int")))
    )
    assertEquals(
      method.body,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(BinderId(0), "x"),
          TermShape.BoundReference(BinderId(1), "y")
        )
      )
    )
    assertEquals(result.sourceSpan, Some(NeutralSourceSpan(0, source.length)))

    val swapped = two(project(parsed("def pair(x: Int, y: Int): (Int, Int) = (y, x)")))
    assertNotEquals(method.body, swapped.body)

  test("preserves renamed and backticked names through the shared DefinitionName authority"):
    val renamed = two(project(parsed("def compute(left: Int, right: Int): Int = left + right")))
    assertEquals(renamed.name.source, "compute")
    assertEquals(renamed.firstParameterName.source, "left")
    assertEquals(renamed.secondParameterName.source, "right")

    val keyword = two(project(parsed(
      "def `type`(`match`: Int, `then`: Int): Int = `match` + `then`"
    )))
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(keyword.firstParameterName.source, "`match`")
    assertEquals(keyword.firstParameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(keyword.secondParameterName.source, "`then`")
    assertEquals(keyword.secondParameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(
      keyword.body,
      TermShape.Infix(
        TermShape.BoundReference(BinderId(0), "match"),
        "+",
        TermShape.BoundReference(BinderId(1), "then")
      )
    )

  test("allows the method spelling to equal either distinct parameter spelling"):
    val first = two(project(parsed("def answer(answer: Int, y: Int): Int = answer + y")))
    assertEquals(first.name.decoded, "answer")
    assertEquals(first.firstParameterName.decoded, "answer")
    assertEquals(
      first.body,
      TermShape.Infix(
        TermShape.BoundReference(BinderId(0), "answer"),
        "+",
        TermShape.BoundReference(BinderId(1), "y")
      )
    )

    val second = two(project(parsed("def answer(x: Int, answer: Int): Int = x + answer")))
    assertEquals(second.name.decoded, "answer")
    assertEquals(second.secondParameterName.decoded, "answer")
    assertEquals(
      second.body,
      TermShape.Infix(
        TermShape.BoundReference(BinderId(0), "x"),
        "+",
        TermShape.BoundReference(BinderId(1), "answer")
      )
    )

  test("resolves both parameters recursively through representative admitted body families"):
    val expected = List(
      "def f(x: Int, y: Int): Int = x" -> "BoundRef(x)",
      "def f(x: Int, y: Int): Int = x.value + y.value" ->
        "Infix(Select(BoundRef(x), value), +, Select(BoundRef(y), value))",
      "def f(x: Int, y: Int): Int = service.compute(x, y)" ->
        "Apply(Select(Ident(service), compute), [BoundRef(x), BoundRef(y)])",
      "def f(x: Int, y: Int): Int = x + y" ->
        "Infix(BoundRef(x), +, BoundRef(y))",
      "def f(x: Int, y: Int): Int = -x + y" ->
        "Infix(Unary(-, BoundRef(x)), +, BoundRef(y))",
      "def f(x: Int, y: Int): (Int, Int) = (x, y)" ->
        "Tuple([BoundRef(x), BoundRef(y)])",
      "def f(x: Int, y: Int): Int = if flag then x else y" ->
        "If(Ident(flag), BoundRef(x), BoundRef(y))",
      "def f(x: Int, y: Int): String = s\"$x:$y\"" ->
        "InterpolatedString(s, [\"\", \":\", \"\"], [BoundRef(x), BoundRef(y)])",
      "def f(x: Int, y: Int): String = s\"$x:${s\"$y\"}\"" ->
        "InterpolatedString(s, [\"\", \":\", \"\"], [BoundRef(x), InterpolatedString(s, [\"\", \"\"], [BoundRef(y)])])",
      "def f(x: Int, y: Int): (Int, Int) = ((x: Int), (y: Int))" ->
        "Tuple([Typed(BoundRef(x), Type(Int)), Typed(BoundRef(y), Type(Int))])"
    )

    expected.foreach { (source, bodyRender) =>
      assertEquals(two(project(parsed(source))).body.render, bodyRender, clues(source))
    }

  test("the N022 Term seam binds two seeds and allocates nested binders from two"):
    val seeds = Vector(
      ScalametaTermProjection.DefinitionBinder("x", BinderId(0)),
      ScalametaTermProjection.DefinitionBinder("y", BinderId(1))
    )
    val nested = projectTermWithBinders(parseTerm("(z: Int) => x + y + z"), seeds)
    assertEquals(
      nested.shape,
      TermShape.Lambda1(
        BinderId(2),
        "z",
        "Int",
        TermShape.Infix(
          TermShape.Infix(
            TermShape.BoundReference(BinderId(0), "x"),
            "+",
            TermShape.BoundReference(BinderId(1), "y")
          ),
          "+",
          TermShape.BoundReference(BinderId(2), "z")
        )
      )
    )

    val shadowFirst = projectTermWithBinders(parseTerm("(x: Int) => x + y"), seeds)
    assertEquals(
      shadowFirst.shape,
      TermShape.Lambda1(
        BinderId(2),
        "x",
        "Int",
        TermShape.Infix(
          TermShape.BoundReference(BinderId(2), "x"),
          "+",
          TermShape.BoundReference(BinderId(1), "y")
        )
      )
    )

    val shadowSecond = projectTermWithBinders(parseTerm("(y: Int) => x + y"), seeds)
    assertEquals(
      shadowSecond.shape,
      TermShape.Lambda1(
        BinderId(2),
        "y",
        "Int",
        TermShape.Infix(
          TermShape.BoundReference(BinderId(0), "x"),
          "+",
          TermShape.BoundReference(BinderId(2), "y")
        )
      )
    )

  test("public Term projection remains unseeded"):
    assertEquals(
      ScalametaTermProjection.project(Term.Name("x")).toOption.get.shape,
      TermShape.Identifier("x", false)
    )
    assertEquals(
      ScalametaTermProjection.project(Term.Name("y")).toOption.get.shape,
      TermShape.Identifier("y", false)
    )

  test("reuses N002 independently for first parameter second parameter and result Types"):
    val fixtures = List(
      "List[Int]" -> TypeShape.Apply(
        TypeShape.Identifier("List"),
        List(TypeShape.Identifier("Int"))
      ),
      "(Int, String)" -> TypeShape.Tuple(
        List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))
      ),
      "(Int, String) => Boolean" -> TypeShape.Function(
        List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
        TypeShape.Identifier("Boolean")
      ),
      "Either[List[Int], Option[String]]" -> TypeShape.Apply(
        TypeShape.Identifier("Either"),
        List(
          TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("Int"))),
          TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("String")))
        )
      )
    )

    fixtures.foreach { case (sourceType, expectedType) =>
      val tree = parseType(sourceType)
      assert(ScalametaTypeNormalFormProjection.project(tree).isRight)
      assertEquals(two(project(direct("first", "x", tree, "y", Type.Name("Int"), Type.Name("Int"), Term.Name("x")))).firstParameterType, expectedType)
      assertEquals(two(project(direct("second", "x", Type.Name("Int"), "y", tree, Type.Name("Int"), Term.Name("y")))).secondParameterType, expectedType)
      assertEquals(two(project(direct("result", "x", Type.Name("Int"), "y", Type.Name("Int"), tree, Term.Name("x")))).resultType, expectedType)
    }

  test("N023 and N002 reject the same neighboring families in all three Type roles"):
    List("Double", "scala.Int", "Map[Int, String]", "Int | String").foreach { sourceType =>
      val tree = parseType(sourceType)
      assert(ScalametaTypeNormalFormProjection.project(tree).isLeft)
      assertErrorCode(
        direct("badFirst", "x", tree, "y", Type.Name("Int"), Type.Name("Int"), Term.Name("x")),
        "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
      )
      assertErrorCode(
        direct("badSecond", "x", Type.Name("Int"), "y", tree, Type.Name("Int"), Term.Name("y")),
        "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
      )
      assertErrorCode(
        direct("badResult", "x", Type.Name("Int"), "y", Type.Name("Int"), tree, Term.Name("x")),
        "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
      )
    }

  test("rejects every constructible neighboring topology before child projection"):
    val canonical = parsed("def combine(x: Int, y: Int): Int = x + y")
    val missingFirstType = withParameters(canonical)((first, second) =>
      (first.copy(decltpe = None), second)
    )
    val missingSecondType = withParameters(canonical)((first, second) =>
      (first, second.copy(decltpe = None))
    )
    val topology = List(
      parsed("inline def `answer`(x: Double, y: Double): Double = value match { case _ => x }"),
      parsed("def combine: Int = 0"),
      parsed("def combine(): Int = 0"),
      parsed("def combine(x: Int): Int = x"),
      parsed("def combine(x: Int, y: Int, z: Int): Int = x"),
      parsed("def combine(x: Int)(y: Int): Int = x"),
      parsed("def combine[A](x: Int, y: Int): Int = x"),
      parsed("def combine(using x: Int, y: Int): Int = x"),
      parsed("def combine(x: => Int, y: Int): Int = x"),
      parsed("def combine(x: Int, y: => Int): Int = x"),
      parsed("def combine(x: Int, y: Int*): Int = x"),
      parsed("def combine(x: Int = 0, y: Int): Int = y"),
      parsed("def combine(x: Int, y: Int = 0): Int = x"),
      missingFirstType,
      missingSecondType,
      canonical.copy(decltpe = None)
    )

    topology.foreach(assertErrorCode(_, "NEUTRAL_TWO_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED"))

  test("distinguishes all name roles and rejects duplicate parameter names before Types and body"):
    val valid = parsed("def answer(x: Int, y: Int): Int = x + y")
    val invalidMethod = valid.copy(name = Term.Name("_"))
    val invalidFirst = withParameters(valid)((first, second) =>
      (first.copy(name = Term.Name("_")), second)
    )
    val invalidSecond = withParameters(valid)((first, second) =>
      (first, second.copy(name = Term.Name("_")))
    )

    List(
      invalidMethod -> "method name",
      invalidFirst -> "first parameter name",
      invalidSecond -> "second parameter name"
    ).foreach { (definition, role) =>
      val problem = error(definition)
      assertEquals(problem.code, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED")
      assert(problem.detail.contains(role), clues(problem))
    }

    val duplicate = direct(
      "answer",
      "same",
      parseType("Double"),
      "same",
      parseType("Double"),
      parseType("Double"),
      Scala3("value match { case _ => value }").parse[Term].get
    )
    val duplicateProblem = error(duplicate)
    assertEquals(duplicateProblem.code, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED")
    assert(duplicateProblem.detail.contains("distinct"), clues(duplicateProblem))

  test("separates Term projection failure Core rejection and recursion in the required order"):
    assertErrorCode(
      parsed("def pair(x: Int, y: Int): Int = value match { case _ => x + y }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )

    val coreRejected = parsed(
      "def build(x: Int, y: Int): String = new java.lang.StringBuilder(x + y)"
    )
    val seeds = Vector(
      ScalametaTermProjection.DefinitionBinder("x", BinderId(0)),
      ScalametaTermProjection.DefinitionBinder("y", BinderId(1))
    )
    assert(ScalametaTermProjection.projectWithDefinitionBinders(coreRejected.body, seeds).isRight)
    assertErrorCode(coreRejected, "NEUTRAL_DEFINITION_CORE_REJECTED")

    List(
      "def answer(x: Int, y: Int): Int = answer",
      "def answer(x: Int, y: Int): Int = x + answer",
      "def answer(x: Int, y: Int): Int = if flag then y else answer",
      "def answer(x: Int, y: Int): String = s\"$x:$y:$answer\""
    ).foreach(source =>
      assertErrorCode(parsed(source), "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED")
    )

    assert(ScalametaTypedTwoParameterDefProjection.project(
      parsed("def answer(x: Int, y: Int): Int = service.answer")
    ).isRight)
    assertErrorCode(
      parsed("def answer(x: Int, y: Int): String = new java.lang.StringBuilder(answer)"),
      "NEUTRAL_DEFINITION_CORE_REJECTED"
    )

  test("orders missing topology names Types body Core and recursion deterministically"):
    assertEquals(
      ScalametaTypedTwoParameterDefProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
    )
    assertErrorCode(
      parsed("inline def `answer`(x: Double, y: Double): Double = value match { case _ => x }"),
      "NEUTRAL_TWO_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def `answer`(x: Double, y: Double): Double = value match { case _ => x }"),
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Double, y: Double): Double = value match { case _ => x }"),
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int, y: Int): Int = value match { case _ => answer }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int, y: Int): String = new java.lang.StringBuilder(answer)"),
      "NEUTRAL_DEFINITION_CORE_REJECTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int, y: Int): Int = x + answer"),
      "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED"
    )

  test("reports no source span for a fresh root"):
    val fresh = parsed("def pair(x: Int, y: Int): (Int, Int) = (x, y)").copy()
    assertEquals(fresh.pos, Position.None)
    assertEquals(project(fresh).sourceSpan, None)

  private def projectTermWithBinders(
      term: Term,
      binders: Vector[ScalametaTermProjection.DefinitionBinder]
  ): ProjectedTermShape =
    ScalametaTermProjection.projectWithDefinitionBinders(term, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def two(result: ProjectedDefinitionShape): DefinitionShape.TwoParameterDef =
    result.shape match
      case method: DefinitionShape.TwoParameterDef => method
      case other => fail(s"expected TwoParameterDef, found ${other.render}")

  private def project(definition: Defn.Def): ProjectedDefinitionShape =
    ScalametaTypedTwoParameterDefProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def error(definition: Defn.Def): NeutralProjectionError =
    ScalametaTypedTwoParameterDefProjection.project(definition).left.toOption.getOrElse(
      fail(s"expected failure for $definition")
    )

  private def assertErrorCode(definition: Defn.Def, expected: String): Unit =
    assertEquals(error(definition).code, expected, clues(definition))

  private def parsed(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def parseTerm(source: String): Term = Scala3(source).parse[Term].get
  private def parseType(source: String): Type = Scala3(source).parse[Type].get

  private def direct(
      methodName: String,
      firstName: String,
      firstType: Type,
      secondName: String,
      secondType: Type,
      resultType: Type,
      body: Term
  ): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(
                Term.Param(Nil, Term.Name(firstName), Some(firstType), None),
                Term.Param(Nil, Term.Name(secondName), Some(secondType), None)
              )
            )
          )
        )
      ),
      Some(resultType),
      body
    )

  private def withParameters(
      definition: Defn.Def
  )(
      change: (Term.Param, Term.Param) => (Term.Param, Term.Param)
  ): Defn.Def =
    val group = definition.paramClauseGroups.head
    val clause = group.paramClauses.head
    val (first, second) = change(clause.values.head, clause.values(1))
    Defn.Def(
      definition.mods,
      definition.name,
      group.tparamClause.values,
      List(List(first, second)),
      definition.decltpe,
      definition.body
    )
