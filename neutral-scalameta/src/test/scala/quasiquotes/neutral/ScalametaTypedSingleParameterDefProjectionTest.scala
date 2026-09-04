package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedSingleParameterDefProjectionTest extends munit.FunSuite:
  test("projects the canonical parameter as BinderId zero and preserves its root span"):
    val source = "def id(x: Int): Int = x"
    val result = project(parsed(source))
    val method = single(result)

    assertEquals(method.name.decoded, "id")
    assertEquals(method.parameterBinderId, BinderId(0))
    assertEquals(method.parameterName.decoded, "x")
    assertEquals(method.parameterType, TypeShape.Identifier("Int"))
    assertEquals(method.resultType, TypeShape.Identifier("Int"))
    assertEquals(method.body, TermShape.BoundReference(BinderId(0), "x"))
    assertEquals(result.sourceSpan, Some(NeutralSourceSpan(0, source.length)))

  test("preserves renamed and backticked names through the shared DefinitionName authority"):
    val renamed = single(project(parsed("def compute(value: Int): String = s\"v=$value\"")))
    assertEquals(renamed.name.source, "compute")
    assertEquals(renamed.parameterName.source, "value")
    assertEquals(renamed.body.render, "InterpolatedString(s, [\"v=\", \"\"], [BoundRef(value)])")

    val keyword = single(project(parsed("def `type`(`match`: Int): Int = `match`")))
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(keyword.parameterName.source, "`match`")
    assertEquals(keyword.parameterName.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(keyword.body, TermShape.BoundReference(BinderId(0), "match"))

  test("allows equal method and parameter spellings because the body resolves to the parameter"):
    val method = single(project(parsed("def answer(answer: Int): Int = answer")))

    assertEquals(method.name.decoded, "answer")
    assertEquals(method.parameterName.decoded, "answer")
    assertEquals(method.body, TermShape.BoundReference(BinderId(0), "answer"))

  test("resolves the parameter recursively through every admitted representative body family"):
    val expected = List(
      "def f(x: Int): Int = service.compute(x)" ->
        "Apply(Select(Ident(service), compute), [BoundRef(x)])",
      "def f(x: Int): Int = x + 1" -> "Infix(BoundRef(x), +, Literal(1))",
      "def f(x: Int): Int = if flag then x else 0" ->
        "If(Ident(flag), BoundRef(x), Literal(0))",
      "def f(x: Int): String = s\"x=$x\"" ->
        "InterpolatedString(s, [\"x=\", \"\"], [BoundRef(x)])",
      "def f(x: Int): (Int, Int) = (x, 1)" ->
        "Tuple([BoundRef(x), Literal(1)])",
      "def f(x: Int): Int = (x: Int)" -> "Typed(BoundRef(x), Type(Int))"
    )

    expected.foreach { (source, bodyRender) =>
      assertEquals(single(project(parsed(source))).body.render, bodyRender, clues(source))
    }

  test("the internal Term seam binds seeded names and allocates nested binders above them"):
    val seed = ScalametaTermProjection.DefinitionBinder("x", BinderId(0))
    val direct = projectTermWithBinders(parseTerm("x"), Vector(seed))
    assertEquals(direct.shape, TermShape.BoundReference(BinderId(0), "x"))

    val shadowing = projectTermWithBinders(parseTerm("(x: Int) => x"), Vector(seed))
    assertEquals(
      shadowing.shape,
      TermShape.Lambda1(
        BinderId(1),
        "x",
        "Int",
        TermShape.BoundReference(BinderId(1), "x")
      )
    )

    val outerAndInner = projectTermWithBinders(parseTerm("(y: Int) => x + y"), Vector(seed))
    assertEquals(
      outerAndInner.shape,
      TermShape.Lambda1(
        BinderId(1),
        "y",
        "Int",
        TermShape.Infix(
          TermShape.BoundReference(BinderId(0), "x"),
          "+",
          TermShape.BoundReference(BinderId(1), "y")
        )
      )
    )

  test("public Term projection remains unseeded and malformed seed relationships fail closed"):
    assertEquals(
      ScalametaTermProjection.project(Term.Name("x")).toOption.get.shape,
      TermShape.Identifier("x", false)
    )

    val duplicateId = Vector(
      ScalametaTermProjection.DefinitionBinder("x", BinderId(0)),
      ScalametaTermProjection.DefinitionBinder("y", BinderId(0))
    )
    val duplicateName = Vector(
      ScalametaTermProjection.DefinitionBinder("x", BinderId(0)),
      ScalametaTermProjection.DefinitionBinder("x", BinderId(1))
    )
    val nullBinders = null.asInstanceOf[Vector[ScalametaTermProjection.DefinitionBinder]]
    val nullBinderId = Vector(
      ScalametaTermProjection.DefinitionBinder("x", null)
    )
    List(duplicateId, duplicateName, nullBinders, nullBinderId).foreach { seeds =>
      assertEquals(
        ScalametaTermProjection
          .projectWithDefinitionBinders(Term.Name("x"), seeds)
          .left
          .toOption
          .map(_.code),
        Some("NEUTRAL_DEFINITION_BINDER_SCOPE_UNSUPPORTED")
      )
    }

  test("reuses N002 for independently admitted parameter and result Types"):
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

    fixtures.zipWithIndex.foreach { case ((sourceType, expectedType), index) =>
      val tree = parseType(sourceType)
      val resultTree = fixtures((index + 1) % fixtures.size)._1
      val method = single(
        project(direct(s"method$index", s"value$index", tree, parseType(resultTree), Lit.Int(index)))
      )
      assertEquals(method.parameterType, expectedType)
      assert(ScalametaTypeNormalFormProjection.project(tree).isRight)
    }

  test("N022 and N002 reject the same neighboring parameter and result Type families"):
    List("Double", "scala.Int", "Map[Int, String]", "Int | String").zipWithIndex.foreach {
      case (sourceType, index) =>
        val tree = parseType(sourceType)
        assert(ScalametaTypeNormalFormProjection.project(tree).isLeft)
        assertErrorCode(
          direct(s"badParameter$index", "x", tree, Type.Name("Int"), Term.Name("x")),
          "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
        )
        assertErrorCode(
          direct(s"badResult$index", "x", Type.Name("Int"), tree, Term.Name("x")),
          "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
        )
    }

  test("rejects every neighboring single-parameter topology before child projection"):
    val missingParameterType = withParameter(
      parsed("def id(x: Int): Int = x")
    )(_.copy(decltpe = None))
    val topology = List(
      parsed("inline def `answer`(x: Double): Double = value match { case _ => x }"),
      parsed("def id: Int = 1"),
      parsed("def id(): Int = 1"),
      parsed("def id(x: Int, y: Int): Int = x"),
      parsed("def id(x: Int)(y: Int): Int = x"),
      parsed("def id[A](x: Int): Int = x"),
      parsed("def id(using x: Int): Int = x"),
      parsed("def id(x: => Int): Int = x"),
      parsed("def id(x: Int*): Int = x"),
      parsed("def id(x: Int = 1): Int = x"),
      missingParameterType,
      parsed("def id(x: Int) = x")
    )

    topology.foreach(assertErrorCode(_, "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED"))

  test("distinguishes method and parameter name failures while using one stable category"):
    val valid = parsed("def answer(value: Int): Int = value")
    val invalidMethod = valid.copy(name = Term.Name("_"))
    val invalidParameter = withParameter(valid)(_.copy(name = Term.Name("_")))

    val methodError = error(invalidMethod)
    assertEquals(methodError.code, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED")
    assert(methodError.detail.contains("method name"), clues(methodError))
    val parameterError = error(invalidParameter)
    assertEquals(parameterError.code, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED")
    assert(parameterError.detail.contains("parameter name"), clues(parameterError))

  test("separates Term projection failure Core rejection and recursion in the required order"):
    assertErrorCode(
      parsed("def id(x: Int): Int = value match { case _ => x }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )

    val coreRejected = parsed("def build(x: Int): String = new java.lang.StringBuilder(x)")
    val seed = ScalametaTermProjection.DefinitionBinder("x", BinderId(0))
    assert(ScalametaTermProjection.projectWithDefinitionBinders(coreRejected.body, Vector(seed)).isRight)
    assertErrorCode(coreRejected, "NEUTRAL_DEFINITION_CORE_REJECTED")

    List(
      "def answer(x: Int): Int = answer",
      "def answer(x: Int): Int = x + answer",
      "def answer(x: Int): Int = if flag then x else answer",
      "def answer(x: Int): String = s\"$x:$answer\""
    ).foreach(source =>
      assertErrorCode(parsed(source), "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED")
    )

    assert(ScalametaTypedSingleParameterDefProjection.project(
      parsed("def answer(x: Int): Int = service.answer")
    ).isRight)
    assertErrorCode(
      parsed("def answer(x: Int): String = new java.lang.StringBuilder(answer)"),
      "NEUTRAL_DEFINITION_CORE_REJECTED"
    )

  test("orders missing topology names Types body Core and recursion deterministically"):
    assertEquals(
      ScalametaTypedSingleParameterDefProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
    )
    assertErrorCode(
      parsed("inline def `answer`(x: Double): Double = value match { case _ => x }"),
      "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def `answer`(x: Double): Double = value match { case _ => x }"),
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Double): Double = value match { case _ => x }"),
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int): Int = value match { case _ => answer }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int): String = new java.lang.StringBuilder(answer)"),
      "NEUTRAL_DEFINITION_CORE_REJECTED"
    )
    assertErrorCode(
      parsed("def answer(x: Int): Int = x + answer"),
      "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED"
    )

  test("reports no source span for a fresh root"):
    val fresh = parsed("def id(x: Int): Int = x").copy()
    assertEquals(fresh.pos, Position.None)
    assertEquals(project(fresh).sourceSpan, None)

  private def projectTermWithBinders(
      term: Term,
      binders: Vector[ScalametaTermProjection.DefinitionBinder]
  ): ProjectedTermShape =
    ScalametaTermProjection.projectWithDefinitionBinders(term, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def single(
      result: ProjectedDefinitionShape
  ): DefinitionShape.SingleParameterDef =
    result.shape match
      case method: DefinitionShape.SingleParameterDef => method
      case other => fail(s"expected SingleParameterDef, found ${other.render}")

  private def project(definition: Defn.Def): ProjectedDefinitionShape =
    ScalametaTypedSingleParameterDefProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def error(definition: Defn.Def): NeutralProjectionError =
    ScalametaTypedSingleParameterDefProjection.project(definition).left.toOption.getOrElse(
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
      parameterName: String,
      parameterType: Type,
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
              List(Term.Param(Nil, Term.Name(parameterName), Some(parameterType), None))
            )
          )
        )
      ),
      Some(resultType),
      body
    )

  private def withParameter(
      definition: Defn.Def
  )(change: Term.Param => Term.Param): Defn.Def =
    val group = definition.paramClauseGroups.head
    val clause = group.paramClauses.head
    Defn.Def(
      definition.mods,
      definition.name,
      group.tparamClause.values,
      List(List(change(clause.values.head))),
      definition.decltpe,
      definition.body
    )
