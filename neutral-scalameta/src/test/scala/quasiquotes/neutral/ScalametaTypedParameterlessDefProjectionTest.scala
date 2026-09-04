package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedParameterlessDefProjectionTest extends munit.FunSuite:
  test("projects the canonical explicitly typed parameterless def"):
    val result = project(parsed("def answer: Int = 42"))
    val method = parameterless(result)

    assertEquals(method.name.decoded, "answer")
    assertEquals(method.name.source, "answer")
    assertEquals(method.name.spelling, DefinitionNameSpelling.Plain)
    assertEquals(method.resultType, TypeShape.Identifier("Int"))
    assertEquals(method.body, TermShape.Literal("42"))
    assertEquals(result.sourceSpan, Some(NeutralSourceSpan(0, 20)))

  test("preserves renamed plain and Core-admitted backticked-keyword names"):
    val renamed = parameterless(
      project(parsed("def renamedMethod: Option[String] = service.answer"))
    )
    assertEquals(renamed.name.decoded, "renamedMethod")
    assertEquals(renamed.name.source, "renamedMethod")
    assertEquals(
      renamed.resultType,
      TypeShape.Apply(
        TypeShape.Identifier("Option"),
        List(TypeShape.Identifier("String"))
      )
    )
    assertEquals(
      renamed.body,
      TermShape.Select(TermShape.Identifier("service", false), "answer")
    )

    val keyword = parameterless(project(parsed("def `type`: Int = 42")))
    assertEquals(keyword.name.decoded, "type")
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)

  test("rejects neighboring method-name spellings through the shared N020 authority"):
    List(
      parsed("def `answer`: Int = 42"),
      parsed("def `+`: Int = 42"),
      direct("_", Type.Name("Int"), Lit.Int(42))
    ).foreach(assertErrorCode(_, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"))

  test("reuses N002 TypeShape mapping and admission for nested admitted result Types"):
    val fixtures = List(
      "List[Int]" ->
        TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("Int"))),
      "(Int, String)" ->
        TypeShape.Tuple(List(TypeShape.Identifier("Int"), TypeShape.Identifier("String"))),
      "(Int, String) => Boolean" ->
        TypeShape.Function(
          List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
          TypeShape.Identifier("Boolean")
        ),
      "Either[List[Int], Option[String]]" ->
        TypeShape.Apply(
          TypeShape.Identifier("Either"),
          List(
            TypeShape.Apply(
              TypeShape.Identifier("List"),
              List(TypeShape.Identifier("Int"))
            ),
            TypeShape.Apply(
              TypeShape.Identifier("Option"),
              List(TypeShape.Identifier("String"))
            )
          )
        )
    )

    fixtures.zipWithIndex.foreach { case ((sourceType, expected), index) =>
      val tree = parseType(sourceType)
      assert(ScalametaTypeNormalFormProjection.project(tree).isRight, clues(sourceType))
      assertEquals(
        parameterless(project(direct(s"method$index", tree, Lit.Int(index)))).resultType,
        expected,
        clues(sourceType)
      )
    }

  test("N021 and N002 reject the same neighboring result-Type families"):
    List("Double", "scala.Int", "Map[Int, String]", "Int | String").zipWithIndex.foreach {
      case (sourceType, index) =>
        val tree = parseType(sourceType)
        assert(ScalametaTypeNormalFormProjection.project(tree).isLeft, clues(sourceType))
        assertErrorCode(
          direct(s"method$index", tree, Lit.Int(index)),
          "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
        )
    }

  test("admits only true parameterless explicitly typed modifier-free topology"):
    val cases = List(
      parsed("def answer = 42"),
      parsed("inline def answer: Int = 42"),
      parsed("private def answer: Int = 42"),
      parsed("def answer(): Int = 42"),
      parsed("def answer(x: Int): Int = x"),
      parsed("def answer[A]: Int = 42"),
      parsed("def answer(using value: Int): Int = value"),
      parsed("def answer(x: Int)(y: Int): Int = x")
    )

    cases.foreach(assertErrorCode(_, "NEUTRAL_PARAMETERLESS_DEF_TOPOLOGY_UNSUPPORTED"))

  test("projects representative existing reusable Definition body families"):
    val sources = List(
      "def literal: Int = 42",
      "def identifier: Int = source",
      "def selected: Int = service.answer",
      "def applied: Int = service.compute(source)",
      "def combined: Int = left + right",
      "def unary: Int = -value",
      "def tuple: (Int, Int) = (1, 2)",
      "def conditional: Int = if condition then 1 else 2",
      "def interpolated: String = s\"value=$source\"",
      "def nested: String = s\"outer=${s\"inner=$source\"}\"",
      "def typed: Int = (source: Int)"
    )

    sources.foreach(source => assert(ScalametaTypedParameterlessDefProjection.project(parsed(source)).isRight, clues(source)))

  test("allows a different free name and the declaration spelling as a selected member only"):
    assert(ScalametaTypedParameterlessDefProjection.project(parsed("def answer: Int = source")).isRight)
    assert(ScalametaTypedParameterlessDefProjection.project(parsed("def answer: Int = service.answer")).isRight)

  test("rejects direct and transitively nested unqualified self-reference"):
    val recursive = List(
      "def answer: Int = answer",
      "def answer: Int = answer(source)",
      "def answer: Int = answer.value",
      "def answer: Int = answer + 1",
      "def answer: String = s\"$answer\"",
      "def answer: (Int, Int) = (answer, 1)",
      "def answer: Int = if condition then answer else 1",
      "def answer: Int = (answer: Int)"
    )

    recursive.foreach(source =>
      assertErrorCode(parsed(source), "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED")
    )

  test("keeps Core DefinitionShape as final body authority after general Term success"):
    val definition = parsed("def built: String = new java.lang.StringBuilder(16)")
    assert(ScalametaTermProjection.project(definition.body).isRight)
    assertErrorCode(definition, "NEUTRAL_DEFINITION_CORE_REJECTED")

  test("separates body projection failure from Core rejection"):
    assertErrorCode(
      parsed("def answer: Int = value match { case _ => 1 }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )

  test("checks Core rejection before recursion inside a Core-excluded body"):
    val definition = parsed(
      "def answer: String = new java.lang.StringBuilder(answer)"
    )
    assert(ScalametaTermProjection.project(definition.body).isRight)
    assertErrorCode(definition, "NEUTRAL_DEFINITION_CORE_REJECTED")

  test("preserves positioned root span and reports no span for a fresh root"):
    val source = "def answer: Int = 42"
    val positioned = parsed(source)
    assertEquals(project(positioned).sourceSpan, Some(NeutralSourceSpan(0, source.length)))

    val unpositioned = positioned.copy()
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("orders root topology name Type body Core and recursion failures"):
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
    )

    assertErrorCode(
      parsed("inline def `answer`(): Double = value match { case _ => 1 }"),
      "NEUTRAL_PARAMETERLESS_DEF_TOPOLOGY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def `answer`: Double = value match { case _ => 1 }"),
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer: Double = value match { case _ => 1 }"),
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer: Int = value match { case _ => answer }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("def answer: String = new java.lang.StringBuilder(answer)"),
      "NEUTRAL_DEFINITION_CORE_REJECTED"
    )
    assertErrorCode(
      parsed("def answer: Int = answer"),
      "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED"
    )

  private def parameterless(
      result: ProjectedDefinitionShape
  ): DefinitionShape.ParameterlessDef =
    result.shape match
      case method: DefinitionShape.ParameterlessDef => method
      case other => fail(s"expected ParameterlessDef, found ${other.render}")

  private def project(definition: Defn.Def): ProjectedDefinitionShape =
    ScalametaTypedParameterlessDefProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(definition: Defn.Def, expected: String): Unit =
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(definition).left.toOption.map(_.code),
      Some(expected),
      clues(definition)
    )

  private def parsed(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def parseType(source: String): Type =
    Scala3(source).parse[Type].get

  private def direct(name: String, resultType: Type, body: Term): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(name),
      Nil,
      Some(resultType),
      body
    )
