package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.TypeShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaSimpleTypeAliasProjectionTest extends munit.FunSuite:
  test("projects the canonical reusable simple type alias"):
    val result = project(parsed("type Result = Option[Int]"))
    val alias = simpleAlias(result)

    assertEquals(alias.name.decoded, "Result")
    assertEquals(alias.name.source, "Result")
    assertEquals(alias.name.spelling, DefinitionNameSpelling.Plain)
    assertEquals(
      alias.rhs,
      TypeShape.Apply(
        TypeShape.Identifier("Option"),
        List(TypeShape.Identifier("Int"))
      )
    )
    assertEquals(result.sourceSpan, Some(NeutralSourceSpan(0, 25)))

  test("preserves ordinary and Core-admitted backticked Type names"):
    val ordinary = simpleAlias(project(parsed("type Evidence = Int")))
    val keyword = simpleAlias(project(parsed("type `type` = Int")))

    assertEquals(ordinary.name.decoded, "Evidence")
    assertEquals(ordinary.name.source, "Evidence")
    assertEquals(keyword.name.decoded, "type")
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)

  test("rejects backticked non-keyword operator-like and underscore Type names"):
    List(
      parsed("type `Result` = Int"),
      direct(Type.Name("+"), Type.Name("Int")),
      direct(Type.Name("_"), Type.Name("Int"))
    ).foreach(assertErrorCode(_, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"))

  test("reuses N002 TypeShape mapping for every representative admitted RHS"):
    val fixtures = List(
      "Int" -> TypeShape.Identifier("Int"),
      "Option[Int]" ->
        TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("Int"))),
      "List[String]" ->
        TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("String"))),
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
      val rhs = parseType(sourceType)
      assert(ScalametaTypeNormalFormProjection.project(rhs).isRight, clues(sourceType))
      assertEquals(
        simpleAlias(project(direct(Type.Name(s"Alias$index"), rhs))).rhs,
        expected,
        clues(sourceType)
      )
    }

  test("N024 and N002 reject the same neighboring RHS Type families"):
    List("Double", "scala.Int", "Map[Int, String]", "Int | String").zipWithIndex.foreach {
      case (sourceType, index) =>
        val rhs = parseType(sourceType)
        assert(ScalametaTypeNormalFormProjection.project(rhs).isLeft, clues(sourceType))
        assertErrorCode(
          direct(Type.Name(s"Alias$index"), rhs),
          "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
        )
    }

  test("rejects generic bounded opaque and modifier-bearing alias topologies"):
    val cases = List(
      parsed("type Result[A] = A"),
      parsed("type Result >: Int <: AnyVal = Int"),
      parsed("opaque type Result = Int"),
      parsed("private type Result = Int"),
      parsed("protected type Result = Int")
    )

    cases.foreach(assertErrorCode(_, "NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED"))

  test("preserves positioned root span and reports none for a fresh root"):
    val source = "type Result = (Int, String)"
    val positioned = parsed(source)
    assertEquals(
      project(positioned).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = positioned.copy()
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("orders root topology name and RHS Type failures deterministically"):
    assertEquals(
      ScalametaSimpleTypeAliasProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Type must be present."
        )
      )
    )

    val topologyBeforeChildren = Defn.Type(
      List(Mod.Private(Name.Anonymous())),
      Type.Name("+"),
      Type.ParamClause(List(Type.Param(Nil, Type.Name("A"), Type.ParamClause(Nil), Type.Bounds.empty))),
      parseType("Double"),
      Type.Bounds(None, Some(Type.Name("Any")), Nil, Nil)
    )
    assertErrorCode(
      topologyBeforeChildren,
      "NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED"
    )
    assertErrorCode(
      direct(Type.Name("+"), parseType("Double")),
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"
    )
    assertErrorCode(
      direct(Type.Name("Result"), parseType("Double")),
      "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
    )

  private def simpleAlias(
      result: ProjectedDefinitionShape
  ): DefinitionShape.SimpleTypeAlias =
    result.shape match
      case alias: DefinitionShape.SimpleTypeAlias => alias
      case other => fail(s"expected SimpleTypeAlias, found ${other.render}")

  private def project(definition: Defn.Type): ProjectedDefinitionShape =
    ScalametaSimpleTypeAliasProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(definition: Defn.Type, expected: String): Unit =
    assertEquals(
      ScalametaSimpleTypeAliasProjection.project(definition).left.toOption.map(_.code),
      Some(expected),
      clues(definition)
    )

  private def parsed(source: String): Defn.Type =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Type => definition
      case other => fail(s"expected Defn.Type, found ${other.productPrefix}")

  private def parseType(source: String): Type =
    Scala3(source).parse[Type].get

  private def direct(name: Type.Name, rhs: Type): Defn.Type =
    Defn.Type(
      Nil,
      name,
      Type.ParamClause(Nil),
      rhs,
      Type.Bounds.empty
    )
