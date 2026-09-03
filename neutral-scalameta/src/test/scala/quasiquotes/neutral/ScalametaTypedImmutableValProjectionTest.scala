package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedImmutableValProjectionTest extends munit.FunSuite:
  test("projects the canonical explicitly typed immutable val"):
    val result = project(parsed("val answer: Int = 42"))
    val value = immutable(result)

    assertEquals(value.name.decoded, "answer")
    assertEquals(value.name.source, "answer")
    assertEquals(value.name.spelling, DefinitionNameSpelling.Plain)
    assertEquals(value.declaredType, TypeShape.Identifier("Int"))
    assertEquals(value.rhs, TermShape.Literal("42"))
    assertEquals(result.sourceSpan, Some(NeutralSourceSpan(0, 20)))

  test("preserves fully renamed name type and body semantics"):
    val value = immutable(
      project(parsed("val renamedValue: Option[String] = service.answer"))
    )

    assertEquals(value.name.decoded, "renamedValue")
    assertEquals(
      value.declaredType,
      TypeShape.Apply(
        TypeShape.Identifier("Option"),
        List(TypeShape.Identifier("String"))
      )
    )
    assertEquals(
      value.rhs,
      TermShape.Select(TermShape.Identifier("service", false), "answer")
    )

  test("preserves Core backticked-keyword spelling and rejects neighboring name spellings"):
    val keyword = immutable(project(parsed("val `type`: Int = 42")))
    assertEquals(keyword.name.decoded, "type")
    assertEquals(keyword.name.source, "`type`")
    assertEquals(keyword.name.spelling, DefinitionNameSpelling.BacktickedKeyword)

    List(
      parsed("val `answer`: Int = 42"),
      parsed("val `+`: Int = 42"),
      direct("_", Type.Name("Int"), Lit.Int(42))
    ).foreach(assertErrorCode(_, "NEUTRAL_DEFINITION_NAME_UNSUPPORTED"))

  test("reuses N002 TypeShape mapping and admission for nested admitted Types"):
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
        immutable(project(direct(s"value$index", tree, Lit.Int(index)))).declaredType,
        expected,
        clues(sourceType)
      )
    }

  test("N020 and N002 reject the same neighboring Type families"):
    List("Double", "scala.Int", "Map[Int, String]", "Int | String").zipWithIndex.foreach {
      case (sourceType, index) =>
        val tree = parseType(sourceType)
        assert(ScalametaTypeNormalFormProjection.project(tree).isLeft, clues(sourceType))
        assertErrorCode(
          direct(s"value$index", tree, Lit.Int(index)),
          "NEUTRAL_DEFINITION_TYPE_UNSUPPORTED"
        )
    }

  test("rejects missing Types modifiers and unsupported binding topology deterministically"):
    val multiplePatterns = Defn.Val(
      Nil,
      List(Pat.Var(Term.Name("x")), Pat.Var(Term.Name("y"))),
      Some(Type.Name("Int")),
      Lit.Int(42)
    )
    val cases = List(
      parsed("val answer = 42"),
      parsed("lazy val answer: Int = 42"),
      parsed("final val answer: Int = 42"),
      parsed("private val answer: Int = 42"),
      parsed("val (x, y): (Int, Int) = (1, 2)"),
      parsed("val _: Int = 42"),
      multiplePatterns
    )

    cases.foreach(assertErrorCode(_, "NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED"))

  test("projects every representative existing reusable Definition RHS family"):
    val sources = List(
      "val literal: Int = 42",
      "val identifier: Int = source",
      "val selected: Int = service.answer",
      "val applied: Int = service.compute(source)",
      "val combined: Int = left + right",
      "val unary: Int = -value",
      "val tuple: (Int, Int) = (1, 2)",
      "val conditional: Int = if condition then 1 else 2",
      "val interpolated: String = s\"value=$source\"",
      "val nested: String = s\"outer=${s\"inner=$source\"}\"",
      "val typed: Int = (source: Int)"
    )

    sources.foreach(source => assert(ScalametaTypedImmutableValProjection.project(parsed(source)).isRight, clues(source)))

  test("keeps Core DefinitionShape as final body authority after general Term success"):
    val definition = parsed(
      "val built: String = new java.lang.StringBuilder(16)"
    )
    assert(ScalametaTermProjection.project(definition.rhs).isRight)
    assertErrorCode(definition, "NEUTRAL_DEFINITION_CORE_REJECTED")

  test("separates RHS projection failure from Core body rejection"):
    assertErrorCode(
      parsed("val answer: Int = value match { case _ => 1 }"),
      "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )

  test("preserves positioned root span and reports no span for a fresh root"):
    val source = "val answer: Int = 42"
    val positioned = parsed(source)
    assertEquals(
      project(positioned).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = positioned.copy()
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("orders root and topology failures before child projection"):
    assertEquals(
      ScalametaTypedImmutableValProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_DEFINITION_MISSING",
          "the Scalameta Defn.Val must be present."
        )
      )
    )

    val modifiedMissingType = Defn.Val(
      List(Mod.Final()),
      List(Pat.Var(Term.Name("answer"))),
      None,
      Term.Name("ignored")
    )
    assertErrorCode(
      modifiedMissingType,
      "NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED"
    )

  private def immutable(result: ProjectedDefinitionShape): DefinitionShape.ImmutableVal =
    result.shape match
      case value: DefinitionShape.ImmutableVal => value
      case other => fail(s"expected ImmutableVal, found ${other.render}")

  private def project(definition: Defn.Val): ProjectedDefinitionShape =
    ScalametaTypedImmutableValProjection.project(definition) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(definition: Defn.Val, expected: String): Unit =
    assertEquals(
      ScalametaTypedImmutableValProjection.project(definition).left.toOption.map(_.code),
      Some(expected),
      clues(definition)
    )

  private def parsed(source: String): Defn.Val =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Val => definition
      case other => fail(s"expected Defn.Val, found ${other.productPrefix}")

  private def parseType(source: String): Type =
    Scala3(source).parse[Type].get

  private def direct(name: String, declaredType: Type, rhs: Term): Defn.Val =
    Defn.Val(
      Nil,
      List(Pat.Var(Term.Name(name))),
      Some(declaredType),
      rhs
    )
