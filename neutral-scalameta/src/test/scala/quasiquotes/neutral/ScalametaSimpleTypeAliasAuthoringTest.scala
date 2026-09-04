package quasiquotes.neutral

import _root_.quasiquotes.definitions.{
  DefinitionError,
  DefinitionName,
  DefinitionNameSpelling,
  DefinitionShape
}
import _root_.quasiquotes.parser.TypeShape

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaSimpleTypeAliasAuthoringTest extends munit.FunSuite:
  private final case class Fixture(
      label: String,
      shape: DefinitionShape.SimpleTypeAlias,
      expectedRhs: Type
  )

  private val resultName = DefinitionName.plain("Result").toOption.get
  private val fixtures = List(
    Fixture(
      "Int",
      alias(resultName, TypeShape.Identifier("Int")),
      Type.Name("Int")
    ),
    Fixture(
      "Option[Int]",
      alias(
        resultName,
        TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("Int")))
      ),
      Type.Apply(Type.Name("Option"), Type.ArgClause(List(Type.Name("Int"))))
    ),
    Fixture(
      "List[String]",
      alias(
        resultName,
        TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("String")))
      ),
      Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("String"))))
    ),
    Fixture(
      "(Int, String)",
      alias(
        resultName,
        TypeShape.Tuple(List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")))
      ),
      Type.Tuple(List(Type.Name("Int"), Type.Name("String")))
    ),
    Fixture(
      "(Int, String) => Boolean",
      alias(
        resultName,
        TypeShape.Function(
          List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
          TypeShape.Identifier("Boolean")
        )
      ),
      Type.Function(List(Type.Name("Int"), Type.Name("String")), Type.Name("Boolean"))
    ),
    Fixture(
      "Either[List[Int], Option[String]]",
      alias(
        resultName,
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
      ),
      Type.Apply(
        Type.Name("Either"),
        Type.ArgClause(
          List(
            Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("Int")))),
            Type.Apply(Type.Name("Option"), Type.ArgClause(List(Type.Name("String"))))
          )
        )
      )
    )
  )

  test("authors every accepted simple alias to the exact fresh Defn.Type topology"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)

      assertEquals(authored.productPrefix, "Defn.Type", clues(fixture.label))
      assertEquals(authored.mods, Nil, clues(fixture.label))
      assertEquals(authored.name.value, "Result", clues(fixture.label))
      assertEquals(authored.tparamClause.values, Nil, clues(fixture.label))
      assertEquals(authored.body.structure, fixture.expectedRhs.structure, clues(fixture.label))
      assertEmptyBounds(authored.bounds)
      assertEquals(authored.pos, Position.None, clues(fixture.label))
      assert(allTypeNodes(authored.body).forall(_.pos == Position.None), clues(fixture.label))
    }

  test("round-trips every accepted alias exactly through N024 and equivalently through N025"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val expected = Right(ProjectedDefinitionShape(fixture.shape, None))

      assertEquals(ScalametaSimpleTypeAliasProjection.project(authored), expected, clues(fixture.label))
      assertEquals(ScalametaDefinitionProjection.project(authored), expected, clues(fixture.label))
    }

  test("preserves a Core-admitted backticked keyword name exactly"):
    val keywordName = DefinitionName.backticked("`type`").toOption.get
    val shape = alias(keywordName, TypeShape.Identifier("Int"))
    val authored = author(shape)
    val projected = ScalametaSimpleTypeAliasProjection.project(authored).toOption.get

    assertEquals(authored.name.value, "type")
    assertEquals(authored.name.tokens.map(_.text).mkString, "`type`")
    assertEquals(projected.shape.name, keywordName)
    assertEquals(projected.shape.name.decoded, "type")
    assertEquals(projected.shape.name.source, "`type`")
    assertEquals(projected.shape.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(projected.sourceSpan, None)

  test("does not collapse semantically distinct right-hand sides"):
    val listShape = alias(
      resultName,
      TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("Int")))
    )
    val optionShape = alias(
      resultName,
      TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("Int")))
    )
    val authoredList = author(listShape)
    val authoredOption = author(optionShape)

    assertNotEquals(authoredList.body.structure, authoredOption.body.structure)
    assertEquals(ScalametaSimpleTypeAliasProjection.project(authoredList).toOption.get.shape, listShape)
    assertEquals(ScalametaSimpleTypeAliasProjection.project(authoredOption).toOption.get.shape, optionShape)

  test("rejects missing input with one stable bounded error"):
    assertEquals(
      ScalametaSimpleTypeAliasAuthoring.author(null),
      Left(
        ScalametaSimpleTypeAliasAuthoring.Error(
          "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_MISSING",
          "the simple type alias shape must be present."
        )
      )
    )

  test("keeps unsupported right-hand sides outside the authoring boundary through Core authority"):
    val unsupportedRhs = List[TypeShape](
      TypeShape.Unsupported("CompilerTypeTree", "outside the unresolved family"),
      TypeShape.Select(TypeShape.Identifier("scala"), "Int"),
      null
    )

    unsupportedRhs.foreach { rhs =>
      assertEquals(
        DefinitionShape.simpleTypeAlias(resultName, rhs),
        Left(DefinitionError.UnsupportedDefinitionType("type alias right-hand side"))
      )
    }

  test("fails closed when the semantic name cannot be authored and projected exactly"):
    val malformed = DefinitionShape
      .simpleTypeAlias(null, TypeShape.Identifier("Int"))
      .toOption
      .get

    assertEquals(
      ScalametaSimpleTypeAliasAuthoring.author(malformed),
      Left(
        ScalametaSimpleTypeAliasAuthoring.Error(
          "NEUTRAL_SIMPLE_TYPE_ALIAS_AUTHORING_NAME_UNSUPPORTED",
          "the alias name cannot be authored as a fresh Type.Name with exact Core spelling."
        )
      )
    )

  private def author(shape: DefinitionShape.SimpleTypeAlias): Defn.Type =
    ScalametaSimpleTypeAliasAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def alias(
      name: _root_.quasiquotes.definitions.DefinitionName,
      rhs: TypeShape
  ): DefinitionShape.SimpleTypeAlias =
    DefinitionShape.simpleTypeAlias(name, rhs) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def allTypeNodes(root: Type): List[Type] =
    root :: (root match
      case _: Type.Name => Nil
      case applied: Type.Apply =>
        allTypeNodes(applied.tpe) ++ applied.args.flatMap(allTypeNodes)
      case tuple: Type.Tuple => tuple.args.flatMap(allTypeNodes)
      case function: Type.Function =>
        function.params.flatMap(allTypeNodes) ++ allTypeNodes(function.res)
      case other => fail(s"unexpected authored node: ${other.productPrefix}")
    )

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)
