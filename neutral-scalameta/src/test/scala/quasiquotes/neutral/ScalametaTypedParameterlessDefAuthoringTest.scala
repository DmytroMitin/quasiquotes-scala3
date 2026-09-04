package quasiquotes.neutral

import _root_.quasiquotes.definitions.{
  DefinitionError,
  DefinitionName,
  DefinitionNameSpelling,
  DefinitionShape
}
import _root_.quasiquotes.parser.{TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedParameterlessDefAuthoringTest extends munit.FunSuite:
  private final case class Fixture(label: String, shape: DefinitionShape.ParameterlessDef)

  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  private val fixtures = List(
    Fixture("integer literal", method("answer", intType, TermShape.Literal("42"))),
    Fixture("boolean literal", method("flag", booleanType, TermShape.Literal("true"))),
    Fixture("string literal", method("source", stringType, TermShape.Literal("\"value\""))),
    Fixture("identifier", method("renamed", intType, TermShape.Identifier("other", false))),
    Fixture(
      "selection and application",
      method(
        "computed",
        intType,
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("service", false), "compute"),
          List(TermShape.Identifier("source", false))
        )
      )
    ),
    Fixture(
      "infix",
      method(
        "sum",
        intType,
        TermShape.Infix(
          TermShape.Identifier("left", false),
          "+",
          TermShape.Identifier("right", false)
        )
      )
    ),
    Fixture(
      "unary",
      method("negated", intType, TermShape.Unary("-", TermShape.Identifier("value", false)))
    ),
    Fixture(
      "tuple",
      method(
        "pair",
        TypeShape.Tuple(List(intType, stringType)),
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("\"x\"")))
      )
    ),
    Fixture(
      "if",
      method(
        "choose",
        intType,
        TermShape.If(
          TermShape.Identifier("cond", false),
          TermShape.Literal("1"),
          TermShape.Literal("2")
        )
      )
    ),
    Fixture(
      "standard s interpolation",
      method(
        "message",
        stringType,
        TermShape.InterpolatedString(
          "s",
          List("value=\"", "\"\\n"),
          List(TermShape.Identifier("source", false))
        )
      )
    ),
    Fixture(
      "recursive result Type",
      method(
        "nested",
        TypeShape.Apply(
          TypeShape.Identifier("Either"),
          List(
            TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
            TypeShape.Apply(TypeShape.Identifier("Option"), List(stringType))
          )
        ),
        TermShape.Identifier("value", false)
      )
    ),
    Fixture(
      "backticked keyword name",
      method(
        DefinitionName.backticked("`type`").toOption.get,
        intType,
        TermShape.Literal("42")
      )
    ),
    Fixture(
      "selected member named like method",
      method(
        "selectedAnswer",
        intType,
        TermShape.Select(TermShape.Identifier("service", false), "selectedAnswer")
      )
    )
  )

  test("authors every honest-intersection row to exact fresh true-parameterless topology"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)

      assertEquals(authored.productPrefix, "Defn.Def", clues(fixture.label))
      assertEquals(authored.mods, Nil, clues(fixture.label))
      assertEquals(authored.paramClauseGroups, Nil, clues(fixture.label))
      assert(authored.decltpe.nonEmpty, clues(fixture.label))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(fixture.label))
    }

  test("round-trips every accepted row exactly through N021 and equivalently through N025"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val expected = Right(ProjectedDefinitionShape(fixture.shape, None))

      assertEquals(
        ScalametaTypedParameterlessDefProjection.project(authored),
        expected,
        clues(fixture.label)
      )
      assertEquals(ScalametaDefinitionProjection.project(authored), expected, clues(fixture.label))
    }

  test("preserves an admitted backticked keyword method name exactly"):
    val shape = fixtures.find(_.label == "backticked keyword name").get.shape
    val authored = author(shape)
    val projected = ScalametaTypedParameterlessDefProjection.project(authored).toOption.get

    assertEquals(authored.name.value, "type")
    assertEquals(authored.name.tokens.map(_.text).mkString, "`type`")
    assertEquals(projected.shape.name.decoded, "type")
    assertEquals(projected.shape.name.source, "`type`")
    assertEquals(projected.shape.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(projected.sourceSpan, None)

  test("keeps a matching selected-member spelling distinct from free self-reference"):
    val shape = fixtures.find(_.label == "selected member named like method").get.shape

    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(author(shape)),
      Right(ProjectedDefinitionShape(shape, None))
    )

  test("rejects a missing input with one stable bounded error"):
    assertEquals(
      ScalametaTypedParameterlessDefAuthoring.author(null),
      Left(
        ScalametaTypedParameterlessDefAuthoring.Error(
          "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_MISSING",
          "the parameterless def shape must be present."
        )
      )
    )

  test("fails closed when the semantic method name cannot be authored exactly"):
    val malformed = DefinitionShape
      .parameterlessDef(null, intType, TermShape.Literal("42"))
      .toOption
      .get

    assertErrorCode(malformed, "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_NAME_UNSUPPORTED")

  test("keeps unsupported and null result Types and bodies outside the Core boundary"):
    val name = DefinitionName.plain("answer").toOption.get
    val literal = TermShape.Literal("42")
    val constructor = TermShape.New("java.lang.StringBuilder", Nil)

    assert(
      DefinitionShape
        .parameterlessDef(name, TypeShape.Unsupported("Type.Match", "outside N011"), literal)
        .isLeft
    )
    assert(DefinitionShape.parameterlessDef(name, null, literal).isLeft)
    assertEquals(
      DefinitionShape.parameterlessDef(name, intType, null),
      Left(
        DefinitionError.UnsupportedDefinitionBody(
          "method body",
          "definition bodies require a present TermShape"
        )
      )
    )
    assert(ScalametaTermShapeAuthoring.author(constructor).isRight)
    assert(DefinitionShape.parameterlessDef(name, intType, constructor).isLeft)

  test("reports Core-admitted but generic-Term-authoring-rejected bodies as intersection failures"):
    val rejected = List(
      method(
        "typed",
        intType,
        TermShape.Typed(TermShape.Identifier("source", false), "Int")
      ),
      method(
        "parenthesized",
        intType,
        TermShape.Parenthesized(TermShape.Identifier("source", false))
      )
    )

    rejected.foreach(assertErrorCode(_, "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("maps generic Term semantic-value rejection to the bounded method-Term category"):
    val rejected = List(
      method("nonCanonical", intType, TermShape.Literal("01")),
      method("malformedName", intType, TermShape.Identifier("bad-name", false))
    )

    rejected.foreach(assertErrorCode(_, "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_TERM_UNSUPPORTED"))

  test("fails closed through N021 for an unqualified free self-reference"):
    val recursive = method("answer", intType, TermShape.Identifier("answer", false))

    assert(ScalametaTermShapeAuthoring.author(recursive.body).isRight)
    assertErrorCode(recursive, "NEUTRAL_PARAMETERLESS_DEF_AUTHORING_ROUNDTRIP_FAILED")

  private def author(shape: DefinitionShape.ParameterlessDef): Defn.Def =
    ScalametaTypedParameterlessDefAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def method(
      name: String,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.ParameterlessDef =
    method(DefinitionName.plain(name).toOption.get, resultType, body)

  private def method(
      name: DefinitionName,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.ParameterlessDef =
    DefinitionShape.parameterlessDef(name, resultType, body) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(
      shape: DefinitionShape.ParameterlessDef,
      expected: String
  ): Unit =
    assertEquals(
      ScalametaTypedParameterlessDefAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
