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
final class ScalametaTypedImmutableValAuthoringTest extends munit.FunSuite:
  private final case class Fixture(label: String, shape: DefinitionShape.ImmutableVal)

  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  private val fixtures = List(
    Fixture("integer literal", immutable("answer", intType, TermShape.Literal("42"))),
    Fixture("boolean literal", immutable("flag", booleanType, TermShape.Literal("true"))),
    Fixture("string literal", immutable("source", stringType, TermShape.Literal("\"value\""))),
    Fixture("identifier", immutable("renamed", intType, TermShape.Identifier("other", false))),
    Fixture(
      "selection and application",
      immutable(
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
      immutable(
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
      immutable("negated", intType, TermShape.Unary("-", TermShape.Identifier("value", false)))
    ),
    Fixture(
      "tuple",
      immutable(
        "pair",
        TypeShape.Tuple(List(intType, stringType)),
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("\"x\"")))
      )
    ),
    Fixture(
      "if",
      immutable(
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
      immutable(
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
      "recursive declared Type",
      immutable(
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
      immutable(
        DefinitionName.backticked("`type`").toOption.get,
        intType,
        TermShape.Literal("42")
      )
    )
  )

  test("authors every honest-intersection row to exact fresh immutable-val topology"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)

      assertEquals(authored.productPrefix, "Defn.Val", clues(fixture.label))
      assertEquals(authored.mods, Nil, clues(fixture.label))
      assertEquals(authored.pats.size, 1, clues(fixture.label))
      assert(authored.pats.head.isInstanceOf[Pat.Var], clues(fixture.label))
      assert(authored.decltpe.nonEmpty, clues(fixture.label))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(fixture.label))
    }

  test("round-trips every accepted row exactly through N020 and equivalently through N025"):
    fixtures.foreach { fixture =>
      val authored = author(fixture.shape)
      val expected = Right(ProjectedDefinitionShape(fixture.shape, None))

      assertEquals(
        ScalametaTypedImmutableValProjection.project(authored),
        expected,
        clues(fixture.label)
      )
      assertEquals(ScalametaDefinitionProjection.project(authored), expected, clues(fixture.label))
    }

  test("preserves an admitted backticked keyword name exactly"):
    val shape = fixtures.find(_.label == "backticked keyword name").get.shape
    val authored = author(shape)
    val pattern = authored.pats.head.asInstanceOf[Pat.Var]
    val projected = ScalametaTypedImmutableValProjection.project(authored).toOption.get

    assertEquals(pattern.name.value, "type")
    assertEquals(pattern.name.tokens.map(_.text).mkString, "`type`")
    assertEquals(projected.shape.name.decoded, "type")
    assertEquals(projected.shape.name.source, "`type`")
    assertEquals(projected.shape.name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(projected.sourceSpan, None)

  test("rejects a missing input with one stable bounded error"):
    assertEquals(
      ScalametaTypedImmutableValAuthoring.author(null),
      Left(
        ScalametaTypedImmutableValAuthoring.Error(
          "NEUTRAL_TYPED_VAL_AUTHORING_MISSING",
          "the immutable val shape must be present."
        )
      )
    )

  test("fails closed when the semantic declaration name cannot be authored exactly"):
    val malformed = DefinitionShape
      .immutableVal(null, intType, TermShape.Literal("42"))
      .toOption
      .get

    assertErrorCode(malformed, "NEUTRAL_TYPED_VAL_AUTHORING_NAME_UNSUPPORTED")

  test("keeps unsupported and null declared Types and RHS families outside the Core boundary"):
    val name = DefinitionName.plain("answer").toOption.get
    val literal = TermShape.Literal("42")
    val constructor = TermShape.New("java.lang.StringBuilder", Nil)

    assert(
      DefinitionShape
        .immutableVal(name, TypeShape.Unsupported("Type.Match", "outside N011"), literal)
        .isLeft
    )
    assert(DefinitionShape.immutableVal(name, null, literal).isLeft)
    assertEquals(
      DefinitionShape.immutableVal(name, intType, null),
      Left(
        DefinitionError.UnsupportedDefinitionBody(
          "value right-hand side",
          "definition bodies require a present TermShape"
        )
      )
    )
    assert(ScalametaTermShapeAuthoring.author(constructor).isRight)
    assert(
      DefinitionShape
        .immutableVal(name, intType, constructor)
        .isLeft
    )
    assert(
      DefinitionShape
        .immutableVal(
          name,
          intType,
          TermShape.Block(List(TermShape.Literal("1")), TermShape.Literal("2"))
        )
        .isLeft
    )

  test("reports remaining Core-admitted but generic-Term-authoring-rejected bodies as intersection failures"):
    val rejected = List(
      immutable(
        "parenthesized",
        intType,
        TermShape.Parenthesized(TermShape.Identifier("source", false))
      )
    )

    rejected.foreach(assertErrorCode(_, "NEUTRAL_TYPED_VAL_AUTHORING_TERM_UNSUPPORTED"))

  test("maps generic Term semantic-value rejection to the bounded val-Term category"):
    val rejected = List(
      immutable("nonCanonical", intType, TermShape.Literal("01")),
      immutable("malformedName", intType, TermShape.Identifier("bad-name", false))
    )

    rejected.foreach(assertErrorCode(_, "NEUTRAL_TYPED_VAL_AUTHORING_TERM_UNSUPPORTED"))

  private def author(shape: DefinitionShape.ImmutableVal): Defn.Val =
    ScalametaTypedImmutableValAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def immutable(
      name: String,
      declaredType: TypeShape,
      rhs: TermShape
  ): DefinitionShape.ImmutableVal =
    immutable(DefinitionName.plain(name).toOption.get, declaredType, rhs)

  private def immutable(
      name: DefinitionName,
      declaredType: TypeShape,
      rhs: TermShape
  ): DefinitionShape.ImmutableVal =
    DefinitionShape.immutableVal(name, declaredType, rhs) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(
      shape: DefinitionShape.ImmutableVal,
      expected: String
  ): Unit =
    assertEquals(
      ScalametaTypedImmutableValAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
