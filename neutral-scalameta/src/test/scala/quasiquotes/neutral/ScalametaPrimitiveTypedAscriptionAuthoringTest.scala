package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

final class ScalametaPrimitiveTypedAscriptionAuthoringTest extends munit.FunSuite:
  private type DefinitionBinder = ScalametaTermShapeAuthoring.DefinitionBinder

  private val free = TermShape.Identifier("value", isPlaceholder = false)
  private val id7 = BinderId(7)
  private val xBinder = ScalametaTermShapeAuthoring.DefinitionBinder(
    id7,
    DefinitionName.plain("x").toOption.get
  )

  test("authors the bounded public primitive Typed matrix as fresh exact ascriptions"):
    val fixtures = List(
      TermShape.Typed(free, "Int"),
      TermShape.Typed(TermShape.Literal("\"x\""), "String"),
      TermShape.Typed(TermShape.Literal("true"), "Boolean"),
      TermShape.Typed(
        TermShape.Apply(
          TermShape.Identifier("f", false),
          List(TermShape.Literal("1"))
        ),
        "Int"
      ),
      TermShape.Typed(
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"))),
        "String"
      ),
      TermShape.Typed(
        TermShape.InterpolatedString(
          "s",
          List("value=", ""),
          List(TermShape.Identifier("part", false))
        ),
        "String"
      )
    )

    fixtures.foreach { shape =>
      val authored = author(shape)
      val ascription = authored.asInstanceOf[Term.Ascribe]

      assertEquals(authored.productPrefix, "Term.Ascribe", clues(shape))
      assertEquals(
        ascription.tpe.asInstanceOf[Type.Name].value,
        shape.typeName,
        clues(shape)
      )
      assert(allTrees(ascription).forall(_.pos == Position.None), clues(shape))
      assertEquals(
        ScalametaTermProjection.project(ascription),
        Right(ProjectedTermShape(shape, None)),
        clues(shape)
      )
    }

  test("rejects every non-primitive Typed type text with one stable bounded failure"):
    val unsupported = List(
      "Long",
      "AnyVal",
      "Option[Int]",
      "Either[Int, String]",
      "(Int, String)",
      "Int => String",
      "scala.Int",
      "java.lang.String",
      "",
      null
    )
    val expected = Left(
      ScalametaTermShapeAuthoring.Error(
        "NEUTRAL_TERM_AUTHORING_TYPED_TYPE_UNSUPPORTED",
        "typed/ascribed authoring admits only canonical Int, String, and Boolean."
      )
    )

    unsupported.foreach(typeName =>
      assertEquals(
        ScalametaTermShapeAuthoring.author(TermShape.Typed(free, typeName)),
        expected,
        clues(typeName)
      )
    )

  test("preserves recursive child failures instead of relabelling them as Type failures"):
    val failures = List(
      TermShape.Typed(null, "Int") -> "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
      TermShape.Typed(TermShape.Parenthesized(free), "Int") ->
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
      TermShape.Typed(TermShape.Literal("01"), "Int") ->
        "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED",
      TermShape.Typed(TermShape.Identifier("bad-name", false), "Int") ->
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
    )

    failures.foreach { (shape, expectedCode) =>
      assertErrorCode(shape, expectedCode)
    }

  test("does not generalize the Parenthesized or public BoundReference fallbacks"):
    val expected = Left(
      ScalametaTermShapeAuthoring.Error(
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
        "this TermShape family is outside binder-free N013-N015/N019 authoring."
      )
    )

    List(
      TermShape.Parenthesized(free),
      TermShape.BoundReference(BinderId(0), "x")
    ).foreach(shape => assertEquals(ScalametaTermShapeAuthoring.author(shape), expected))

  test("threads the external Definition binder scope through a Typed expression"):
    val input = TermShape.Typed(TermShape.BoundReference(id7, "stale"), "Int")
    val authored = authorSeeded(input, Vector(xBinder)).asInstanceOf[Term.Ascribe]

    assertEquals(authored.expr.asInstanceOf[Term.Name].value, "x")
    assertEquals(authored.tpe.asInstanceOf[Type.Name].value, "Int")
    assert(allTrees(authored).forall(_.pos == Position.None))
    assertSeededRoundTrip(
      input,
      authored,
      Vector(xBinder),
      TermShape.Typed(TermShape.BoundReference(id7, "x"), "Int")
    )

  test("keeps seeded capture checks and selected-member non-binding semantics authoritative"):
    val captures = List(
      TermShape.Typed(TermShape.Identifier("x", false), "Int"),
      TermShape.Typed(
        TermShape.Tuple(
          List(TermShape.Literal("1"), TermShape.Identifier("x", false))
        ),
        "Int"
      )
    )

    captures.foreach(shape =>
      assertSeededErrorCode(
        shape,
        Vector(xBinder),
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
      )
    )

    val selected = TermShape.Typed(
      TermShape.Select(TermShape.Identifier("service", false), "x"),
      "Int"
    )
    val authored = authorSeeded(selected, Vector(xBinder))
    assertSeededRoundTrip(selected, authored, Vector(xBinder), selected)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def authorSeeded(
      shape: TermShape,
      binders: Vector[DefinitionBinder]
  ): Term =
    ScalametaTermShapeAuthoring.authorWithDefinitionBinders(shape, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertErrorCode(shape: TermShape, expectedCode: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expectedCode),
      clues(shape)
    )

  private def assertSeededErrorCode(
      shape: TermShape,
      binders: Vector[DefinitionBinder],
      expectedCode: String
  ): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring
        .authorWithDefinitionBinders(shape, binders)
        .left
        .toOption
        .map(_.code),
      Some(expectedCode),
      clues(shape)
    )

  private def assertSeededRoundTrip(
      expected: TermShape,
      authored: Term,
      binders: Vector[DefinitionBinder],
      exactProjected: TermShape
  ): Unit =
    val seeds = binders.map(binder =>
      ScalametaTermProjection.DefinitionBinder(binder.name.decoded, binder.binderId)
    )
    val projected = ScalametaTermProjection
      .projectWithDefinitionBinders(authored, seeds)
      .toOption
      .get
    val binderIds = binders.map(_.binderId)

    assertEquals(projected.shape, exactProjected)
    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(projected.shape, binderIds),
      TermShapeTraversal.alphaNormalizeInScope(expected, binderIds)
    )
    assertEquals(projected.sourceSpan, None)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
