package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.meta.*

final class ScalametaTermShapeAuthoringTest extends munit.FunSuite:
  private val free = TermShape.Identifier("value", isPlaceholder = false)

  test("authors the exact literal protocol without parsing string interiors"):
    val fixtures = List(
      TermShape.Literal("0") -> Lit.Int(0),
      TermShape.Literal("17") -> Lit.Int(17),
      TermShape.Literal("-17") -> Lit.Int(-17),
      TermShape.Literal("true") -> Lit.Boolean(true),
      TermShape.Literal("false") -> Lit.Boolean(false),
      TermShape.Literal("\"\"") -> Lit.String(""),
      TermShape.Literal("\"text\"") -> Lit.String("text"),
      TermShape.Literal("\"a\\nb\"") -> Lit.String("a\\nb"),
      TermShape.Literal("\"a\"b\\c\nb\"") -> Lit.String("a\"b\\c\nb")
    )

    fixtures.foreach { (shape, expected) =>
      val authored = author(shape)
      assertEquals(authored.productPrefix, expected.productPrefix, clues(shape))
      (authored, expected) match
        case (actual: Lit.Int, wanted: Lit.Int) => assertEquals(actual.value, wanted.value)
        case (actual: Lit.Boolean, wanted: Lit.Boolean) => assertEquals(actual.value, wanted.value)
        case (actual: Lit.String, wanted: Lit.String) => assertEquals(actual.value, wanted.value)
        case _ => fail(s"unexpected literal pairing for $shape")
      assertRoundTrip(shape, authored)
    }

  test("authors names selections and one ordinary Apply list recursively"):
    val shapes = List(
      free,
      TermShape.Select(free, "field"),
      TermShape.Select(TermShape.Select(free, "inner"), "field"),
      TermShape.Apply(free, Nil),
      TermShape.Apply(free, List(TermShape.Literal("1"))),
      TermShape.Apply(
        TermShape.Select(free, "call"),
        List(TermShape.Literal("1"), TermShape.Literal("true"), TermShape.Literal("\"three\""))
      )
    )

    shapes.foreach(shape => assertRoundTrip(shape, author(shape)))

    val application = author(shapes.last).asInstanceOf[Term.Apply]
    assertEquals(application.argClause.mod, None)
    assertEquals(application.argClause.values.map(_.productPrefix), List("Lit.Int", "Lit.Boolean", "Lit.String"))

  test("authors infix unary tuple and explicit If topology in child order"):
    val infix = TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2"))
    val unaries = List("+", "-", "!", "~").map(operator => TermShape.Unary(operator, free))
    val tuples = List(
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"))),
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"), TermShape.Literal("3"))),
      TermShape.Tuple((1 to 22).toList.map(value => TermShape.Literal(value.toString)))
    )
    val conditional = TermShape.If(
      TermShape.Literal("true"),
      TermShape.Literal("1"),
      TermShape.Literal("2")
    )

    (infix :: unaries ::: tuples ::: List(conditional)).foreach { shape =>
      assertRoundTrip(shape, author(shape))
    }

    val authoredInfix = author(infix).asInstanceOf[Term.ApplyInfix]
    assertEquals(authoredInfix.targClause.values, Nil)
    assertEquals(authoredInfix.argClause.mod, None)
    assertEquals(authoredInfix.argClause.values.size, 1)

  test("authors a nested mixed binder-free composition and recursively drops provenance"):
    val shape = TermShape.If(
      TermShape.Unary("!", TermShape.Identifier("disabled", false)),
      TermShape.Apply(
        TermShape.Select(TermShape.Identifier("service", false), "call"),
        List(
          TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2")),
          TermShape.Tuple(List(TermShape.Literal("true"), TermShape.Literal("\"x\\ny\"")))
        )
      ),
      TermShape.Select(TermShape.Identifier("fallback", false), "value")
    )

    val authored = author(shape)
    assertRoundTrip(shape, authored)
    assert(allTrees(authored).forall(_.pos == Position.None))

    val application = authored.asInstanceOf[Term.If].thenp.asInstanceOf[Term.Apply]
    val infix = application.argClause.values.head.asInstanceOf[Term.ApplyInfix]
    assertEquals(application.argClause.pos, Position.None)
    assertEquals(infix.targClause.pos, Position.None)
    assertEquals(infix.argClause.pos, Position.None)

  test("rejects missing and malformed literal values with stable bounded categories"):
    assertErrorCode(null, "NEUTRAL_TERM_AUTHORING_MISSING")

    List(
      TermShape.Literal(null),
      TermShape.Literal("arbitrary"),
      TermShape.Literal("\"unterminated"),
      TermShape.Literal("01"),
      TermShape.Literal("+1"),
      TermShape.Literal("-0"),
      TermShape.Literal("2147483648"),
      TermShape.Literal("-2147483649")
    ).foreach(shape => assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_LITERAL_UNSUPPORTED"))

  test("rejects malformed identifier selection Apply and Infix structures without throwing"):
    val projectorRejected = List(
      TermShape.Identifier("_", false),
      TermShape.Identifier("if", false),
      TermShape.Identifier("bad-name", false),
      TermShape.Select(free, "if"),
      TermShape.Select(free, "bad-name")
    )
    projectorRejected.foreach(shape =>
      assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED")
    )

    val structurallyRejected = List(
      TermShape.Identifier(null, false),
      TermShape.Identifier("value", true),
      TermShape.Select(free, null),
      TermShape.Select(null, "field"),
      TermShape.Apply(free, null),
      TermShape.Apply(free, List(null)),
      TermShape.Apply(null, Nil),
      TermShape.Apply(TermShape.Apply(free, Nil), Nil),
      TermShape.Infix(null, "+", free),
      TermShape.Infix(free, null, free),
      TermShape.Infix(free, "+", null)
    )

    structurallyRejected.foreach(shape =>
      assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")
    )

  test("rejects invalid unary and tuple and If structures without throwing"):
    val rejected = List(
      TermShape.Unary("custom", free),
      TermShape.Unary(null, free),
      TermShape.Unary("!", null),
      TermShape.Tuple(null),
      TermShape.Tuple(Nil),
      TermShape.Tuple(List(TermShape.Literal("1"))),
      TermShape.Tuple((1 to 23).toList.map(value => TermShape.Literal(value.toString))),
      TermShape.Tuple(List(TermShape.Literal("1"), null)),
      TermShape.If(null, free, free),
      TermShape.If(free, null, free),
      TermShape.If(free, free, null)
    )

    rejected.foreach(shape => assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"))

  test("fails closed for every explicitly excluded TermShape family"):
    val excluded = List(
      TermShape.BoundReference(BinderId(0), "x"),
      TermShape.Parenthesized(free),
      TermShape.Unsupported("Term.Match", "outside N013")
    )

    excluded.foreach(shape => assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"))

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertRoundTrip(shape: TermShape, authored: Term): Unit =
    ScalametaTermProjection.project(authored) match
      case Right(projected) =>
        assertEquals(projected.shape, shape)
        assertEquals(projected.sourceSpan, None)
      case Left(error) => fail(error.message)

  private def assertErrorCode(shape: TermShape, expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
