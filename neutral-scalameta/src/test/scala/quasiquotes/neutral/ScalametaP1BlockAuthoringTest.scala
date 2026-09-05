package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}

import scala.meta.*

final class ScalametaP1BlockAuthoringTest extends munit.FunSuite:
  private val result = TermShape.Identifier("result", isPlaceholder = false)

  test("authors one-prefix and multiple-prefix P1 Blocks in exact order"):
    val onePrefix = block(
      List(TermShape.Apply(ident("first"), Nil)),
      result
    )
    val multiplePrefixes = block(
      List(
        TermShape.Apply(ident("first"), Nil),
        TermShape.Apply(ident("second"), List(TermShape.Literal("2")))
      ),
      result
    )

    List(onePrefix, multiplePrefixes).foreach(shape => assertRoundTrip(shape, author(shape)))

    val authored = author(multiplePrefixes).asInstanceOf[Term.Block]
    assertEquals(
      authored.stats.map(_.productPrefix),
      List("Term.Apply", "Term.Apply", "Term.Name")
    )
    assertEquals(authored.stats.last.asInstanceOf[Term.Name].value, "result")

  test("authors tuple If literal and N014 New children inside P1"):
    val shape = block(
      List(
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("true"))),
        TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("16")))
      ),
      TermShape.If(
        ident("flag"),
        TermShape.Literal("\"yes\""),
        TermShape.Literal("\"no\"")
      )
    )

    val authored = author(shape).asInstanceOf[Term.Block]
    assertRoundTrip(shape, authored)
    assertEquals(
      authored.stats.map(_.productPrefix),
      List("Term.Tuple", "Term.New", "Term.If")
    )

  test("recursively authors P1 Blocks in prefix and result positions without provenance"):
    val shape = block(
      List(
        block(List(TermShape.Literal("1")), TermShape.Literal("2")),
        TermShape.Unary("!", ident("disabled"))
      ),
      block(
        List(TermShape.Apply(ident("finish"), Nil)),
        TermShape.Literal("0")
      )
    )

    val authored = author(shape).asInstanceOf[Term.Block]
    assertRoundTrip(shape, authored)
    assertEquals(
      authored.stats.map(_.productPrefix),
      List("Term.Block", "Term.ApplyUnary", "Term.Block")
    )
    assert(allTrees(authored).forall(_.pos == Position.None))

  test("keeps P0 semantic authoring transparent instead of inventing braces"):
    val authored = author(result)

    assert(authored.isInstanceOf[Term.Name])
    assert(!authored.isInstanceOf[Term.Block])
    assertRoundTrip(result, authored)

  test("preserves prefix order and keeps the final result structurally distinct"):
    val original = block(
      List(TermShape.Literal("1"), TermShape.Literal("2")),
      TermShape.Literal("3")
    )
    val swapped = block(
      List(TermShape.Literal("2"), TermShape.Literal("1")),
      TermShape.Literal("3")
    )
    val movedResult = block(
      List(TermShape.Literal("1"), TermShape.Literal("3")),
      TermShape.Literal("2")
    )

    assertNotEquals(original, swapped)
    assertNotEquals(original, movedResult)
    List(original, swapped, movedResult).foreach(shape => assertRoundTrip(shape, author(shape)))

  test("recursively authors N019 interpolation in P1 prefix and result positions"):
    val interpolation = TermShape.InterpolatedString(
      "s",
      List("value=", ""),
      List(TermShape.Identifier("x", false))
    )
    val shape = block(
      List(interpolation),
      TermShape.InterpolatedString("s", List("done"), Nil)
    )

    val authored = author(shape).asInstanceOf[Term.Block]
    assertRoundTrip(shape, authored)
    assert(authored.stats.forall(_.isInstanceOf[Term.Interpolate]))

  test("rejects LocalVal and LocalDef prefixes through the family boundary"):
    val localVal = BlockStatement.LocalVal(
      BinderId(0),
      "value",
      "Int",
      TermShape.Literal("1")
    )
    val localDef = BlockStatement.LocalDef(
      BinderId(1),
      "identity",
      BinderId(2),
      "value",
      TypeShape.Identifier("Int"),
      TypeShape.Identifier("Int"),
      TermShape.BoundReference(BinderId(2), "value")
    )

    List(localVal, localDef).foreach(statement =>
      assertErrorCode(
        TermShape.Block(List(statement), result),
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
      )
    )

  test("rejects binder-bearing and independently excluded prefix or result children"):
    val lambda = TermShape.Lambda1(
      BinderId(3),
      "x",
      "Int",
      TermShape.BoundReference(BinderId(3), "x")
    )
    val excluded = List[TermShape](
      lambda,
      TermShape.BoundReference(BinderId(4), "x")
    )

    excluded.foreach(child =>
      assertErrorCode(
        block(List(child), result),
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
      )
      assertErrorCode(
        block(List(TermShape.Literal("1")), child),
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
      )
    )

  test("rejects null and recursively malformed children without throwing"):
    List(
      block(List(null), result),
      block(List(TermShape.Literal("1")), null)
    ).foreach(shape =>
      assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")
    )

    assertErrorCode(
      block(
        List(TermShape.New("StringBuilder", Nil)),
        result
      ),
      "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"
    )

  private def block(statements: List[BlockStatement], result: TermShape): TermShape =
    TermShape.Block(statements, result)

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertRoundTrip(shape: TermShape, authored: Term): Unit =
    assertEquals(
      ScalametaTermProjection.project(authored),
      Right(ProjectedTermShape(shape, None))
    )

  private def assertErrorCode(shape: TermShape, expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
