package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.meta.*

final class ScalametaConstructorNewAuthoringTest extends munit.FunSuite:
  private val free = TermShape.Identifier("x", isPlaceholder = false)

  test("authors fully-qualified constructors with one ordinary positional argument clause"):
    val fixtures = List(
      TermShape.New("java.lang.StringBuilder", Nil),
      TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("16"))),
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(free, TermShape.Literal("1"))
      )
    )

    fixtures.foreach(shape => assertRoundTrip(shape, author(shape)))

    val empty = author(fixtures.head).asInstanceOf[Term.New]
    assertEquals(empty.init.name.value, "")
    assertEquals(constructorSegments(empty.init.tpe), List("java", "lang", "StringBuilder"))
    assertEquals(empty.init.argClauses.size, 1)
    assertEquals(empty.init.argClauses.head.mod, None)
    assertEquals(empty.init.argClauses.head.values, Nil)

    val ordered = author(fixtures.last).asInstanceOf[Term.New]
    assertEquals(constructorSegments(ordered.init.tpe), List("synthetic", "unresolved", "Widget"))
    assertEquals(
      ordered.init.argClauses.head.values.map(_.productPrefix).toList,
      List("Term.Name", "Lit.Int")
    )

  test("recursively authors the N013 family and nested New arguments"):
    val shape = TermShape.New(
      "synthetic.unresolved.Widget",
      List(
        TermShape.New("other.missing.Value", List(TermShape.Literal("1"))),
        TermShape.If(
          TermShape.Identifier("cond", false),
          TermShape.Apply(
            TermShape.Identifier("foo", false),
            List(TermShape.Identifier("x", false))
          ),
          TermShape.Tuple(List(TermShape.Literal("0"), TermShape.Literal("1")))
        )
      )
    )

    val authored = author(shape).asInstanceOf[Term.New]
    assertRoundTrip(shape, authored)
    assertEquals(authored.init.argClauses.head.values.map(_.productPrefix).toList, List("Term.New", "Term.If"))
    assert(allTrees(authored).forall(_.pos == Position.None))
    authored.init.argClauses.foreach(clause => assertEquals(clause.pos, Position.None))

  test("recursively authors N019 interpolation inside a New argument"):
    val interpolation = TermShape.InterpolatedString(
      "s",
      List("value=", ""),
      List(free)
    )
    val shape = TermShape.New("synthetic.unresolved.Widget", List(interpolation))

    val authored = author(shape).asInstanceOf[Term.New]
    assertRoundTrip(shape, authored)
    assert(authored.init.argClauses.head.values.head.isInstanceOf[Term.Interpolate])

  test("rejects every malformed constructor spelling through one bounded category"):
    List(
      null,
      "",
      "StringBuilder",
      ".java.lang.StringBuilder",
      "java.lang.StringBuilder.",
      "java..lang.StringBuilder",
      "java.lang.StringBuilder[Int]",
      "java.lang.`StringBuilder`",
      "java.lang.Outer$Inner"
    ).foreach(constructor =>
      assertErrorCode(
        TermShape.New(constructor, Nil),
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"
      )
    )

  test("rejects missing argument lists and missing arguments without throwing"):
    List(
      TermShape.New("java.lang.StringBuilder", null),
      TermShape.New("java.lang.StringBuilder", List(null)),
      TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("1"), null))
    ).foreach(shape =>
      assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")
    )

  test("retains child-family failures and rejects a malformed nested constructor"):
    List(
      TermShape.Typed(free, "Int"),
      TermShape.Parenthesized(free)
    ).foreach(child =>
      assertErrorCode(
        TermShape.New("synthetic.unresolved.Widget", List(child)),
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
      )
    )

    assertErrorCode(
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(TermShape.New("Value", Nil))
      ),
      "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"
    )

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

  private def constructorSegments(tpe: Type): List[String] =
    tpe match
      case name: Type.Name => name.value :: Nil
      case select: Type.Select => qualifierSegments(select.qual) :+ select.name.value
      case other => fail(s"expected Type.Name/Type.Select constructor path, got ${other.productPrefix}")

  private def qualifierSegments(term: Term): List[String] =
    term match
      case name: Term.Name => name.value :: Nil
      case select: Term.Select => qualifierSegments(select.qual) :+ select.name.value
      case other => fail(s"expected Term.Name/Term.Select qualifier path, got ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
