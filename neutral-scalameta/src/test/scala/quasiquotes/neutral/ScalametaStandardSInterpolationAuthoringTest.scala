package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.meta.*
import scala.meta.tokens.Token

final class ScalametaStandardSInterpolationAuthoringTest extends munit.FunSuite:
  private val freeX = TermShape.Identifier("x", isPlaceholder = false)

  test("authors plain zero-argument single-argument and multi-argument standard s shapes"):
    val fixtures = List(
      TermShape.InterpolatedString("s", List("plain"), Nil),
      TermShape.InterpolatedString("s", List("hello ", ""), List(freeX)),
      TermShape.InterpolatedString(
        "s",
        List("left=", ", right=", ""),
        List(freeX, TermShape.Literal("2"))
      )
    )

    fixtures.foreach { shape =>
      val authored = author(shape)
      assertRoundTrip(shape, authored)
      assertEquals(authored.prefix.value, "s")
      assertEquals(authored.prefix.tokens.map(_.text).mkString, "s")
      assertEquals(rootDelimiters(authored), List("\"", "\""))
      assertEquals(authored.parts.size, shape.parts.size)
      assertEquals(authored.args.size, shape.arguments.size)
      assert(allTrees(authored).forall(_.pos == Position.None))
    }

  test("encodes the complete N016 semantic-part matrix with exact N009R round trips"):
    val semanticParts = List(
      "",
      "plain",
      "$",
      "quote: \"",
      "slash: \\",
      "two chars: \\n",
      "line:\n",
      "return:\r",
      "tab:\t",
      "back:\b",
      "form:\f",
      "\\q",
      "\\u0001",
      "\u0001",
      "\u007f",
      "λ😀漢字",
      "before $ \" \\ \n \t λ after"
    )

    semanticParts.foreach { semantic =>
      val shape = TermShape.InterpolatedString("s", List(semantic), Nil)
      val authored = author(shape)
      assertRoundTrip(shape, authored)
      assertEquals(StringContext.processEscapes(partValue(authored.parts.head)), semantic)
      assertEquals(rootDelimiters(authored), List("\"", "\""))
    }

  test("preserves mixed escape-sensitive parts around ordered arguments"):
    val shape = TermShape.InterpolatedString(
      "s",
      List("before \\n\"", " middle \\ ", " after \t\u0001"),
      List(
        TermShape.Select(TermShape.Identifier("service", false), "value"),
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("true")))
      )
    )

    val authored = author(shape)
    assertRoundTrip(shape, authored)
    assertEquals(authored.args.map(_.productPrefix), List("Term.Select", "Term.Tuple"))
    assertEquals(authored.parts.map(part => StringContext.processEscapes(partValue(part))), shape.parts)

  test("recursively authors N013 N014 and N015 argument topologies"):
    val shape = TermShape.InterpolatedString(
      "s",
      List("apply=", ", new=", ", block=", ""),
      List(
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("service", false), "call"),
          List(TermShape.Unary("-", TermShape.Literal("1")))
        ),
        TermShape.New("synthetic.unresolved.Widget", List(TermShape.Literal("2"))),
        TermShape.Block(
          List(TermShape.Infix(TermShape.Literal("1"), "+", TermShape.Literal("2"))),
          TermShape.If(
            TermShape.Identifier("flag", false),
            TermShape.Literal("true"),
            TermShape.Literal("false")
          )
        )
      )
    )

    val authored = author(shape)
    assertRoundTrip(shape, authored)
    assertEquals(authored.args.map(_.productPrefix), List("Term.Apply", "Term.New", "Term.Block"))

  test("authors the previously blocked nested interpolation and two recursive levels"):
    val inner = TermShape.InterpolatedString(
      "s",
      List("inner=", ""),
      List(freeX)
    )
    val blockedN016 = TermShape.InterpolatedString(
      "s",
      List("outer=", ""),
      List(inner)
    )
    val twoLevels = TermShape.InterpolatedString(
      "s",
      List("top=", ""),
      List(blockedN016)
    )

    List(blockedN016, twoLevels).foreach { shape =>
      val authored = author(shape)
      assertRoundTrip(shape, authored)
      assertEquals(rootDelimiters(authored), List("\"", "\""))
      assert(allTrees(authored).forall(_.pos == Position.None))
    }

  test("preserves escape-sensitive semantics at multiple nested interpolation levels"):
    val inner = TermShape.InterpolatedString(
      "s",
      List("inner \\ \" \n=", ""),
      List(freeX)
    )
    val outer = TermShape.InterpolatedString(
      "s",
      List("outer \\q \t=", ""),
      List(inner)
    )

    val authored = author(outer)
    assertRoundTrip(outer, authored)
    val authoredInner = authored.args.head.asInstanceOf[Term.Interpolate]
    assertEquals(StringContext.processEscapes(partValue(authored.parts.head)), outer.parts.head)
    assertEquals(StringContext.processEscapes(partValue(authoredInner.parts.head)), inner.parts.head)

  test("authors nested interpolation inside an admitted Apply argument topology"):
    val nested = TermShape.InterpolatedString("s", List("nested=", ""), List(freeX))
    val shape = TermShape.InterpolatedString(
      "s",
      List("outer=", ""),
      List(
        TermShape.Apply(
          TermShape.Identifier("wrap", false),
          List(nested)
        )
      )
    )

    assertRoundTrip(shape, author(shape))

  test("fails closed for malformed prefix lists parts and arguments"):
    List(
      TermShape.InterpolatedString(null, List("plain"), Nil),
      TermShape.InterpolatedString("raw", List("plain"), Nil),
      TermShape.InterpolatedString("f", List("plain"), Nil),
      TermShape.InterpolatedString("custom", List("plain"), Nil),
      TermShape.InterpolatedString("s", List(null), Nil),
      TermShape.InterpolatedString("s", List("", ""), List(null))
    ).foreach(shape => assertErrorCode(shape, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"))

  test("Core cardinality construction prevents null interpolation lists before authoring"):
    intercept[NullPointerException](TermShape.InterpolatedString("s", null, Nil))
    intercept[NullPointerException](TermShape.InterpolatedString("s", List("plain"), null))

  test("retains existing excluded child-family diagnostics before construction"):
    val excluded = List(
      TermShape.BoundReference(BinderId(0), "x"),
      TermShape.Parenthesized(freeX),
      TermShape.Unsupported("Term.Match", "outside N019")
    )

    excluded.foreach { child =>
      assertErrorCode(
        TermShape.InterpolatedString("s", List("", ""), List(child)),
        "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
      )
    }

  private def author(shape: TermShape): Term.Interpolate =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value: Term.Interpolate) => value
      case Right(other) => fail(s"expected Term.Interpolate, found ${other.productPrefix}")
      case Left(problem) => fail(problem.message)

  private def assertRoundTrip(shape: TermShape, authored: Term.Interpolate): Unit =
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

  private def rootDelimiters(interpolation: Term.Interpolate): List[String] =
    val tokens = interpolation.tokens.toList
    val start = tokens.collectFirst { case token: Token.Interpolation.Start => token.text }
    val end = tokens.reverse.collectFirst { case token: Token.Interpolation.End => token.text }
    start.toList ++ end.toList

  private def partValue(part: Lit): String =
    part match
      case value: Lit.String => value.value
      case other => fail(s"expected Lit.String interpolation part, found ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
