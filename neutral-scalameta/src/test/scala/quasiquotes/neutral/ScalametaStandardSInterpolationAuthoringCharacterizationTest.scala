package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.tokens.Token

@nowarn("cat=deprecation")
final class ScalametaStandardSInterpolationAuthoringCharacterizationTest
    extends munit.FunSuite:
  private val semanticParts = List(
    "empty" -> "",
    "plain" -> "plain",
    "dollar" -> "$",
    "quote" -> "quote: \"",
    "slash" -> "slash: \\",
    "two-character newline" -> "two chars: \\n",
    "newline" -> "line:\n",
    "carriage return" -> "return:\r",
    "tab" -> "tab:\t",
    "backspace" -> "back:\b",
    "form feed" -> "form:\f",
    "unknown-looking escape" -> "\\q",
    "literal unicode escape" -> "\\u0001",
    "control" -> "\u0001",
    "unicode" -> "λ😀漢字",
    "mixed" -> "before $ \" \\ \n \t λ after"
  )

  test("direct Term.Interpolate has an exact s prefix ordered parts and ordered arguments"):
    val first = Term.Name("x")
    val second = Term.Apply(Term.Name("f"), Term.ArgClause(List(Lit.Int(1))))
    val tree = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("a"), Lit.String("b"), Lit.String("c")),
      List(first, second)
    )

    assertEquals(tree.prefix.value, "s")
    assertEquals(tree.prefix.tokens.map(_.text).mkString, "s")
    assertEquals(tree.parts.map(_.value), List("a", "b", "c"))
    assertEquals(tree.args, List(first, second))
    assertEquals(delimiters(tree), List("\"", "\""))
    assert(allTrees(tree).forall(_.pos == Position.None))

  test("the bounded standard escape encoder is an exact processEscapes inverse"):
    semanticParts.foreach { case (label, semantic) =>
      val encoded = encodeCandidate(semantic)
      assertEquals(
        StringContext.processEscapes(encoded),
        semantic,
        clues(label, encoded, semantic)
      )
    }

  test("direct construction keeps the ordinary quoted surface for every required semantic part"):
    semanticParts.foreach { case (label, semantic) =>
      val tree = Term.Interpolate(
        Term.Name("s"),
        List(Lit.String(encodeCandidate(semantic))),
        Nil
      )

      assertEquals(delimiters(tree), List("\"", "\""), clues(label, tree.tokens))
      assert(allTrees(tree).forall(_.pos == Position.None), clues(label))
      assertEquals(
        ScalametaTermProjection.project(tree),
        Right(
          ProjectedTermShape(
            TermShape.InterpolatedString("s", List(semantic), Nil),
            None
          )
        ),
        clues(label)
      )
    }

  test("direct and one-Term braced arguments project identically including new"):
    val fresh = Term.New(
      Init(
        Type.Select(
          Term.Select(Term.Name("java"), Term.Name("lang")),
          Type.Name("StringBuilder")
        ),
        Name.Anonymous(),
        List(Term.ArgClause(List(Lit.Int(1))))
      )
    )
    val expected = TermShape.InterpolatedString(
      "s",
      List("", ""),
      List(TermShape.New("java.lang.StringBuilder", List(TermShape.Literal("1"))))
    )
    val direct = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String(""), Lit.String("")),
      List(fresh)
    )
    val braced = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String(""), Lit.String("")),
      List(Term.Block(List(fresh)))
    )

    List(direct, braced).foreach { tree =>
      assertEquals(delimiters(tree), List("\"", "\""))
      assertEquals(
        ScalametaTermProjection.project(tree),
        Right(ProjectedTermShape(expected, None))
      )
      assert(allTrees(tree).forall(_.pos == Position.None))
    }

  test("nested interpolation exposes the N009R recursive delimiter blocker"):
    val nested = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("nested="), Lit.String("")),
      List(Term.Name("x"))
    )
    val direct = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("outer="), Lit.String("")),
      List(nested)
    )
    val braced = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("outer="), Lit.String("")),
      List(Term.Block(List(nested)))
    )
    List(direct, braced).foreach { tree =>
      assertEquals(delimiters(tree), List("\"", "\"", "\"", "\""))
      assertEquals(
        ScalametaTermProjection.project(tree).left.toOption.map(_.code),
        Some("NEUTRAL_INTERPOLATION_SURFACE_UNSUPPORTED")
      )
    }

  private def encodeCandidate(value: String): String =
    value.flatMap {
      case '\\' => "\\\\"
      case '"' => "\\u0022"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case character if character < ' ' || character == '\u007f' =>
        f"\\u${character.toInt}%04x"
      case character => character.toString
    }

  private def delimiters(interpolation: Term.Interpolate): List[String] =
    interpolation.tokens.toList.collect {
      case token: Token.Interpolation.Start => token.text
      case token: Token.Interpolation.End => token.text
    }

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
