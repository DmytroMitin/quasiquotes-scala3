package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaDefinitionProjectionCharacterizationTest extends munit.FunSuite:
  test("Defn is the common input and preserves supported plus neighboring runtime kinds"):
    val supported: List[Defn] = List(
      parsed("val answer: Int = 42"),
      parsed("def answer: Int = 42"),
      parsed("def id(x: Int): Int = x"),
      parsed("def pair(x: Int, y: Int): (Int, Int) = (x, y)"),
      parsed("type Result = Option[Int]")
    )
    val neighboring: List[Defn] = List(
      parsed("var answer: Int = 42"),
      parsed("class Answer"),
      parsed("trait Answer"),
      parsed("object Answer"),
      parsed("enum Answer { case Yes }"),
      parsed("given answer: Ordering[Int] = Ordering.Int")
    )

    assertEquals(
      supported.map(_.productPrefix),
      List("Defn.Val", "Defn.Def", "Defn.Def", "Defn.Def", "Defn.Type")
    )
    assertEquals(
      neighboring.map(_.productPrefix),
      List(
        "Defn.Var",
        "Defn.Class",
        "Defn.Trait",
        "Defn.Object",
        "Defn.Enum",
        "Defn.GivenAlias"
      )
    )

  test("minimal Defn.Def fields distinguish accepted arities from neighboring clause shapes"):
    val fixtures = List(
      "def answer: Int = 42" -> (0, Nil, Nil),
      "def answer(): Int = 42" -> (1, List(0), List(0)),
      "def answer(x: Int): Int = x" -> (1, List(0), List(1)),
      "def answer(x: Int, y: Int): Int = x" -> (1, List(0), List(2)),
      "def answer(x: Int, y: Int, z: Int): Int = x" -> (1, List(0), List(3)),
      "def answer(x: Int)(y: Int): Int = x" -> (1, List(0), List(1, 1)),
      "def answer[A](x: Int): Int = x" -> (1, List(1), List(1)),
      "def answer[A]: Int = 42" -> (1, List(1), Nil),
      "def answer(using x: Int): Int = x" -> (1, List(0), List(1))
    )

    fixtures.foreach { case (source, expected) =>
      val definition = parsed(source).asInstanceOf[Defn.Def]
      val actual = (
        definition.paramClauseGroups.size,
        definition.paramClauseGroups.map(_.tparamClause.values.size),
        definition.paramClauseGroups.flatMap(_.paramClauses).map(_.values.size)
      )

      assertEquals(actual, expected, clues(source))
    }

  test("dispatch evidence is structural and fresh roots remain unpositioned"):
    val positioned = parsed("def id(x: Int): Int = x").asInstanceOf[Defn.Def]
    val unpositioned = positioned.copy()

    assertEquals(positioned.pos, Position.Range(positioned.pos.input, 0, 23))
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(positioned.paramClauseGroups.head.paramClauses.head.values.size, 1)

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")
