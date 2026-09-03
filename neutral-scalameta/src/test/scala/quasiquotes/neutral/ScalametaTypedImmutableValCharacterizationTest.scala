package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedImmutableValCharacterizationTest extends munit.FunSuite:
  test("Scalameta preserves decoded and token spelling for ordinary and backticked val names"):
    val ordinary = parsed("val answer: Int = 42")
    val keyword = parsed("val `type`: Int = 42")

    assertNameEvidence(ordinary, "answer", "answer")
    assertNameEvidence(keyword, "type", "`type`")
    assertEquals(ordinary.pos, Position.Range(ordinary.pos.input, 0, 20))
    assertEquals(keyword.pos, Position.Range(keyword.pos.input, 0, 20))

  test("direct unpositioned names expose generated token spelling without source provenance"):
    val ordinary = direct("answer")
    val keywordValue = direct("type")

    assertNameEvidence(ordinary, "answer", "answer")
    assertNameEvidence(keywordValue, "type", "`type`")
    assertEquals(ordinary.pos, Position.None)
    assertEquals(keywordValue.pos, Position.None)

  private def assertNameEvidence(
      definition: Defn.Val,
      expectedValue: String,
      expectedTokenText: String
  ): Unit =
    definition.pats match
      case Pat.Var(name) :: Nil =>
        assertEquals(name.value, expectedValue)
        assertEquals(name.tokens.map(_.text).mkString, expectedTokenText)
      case other => fail(s"expected one Pat.Var, found $other")

  private def parsed(source: String): Defn.Val =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Val => definition
      case other => fail(s"expected Defn.Val, found ${other.productPrefix}")

  private def direct(name: String): Defn.Val =
    Defn.Val(
      Nil,
      List(Pat.Var(Term.Name(name))),
      Some(Type.Name("Int")),
      Lit.Int(42)
    )
