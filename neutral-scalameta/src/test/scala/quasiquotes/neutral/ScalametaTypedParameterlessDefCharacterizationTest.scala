package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedParameterlessDefCharacterizationTest extends munit.FunSuite:
  test("distinguishes true parameterless empty-parens type-only and value-parameter groups"):
    val parameterless = parsed("def answer: Int = 42")
    val emptyParens = parsed("def answer(): Int = 42")
    val typeOnly = parsed("def answer[A]: Int = 42")
    val oneParameter = parsed("def answer(x: Int): Int = x")

    assertEquals(parameterless.mods, Nil)
    assertEquals(parameterless.paramClauseGroups, Nil)
    assertExplicitIntAndBodyKind(parameterless, "Lit.Int")

    emptyParens.paramClauseGroups match
      case List(group) =>
        assertEquals(group.tparamClause.values, Nil)
        group.paramClauses match
          case List(clause) =>
            assertEquals(clause.mod, None)
            assertEquals(clause.values, Nil)
          case other => fail(s"expected one empty value clause, found $other")
      case other => fail(s"expected one empty-parens group, found $other")
    assertExplicitIntAndBodyKind(emptyParens, "Lit.Int")

    typeOnly.paramClauseGroups match
      case List(group) =>
        assertEquals(group.tparamClause.values.map(_.name.value), List("A"))
        assertEquals(group.paramClauses, Nil)
      case other => fail(s"expected one type-only group, found $other")
    assertExplicitIntAndBodyKind(typeOnly, "Lit.Int")

    oneParameter.paramClauseGroups match
      case List(group) =>
        assertEquals(group.tparamClause.values, Nil)
        group.paramClauses match
          case List(clause) =>
            assertEquals(clause.mod, None)
            assertEquals(clause.values.map(_.name.value), List("x"))
          case other => fail(s"expected one ordinary value clause, found $other")
      case other => fail(s"expected one ordinary parameter group, found $other")
    assertExplicitIntAndBodyKind(oneParameter, "Term.Name")

  test("preserves decoded and token spelling for ordinary and backticked method names"):
    val ordinary = parsed("def answer: Int = 42")
    val keyword = parsed("def `type`: Int = 42")

    assertNameEvidence(ordinary, "answer", "answer")
    assertNameEvidence(keyword, "type", "`type`")
    assertEquals(ordinary.pos, Position.Range(ordinary.pos.input, 0, 20))
    assertEquals(keyword.pos, Position.Range(keyword.pos.input, 0, 20))

  test("direct construction is unpositioned and retains structural parameterless fields"):
    val ordinary = direct("answer")
    val keyword = direct("type")

    List(ordinary, keyword).foreach { definition =>
      assertEquals(definition.mods, Nil)
      assertEquals(definition.paramClauseGroups, Nil)
      assertExplicitInt(definition)
      assert(definition.body.isInstanceOf[Lit.Int], clues(definition.body))
      assertEquals(definition.pos, Position.None)
    }
    assertNameEvidence(ordinary, "answer", "answer")
    assertNameEvidence(keyword, "type", "`type`")

  private def assertExplicitIntAndBodyKind(
      definition: Defn.Def,
      expectedBodyKind: String
  ): Unit =
    assertExplicitInt(definition)
    assertEquals(definition.body.productPrefix, expectedBodyKind)

  private def assertExplicitInt(definition: Defn.Def): Unit =
    definition.decltpe match
      case Some(name: Type.Name) => assertEquals(name.value, "Int")
      case other => fail(s"expected explicit Int result Type, found $other")

  private def assertNameEvidence(
      definition: Defn.Def,
      expectedValue: String,
      expectedTokenText: String
  ): Unit =
    assertEquals(definition.name.value, expectedValue)
    assertEquals(definition.name.tokens.map(_.text).mkString, expectedTokenText)

  private def parsed(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def direct(name: String): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(name),
      Nil,
      Some(Type.Name("Int")),
      Lit.Int(42)
    )
