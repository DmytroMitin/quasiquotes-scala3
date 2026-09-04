package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedTwoParameterDefCharacterizationTest extends munit.FunSuite:
  test("pins the exact two-ordinary-parameter Defn.Def topology and its neighbors"):
    val canonical = parsed("def combine(x: Int, y: Int): Int = x + y")
    val parameterless = parsed("def combine: Int = 0")
    val emptyParens = parsed("def combine(): Int = 0")
    val oneParameter = parsed("def combine(x: Int): Int = x")
    val threeParameters = parsed("def combine(x: Int, y: Int, z: Int): Int = x")
    val twoClauses = parsed("def combine(x: Int)(y: Int): Int = x")
    val typeParameterized = parsed("def combine[A](x: Int, y: Int): Int = x")
    val contextual = parsed("def combine(using x: Int, y: Int): Int = x")
    val byNameFirst = parsed("def combine(x: => Int, y: Int): Int = x")
    val byNameSecond = parsed("def combine(x: Int, y: => Int): Int = x")
    val repeatedFirst = Scala3("def combine(x: Int*, y: Int): Int = y").parse[Stat]
    val repeatedSecond = parsed("def combine(x: Int, y: Int*): Int = x")
    val defaultedFirst = parsed("def combine(x: Int = 0, y: Int): Int = y")
    val defaultedSecond = parsed("def combine(x: Int, y: Int = 0): Int = x")

    val (group, clause, first, second) = twoParameters(canonical)
    assertEquals(canonical.mods, Nil)
    assertEquals(group.tparamClause.values, Nil)
    assertEquals(group.paramClauses.size, 1)
    assertEquals(clause.mod, None)
    assertEquals(clause.values.size, 2)
    List(first, second).foreach { parameter =>
      assertEquals(parameter.mods, Nil)
      assertEquals(parameter.default, None)
      assertTypeKind(parameter.decltpe, "Type.Name")
    }
    assertTypeKind(canonical.decltpe, "Type.Name")
    canonical.body match
      case infix: Term.ApplyInfix =>
        assertEquals(infix.lhs.asInstanceOf[Term.Name].value, "x")
        assertEquals(infix.op.value, "+")
        assertEquals(
          infix.argClause.values.map(_.asInstanceOf[Term.Name].value),
          List("y")
        )
      case other => fail(s"expected ordered x + y body, found ${other.productPrefix}")

    assertEquals(parameterless.paramClauseGroups, Nil)
    assertEquals(emptyParens.paramClauseGroups.head.paramClauses.head.values, Nil)
    assertEquals(oneParameter.paramClauseGroups.head.paramClauses.head.values.size, 1)
    assertEquals(threeParameters.paramClauseGroups.head.paramClauses.head.values.size, 3)
    assertEquals(twoClauses.paramClauseGroups.head.paramClauses.size, 2)
    assertEquals(typeParameterized.paramClauseGroups.head.tparamClause.values.size, 1)
    assert(contextual.paramClauseGroups.head.paramClauses.head.mod.exists(_.isInstanceOf[Mod.Using]))
    assertTypeKind(twoParameters(byNameFirst)._3.decltpe, "Type.ByName")
    assertTypeKind(twoParameters(byNameSecond)._4.decltpe, "Type.ByName")
    assert(repeatedFirst.isInstanceOf[Parsed.Error])
    assertTypeKind(twoParameters(repeatedSecond)._4.decltpe, "Type.Repeated")
    assert(twoParameters(defaultedFirst)._3.default.nonEmpty)
    assert(twoParameters(defaultedSecond)._4.default.nonEmpty)

  test("preserves method and both parameter decoded values plus source token spellings"):
    val ordinarySource = "def combine(left: Int, right: Int): Int = left + right"
    val keywordSource = "def `type`(`match`: Int, `then`: Int): Int = `match` + `then`"
    val ordinary = parsed(ordinarySource)
    val keyword = parsed(keywordSource)
    val (_, _, ordinaryFirst, ordinarySecond) = twoParameters(ordinary)
    val (_, _, keywordFirst, keywordSecond) = twoParameters(keyword)

    assertNameEvidence(ordinary.name, "combine", "combine")
    assertNameEvidence(ordinaryFirst.name, "left", "left")
    assertNameEvidence(ordinarySecond.name, "right", "right")
    assertNameEvidence(keyword.name, "type", "`type`")
    assertNameEvidence(keywordFirst.name, "match", "`match`")
    assertNameEvidence(keywordSecond.name, "then", "`then`")
    assertEquals(ordinary.pos, Position.Range(ordinary.pos.input, 0, ordinarySource.length))
    assertEquals(keyword.pos, Position.Range(keyword.pos.input, 0, keywordSource.length))

  test("preserves source order and permits the method spelling to equal either parameter"):
    val firstMatches = parsed("def answer(answer: Int, y: Int): Int = answer + y")
    val secondMatches = parsed("def answer(x: Int, answer: Int): Int = x + answer")

    val (_, _, first, firstSecond) = twoParameters(firstMatches)
    assertNameEvidence(firstMatches.name, "answer", "answer")
    assertNameEvidence(first.name, "answer", "answer")
    assertNameEvidence(firstSecond.name, "y", "y")

    val (_, _, secondFirst, second) = twoParameters(secondMatches)
    assertNameEvidence(secondMatches.name, "answer", "answer")
    assertNameEvidence(secondFirst.name, "x", "x")
    assertNameEvidence(second.name, "answer", "answer")

  test("direct construction is unpositioned and can represent missing explicit Types"):
    val definition = direct(
      Term.Name("combine"),
      Term.Param(Nil, Term.Name("x"), None, None),
      Term.Param(Nil, Term.Name("y"), Some(Type.Name("Int")), None),
      None,
      Term.Name("x")
    )
    val (_, clause, first, second) = twoParameters(definition)

    assertEquals(definition.pos, Position.None)
    assertEquals(clause.mod, None)
    assertEquals(first.decltpe, None)
    assertTypeKind(second.decltpe, "Type.Name")
    assertEquals(definition.decltpe, None)

  private def twoParameters(
      definition: Defn.Def
  ): (Member.ParamClauseGroup, Term.ParamClause, Term.Param, Term.Param) =
    definition.paramClauseGroups match
      case List(group) =>
        group.paramClauses match
          case List(clause) =>
            clause.values match
              case List(first, second) => (group, clause, first, second)
              case other => fail(s"expected two Term.Param values, found $other")
          case other => fail(s"expected one value clause, found $other")
      case other => fail(s"expected one parameter-clause group, found $other")

  private def direct(
      name: Term.Name,
      first: Term.Param,
      second: Term.Param,
      resultType: Option[Type],
      body: Term
  ): Defn.Def =
    Defn.Def(
      Nil,
      name,
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(Term.ParamClause(List(first, second)))
        )
      ),
      resultType,
      body
    )

  private def assertTypeKind(sourceType: Option[Type], expected: String): Unit =
    sourceType match
      case Some(value) => assertEquals(value.productPrefix, expected)
      case None => fail(s"expected $expected, found no Type")

  private def assertNameEvidence(
      name: Name,
      expectedValue: String,
      expectedTokenText: String
  ): Unit =
    assertEquals(name.value, expectedValue)
    assertEquals(name.tokens.map(_.text).mkString, expectedTokenText)

  private def parsed(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")
