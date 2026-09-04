package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedSingleParameterDefCharacterizationTest extends munit.FunSuite:
  test("pins the exact one-ordinary-parameter Defn.Def topology and its neighbors"):
    val canonical = parsed("def id(x: Int): Int = x")
    val parameterless = parsed("def id: Int = 1")
    val emptyParens = parsed("def id(): Int = 1")
    val twoParameters = parsed("def id(x: Int, y: Int): Int = x")
    val twoClauses = parsed("def id(x: Int)(y: Int): Int = x")
    val typeParameterized = parsed("def id[A](x: Int): Int = x")
    val contextual = parsed("def id(using x: Int): Int = x")
    val byName = parsed("def id(x: => Int): Int = x")
    val repeated = parsed("def id(x: Int*): Int = x")
    val defaulted = parsed("def id(x: Int = 1): Int = x")

    val (canonicalGroup, canonicalClause, canonicalParameter) = oneParameter(canonical)
    assertEquals(canonical.mods, Nil)
    assertEquals(canonicalGroup.tparamClause.values, Nil)
    assertEquals(canonicalClause.mod, None)
    assertEquals(canonicalParameter.mods, Nil)
    assertEquals(canonicalParameter.default, None)
    assertTypeKind(canonicalParameter.decltpe, "Type.Name")
    assertTypeKind(canonical.decltpe, "Type.Name")
    assertEquals(canonical.body.productPrefix, "Term.Name")

    assertEquals(parameterless.paramClauseGroups, Nil)
    assertEquals(emptyParens.paramClauseGroups.size, 1)
    assertEquals(emptyParens.paramClauseGroups.head.paramClauses.map(_.values), List(Nil))
    assertEquals(twoParameters.paramClauseGroups.head.paramClauses.head.values.size, 2)
    assertEquals(twoClauses.paramClauseGroups.head.paramClauses.size, 2)
    assertEquals(typeParameterized.paramClauseGroups.head.tparamClause.values.size, 1)
    assert(
      contextual.paramClauseGroups.head.paramClauses.head.mod
        .exists(_.isInstanceOf[Mod.Using])
    )
    assertTypeKind(oneParameter(byName)._3.decltpe, "Type.ByName")
    assertTypeKind(oneParameter(repeated)._3.decltpe, "Type.Repeated")
    assert(oneParameter(defaulted)._3.default.nonEmpty)

  test("preserves method and parameter decoded values plus source token spellings"):
    val ordinarySource = "def answer(value: Int): Int = value"
    val keywordSource = "def `type`(`match`: Int): Int = `match`"
    val ordinary = parsed(ordinarySource)
    val keyword = parsed(keywordSource)

    assertNameEvidence(ordinary.name, "answer", "answer")
    assertNameEvidence(oneParameter(ordinary)._3.name, "value", "value")
    assertNameEvidence(keyword.name, "type", "`type`")
    assertNameEvidence(oneParameter(keyword)._3.name, "match", "`match`")
    assertEquals(ordinary.pos, Position.Range(ordinary.pos.input, 0, ordinarySource.length))
    assertEquals(keyword.pos, Position.Range(keyword.pos.input, 0, keywordSource.length))

  test("allows the method and parameter to share spelling while keeping distinct roles"):
    val definition = parsed("def answer(answer: Int): Int = answer")
    val parameter = oneParameter(definition)._3

    assertNameEvidence(definition.name, "answer", "answer")
    assertNameEvidence(parameter.name, "answer", "answer")
    definition.body match
      case name: Term.Name => assertNameEvidence(name, "answer", "answer")
      case other => fail(s"expected Term.Name body, found ${other.productPrefix}")

  test("direct construction is unpositioned and retains the selected structural fields"):
    val definition = Defn.Def(
      Nil,
      Term.Name("id"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Int")), None))
            )
          )
        )
      ),
      Some(Type.Name("Int")),
      Term.Name("x")
    )

    val (_, clause, parameter) = oneParameter(definition)
    assertEquals(clause.mod, None)
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.default, None)
    assertEquals(definition.pos, Position.None)
    assertNameEvidence(definition.name, "id", "id")
    assertNameEvidence(parameter.name, "x", "x")

  private def oneParameter(
      definition: Defn.Def
  ): (Member.ParamClauseGroup, Term.ParamClause, Term.Param) =
    definition.paramClauseGroups match
      case List(group) =>
        group.paramClauses match
          case List(clause) =>
            clause.values match
              case List(parameter) => (group, clause, parameter)
              case other => fail(s"expected one Term.Param, found $other")
          case other => fail(s"expected one value clause, found $other")
      case other => fail(s"expected one parameter-clause group, found $other")

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
