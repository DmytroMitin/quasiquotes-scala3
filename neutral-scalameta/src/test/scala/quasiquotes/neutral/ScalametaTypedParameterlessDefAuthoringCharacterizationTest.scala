package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionNameSpelling, DefinitionShape}
import _root_.quasiquotes.parser.{TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypedParameterlessDefAuthoringCharacterizationTest extends munit.FunSuite:
  test("direct Defn.Def construction exposes the exact N021 true-parameterless topology"):
    val definition = direct("answer", Type.Name("Int"), Lit.Int(42))
    val expected = DefinitionShape
      .parameterlessDef(
        DefinitionName.plain("answer").toOption.get,
        TypeShape.Identifier("Int"),
        TermShape.Literal("42")
      )
      .toOption
      .get

    assertEquals(definition.productPrefix, "Defn.Def")
    assertEquals(definition.mods, Nil)
    assertEquals(definition.paramClauseGroups, Nil)
    definition.decltpe match
      case Some(name: Type.Name) => assertEquals(name.value, "Int")
      case other => fail(s"expected explicit Int result Type, found $other")
    definition.body match
      case literal: Lit.Int => assertEquals(literal.value, 42)
      case other => fail(s"expected integer literal body, found ${other.productPrefix}")
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(definition),
      Right(ProjectedDefinitionShape(expected, None))
    )

  test("direct true-parameterless topology remains distinct from parsed empty parens"):
    val parameterless = direct("answer", Type.Name("Int"), Lit.Int(42))
    val emptyParens = Scala3("def answer(): Int = 42").parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

    assertEquals(parameterless.paramClauseGroups, Nil)
    assert(emptyParens.paramClauseGroups.nonEmpty)
    assert(ScalametaTypedParameterlessDefProjection.project(parameterless).isRight)
    assert(ScalametaTypedParameterlessDefProjection.project(emptyParens).isLeft)

  test("fresh direct methods preserve exact names and have only unpositioned descendants"):
    val ordinary = direct("answer", Type.Name("Int"), Lit.Int(42))
    val keyword = direct(
      "type",
      Type.Apply(Type.Name("Option"), Type.ArgClause(List(Type.Name("Int")))),
      Term.Select(Term.Name("service"), Term.Name("answer"))
    )
    val projectedKeyword = ScalametaDefinitionNameProjection.project(keyword.name).toOption.get

    assertEquals(ordinary.name.value, "answer")
    assertEquals(ordinary.name.tokens.map(_.text).mkString, "answer")
    assertEquals(keyword.name.value, "type")
    assertEquals(keyword.name.tokens.map(_.text).mkString, "`type`")
    assertEquals(projectedKeyword.source, "`type`")
    assertEquals(projectedKeyword.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assert(allTrees(ordinary).forall(_.pos == Position.None))
    assert(allTrees(keyword).forall(_.pos == Position.None))

  private def direct(name: String, resultType: Type, body: Term): Defn.Def =
    Defn.Def(Nil, Term.Name(name), Nil, Some(resultType), body)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
