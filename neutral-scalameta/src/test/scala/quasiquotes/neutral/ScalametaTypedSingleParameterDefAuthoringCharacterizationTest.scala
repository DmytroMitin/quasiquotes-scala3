package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedSingleParameterDefAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("direct Defn.Def construction exposes the exact N022 ordinary one-parameter topology"):
    val definition = direct("id", "x", Type.Name("Int"), Type.Name("Int"), Term.Name("x"))
    val group = definition.paramClauseGroups.head
    val clause = group.paramClauses.head
    val parameter = clause.values.head

    assertEquals(definition.productPrefix, "Defn.Def")
    assertEquals(definition.mods, Nil)
    assertEquals(definition.paramClauseGroups.size, 1)
    assertEquals(group.tparamClause.values, Nil)
    assertEquals(group.paramClauses.size, 1)
    assertEquals(clause.mod, None)
    assertEquals(clause.values.size, 1)
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.default, None)
    assert(parameter.decltpe.nonEmpty)
    assert(!parameter.decltpe.get.isInstanceOf[Type.ByName])
    assert(!parameter.decltpe.get.isInstanceOf[Type.Repeated])
    assert(definition.decltpe.nonEmpty)
    assert(definition.body != null)

  test("a direct fresh ordinary method is entirely unpositioned and projects through N022"):
    val definition = direct("id", "x", Type.Name("Int"), Type.Name("Int"), Term.Name("x"))
    val expected = DefinitionShape
      .singleParameterDef(
        DefinitionName.plain("id").toOption.get,
        BinderId(0),
        DefinitionName.plain("x").toOption.get,
        TypeShape.Identifier("Int"),
        TypeShape.Identifier("Int"),
        TermShape.BoundReference(BinderId(0), "x")
      )
      .toOption
      .get

    assert(allTrees(definition).forall(_.pos == Position.None))
    assertEquals(
      ScalametaTypedSingleParameterDefProjection.project(definition),
      Right(ProjectedDefinitionShape(expected, None))
    )

  test("fresh keyword names retain their exact source spelling at both declaration sites"):
    val definition = direct(
      "type",
      "match",
      Type.Name("Int"),
      Type.Name("Int"),
      Term.Name("match")
    )
    val parameter = definition.paramClauseGroups.head.paramClauses.head.values.head

    assertEquals(definition.name.value, "type")
    assertEquals(definition.name.tokens.map(_.text).mkString, "`type`")
    assertEquals(parameter.name.value, "match")
    assertEquals(parameter.name.tokens.map(_.text).mkString, "`match`")
    assertEquals(definition.body.asInstanceOf[Term.Name].tokens.map(_.text).mkString, "`match`")
    assert(ScalametaTypedSingleParameterDefProjection.project(definition).isRight)

  private def direct(
      methodName: String,
      parameterName: String,
      parameterType: Type,
      resultType: Type,
      body: Term
  ): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(Term.Param(Nil, Term.Name(parameterName), Some(parameterType), None))
            )
          )
        )
      ),
      Some(resultType),
      body
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
