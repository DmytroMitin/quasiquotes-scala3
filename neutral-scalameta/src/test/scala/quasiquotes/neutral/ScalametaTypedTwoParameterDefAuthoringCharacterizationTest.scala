package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedTwoParameterDefAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("direct Defn.Def construction exposes the exact N023 ordinary two-parameter topology"):
    val definition = direct(
      "combine",
      "left",
      Type.Name("Int"),
      "right",
      Type.Name("String"),
      Type.Tuple(List(Type.Name("Int"), Type.Name("String"))),
      Term.Tuple(List(Term.Name("left"), Term.Name("right")))
    )
    val group = definition.paramClauseGroups.head
    val clause = group.paramClauses.head
    val first = clause.values.head
    val second = clause.values(1)

    assertEquals(definition.productPrefix, "Defn.Def")
    assertEquals(definition.mods, Nil)
    assertEquals(definition.paramClauseGroups.size, 1)
    assertEquals(group.tparamClause.values, Nil)
    assertEquals(group.paramClauses.size, 1)
    assertEquals(clause.mod, None)
    assertEquals(clause.values.size, 2)
    assertEquals(clause.values.map(_.name.value), List("left", "right"))
    assertEquals(first.mods, Nil)
    assertEquals(second.mods, Nil)
    assertEquals(first.default, None)
    assertEquals(second.default, None)
    assertEquals(first.decltpe.collect { case name: Type.Name => name.value }, Some("Int"))
    assertEquals(second.decltpe.collect { case name: Type.Name => name.value }, Some("String"))
    assert(!first.decltpe.get.isInstanceOf[Type.ByName])
    assert(!first.decltpe.get.isInstanceOf[Type.Repeated])
    assert(!second.decltpe.get.isInstanceOf[Type.ByName])
    assert(!second.decltpe.get.isInstanceOf[Type.Repeated])
    assert(definition.decltpe.nonEmpty)
    assert(definition.body != null)

  test("a direct fresh ordinary method is entirely unpositioned and projects through N023"):
    val definition = direct(
      "pair",
      "left",
      Type.Name("Int"),
      "right",
      Type.Name("String"),
      Type.Tuple(List(Type.Name("String"), Type.Name("Int"))),
      Term.Tuple(List(Term.Name("right"), Term.Name("left")))
    )
    val expected = DefinitionShape
      .twoParameterDef(
        DefinitionName.plain("pair").toOption.get,
        BinderId(0),
        DefinitionName.plain("left").toOption.get,
        TypeShape.Identifier("Int"),
        BinderId(1),
        DefinitionName.plain("right").toOption.get,
        TypeShape.Identifier("String"),
        TypeShape.Tuple(List(TypeShape.Identifier("String"), TypeShape.Identifier("Int"))),
        TermShape.Tuple(
          List(
            TermShape.BoundReference(BinderId(1), "right"),
            TermShape.BoundReference(BinderId(0), "left")
          )
        )
      )
      .toOption
      .get

    assert(allTrees(definition).forall(_.pos == Position.None))
    assertEquals(
      ScalametaTypedTwoParameterDefProjection.project(definition),
      Right(ProjectedDefinitionShape(expected, None))
    )

  test("fresh keyword names retain exact source spelling at all three declaration sites"):
    val definition = direct(
      "class",
      "match",
      Type.Name("Int"),
      "type",
      Type.Name("Int"),
      Type.Tuple(List(Type.Name("Int"), Type.Name("Int"))),
      Term.Tuple(List(Term.Name("match"), Term.Name("type")))
    )
    val parameters = definition.paramClauseGroups.head.paramClauses.head.values

    assertEquals(definition.name.value, "class")
    assertEquals(definition.name.tokens.map(_.text).mkString, "`class`")
    assertEquals(parameters.head.name.tokens.map(_.text).mkString, "`match`")
    assertEquals(parameters(1).name.tokens.map(_.text).mkString, "`type`")
    assertEquals(
      definition.body.children.collect { case name: Term.Name => name.tokens.map(_.text).mkString },
      List("`match`", "`type`")
    )
    assert(ScalametaTypedTwoParameterDefProjection.project(definition).isRight)

  private def direct(
      methodName: String,
      firstName: String,
      firstType: Type,
      secondName: String,
      secondType: Type,
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
              List(
                Term.Param(Nil, Term.Name(firstName), Some(firstType), None),
                Term.Param(Nil, Term.Name(secondName), Some(secondType), None)
              )
            )
          )
        )
      ),
      Some(resultType),
      body
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
