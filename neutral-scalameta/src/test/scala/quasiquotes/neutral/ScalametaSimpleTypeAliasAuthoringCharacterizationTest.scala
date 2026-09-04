package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaSimpleTypeAliasAuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh Type.Name preserves plain and backticked-keyword projection spelling"):
    val plain = Type.Name("Result")
    val keyword = Type.Name("type")

    assertEquals(plain.value, "Result")
    assertEquals(plain.tokens.map(_.text).mkString, "Result")
    assertEquals(keyword.value, "type")
    assertEquals(keyword.tokens.map(_.text).mkString, "`type`")

    val projectedPlain = ScalametaDefinitionNameProjection.project(plain).toOption.get
    val projectedKeyword = ScalametaDefinitionNameProjection.project(keyword).toOption.get
    assertEquals(projectedPlain.decoded, "Result")
    assertEquals(projectedPlain.source, "Result")
    assertEquals(projectedKeyword.decoded, "type")
    assertEquals(projectedKeyword.source, "`type`")

  test("direct Defn.Type construction has the simple alias topology in the current API"):
    val definition = Defn.Type(
      Nil,
      Type.Name("Result"),
      Type.ParamClause(Nil),
      Type.Name("Int"),
      Type.Bounds.empty
    )

    assertEquals(definition.productPrefix, "Defn.Type")
    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "Result")
    assertEquals(definition.tparamClause.values, Nil)
    assertEquals(definition.body.asInstanceOf[Type.Name].value, "Int")
    assertEmptyBounds(definition.bounds)

  test("direct alias roots and descendants are fresh and unpositioned"):
    val definition = Defn.Type(
      Nil,
      Type.Name("Result"),
      Type.ParamClause(Nil),
      Type.Apply(Type.Name("Option"), Type.ArgClause(List(Type.Name("Int")))),
      Type.Bounds.empty
    )
    val applied = definition.body.asInstanceOf[Type.Apply]
    val trees: List[Tree] = List(
      definition,
      definition.name,
      definition.tparamClause,
      definition.bounds,
      applied,
      applied.tpe,
      applied.argClause,
      applied.args.head
    )

    assert(trees.forall(_.pos == Position.None), clues(trees.filterNot(_.pos == Position.None)))

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)
