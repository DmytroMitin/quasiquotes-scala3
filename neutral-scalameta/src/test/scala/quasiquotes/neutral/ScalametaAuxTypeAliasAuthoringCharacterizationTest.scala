package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaAuxTypeAliasAuthoringCharacterizationTest extends munit.FunSuite:
  test("direct constructors express the exact bounded Defn.Type topology"):
    val definition = canonicalDefinition()

    assertEquals(definition.name.value, "Aux")
    assertEquals(definition.mods, Nil)
    assertEmptyBounds(definition.bounds)

    val parameters = definition.tparamClause.values
    assertEquals(parameters.map(_.name.value), List("N", "M", "Out0"))
    assertEquals(parameters.map(_.mods), List.fill(3)(Nil))
    assertEquals(parameters.map(_.tparamClause.values), List.fill(3)(Nil))
    assertEquals(parameters.map(_.bounds.lo), List.fill(3)(None))
    assertEquals(
      parameters.map(_.bounds.hi.map(_.asInstanceOf[Type.Name].value)),
      List.fill(3)(Some("Nat"))
    )
    assertEquals(parameters.map(_.bounds.context), List.fill(3)(Nil))
    assertEquals(parameters.map(_.bounds.view), List.fill(3)(Nil))

    val refinement = definition.body.asInstanceOf[Type.Refine]
    val applied = refinement.tpe.get.asInstanceOf[Type.Apply]
    assertEquals(applied.tpe.asInstanceOf[Type.Name].value, "Add")
    assertEquals(
      applied.argClause.values.map(_.asInstanceOf[Type.Name].value),
      List("N", "M")
    )

    val member = refinement.stats.head.asInstanceOf[Defn.Type]
    assertEquals(refinement.stats.size, 1)
    assertEquals(member.mods, Nil)
    assertEquals(member.name.value, "Out")
    assertEquals(member.tparamClause.values, Nil)
    assertEmptyBounds(member.bounds)
    assertEquals(member.body.asInstanceOf[Type.Name].value, "Out0")

  test("direct constructor roots children bounds and clauses are unpositioned"):
    val definition = canonicalDefinition()
    val parameters = definition.tparamClause.values
    val refinement = definition.body.asInstanceOf[Type.Refine]
    val applied = refinement.tpe.get.asInstanceOf[Type.Apply]
    val member = refinement.stats.head.asInstanceOf[Defn.Type]

    val constructedTrees: List[Tree] =
      List(
        definition,
        definition.name,
        definition.tparamClause,
        definition.bounds,
        refinement,
        refinement.body,
        applied,
        applied.tpe,
        applied.argClause,
        member,
        member.name,
        member.tparamClause,
        member.bounds,
        member.body
      ) ++ parameters.flatMap(parameter =>
        List(
          parameter,
          parameter.name,
          parameter.tparamClause,
          parameter.bounds,
          parameter.bounds.hi.get
        )
      ) ++ applied.argClause.values

    assert(constructedTrees.forall(_.pos == Position.None), clues(constructedTrees.filterNot(_.pos == Position.None)))

  private def canonicalDefinition(): Defn.Type =
    val parameters = List("N", "M", "Out0").map { name =>
      Type.Param(
        Nil,
        Type.Name(name),
        Type.ParamClause(Nil),
        Type.Bounds(None, Some(Type.Name("Nat")), Nil, Nil)
      )
    }
    val applied = Type.Apply(
      Type.Name("Add"),
      Type.ArgClause(List(Type.Name("N"), Type.Name("M")))
    )
    val member = Defn.Type(
      Nil,
      Type.Name("Out"),
      Type.ParamClause(Nil),
      Type.Name("Out0"),
      Type.Bounds.empty
    )
    val refinement = Type.Refine(Some(applied), Stat.Block(List(member)))
    Defn.Type(
      Nil,
      Type.Name("Aux"),
      Type.ParamClause(parameters),
      refinement,
      Type.Bounds.empty
    )

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)
