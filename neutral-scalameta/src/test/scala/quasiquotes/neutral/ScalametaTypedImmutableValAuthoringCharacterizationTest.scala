package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionNameSpelling}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTypedImmutableValAuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh Term.Name and Pat.Var preserve exact ordinary declaration-name semantics"):
    val expected = DefinitionName.plain("answer").toOption.get
    val name = Term.Name(expected.decoded)
    val pattern = Pat.Var(name)

    assertEquals(name.value, "answer")
    assertEquals(name.tokens.map(_.text).mkString, "answer")
    assertEquals(ScalametaDefinitionNameProjection.project(name), Right(expected))
    assertEquals(pattern.name, name)
    assertEquals(name.pos, Position.None)
    assertEquals(pattern.pos, Position.None)

  test("fresh Term.Name and Pat.Var preserve exact backticked-keyword declaration-name semantics"):
    val expected = DefinitionName.backticked("`type`").toOption.get
    val name = Term.Name(expected.decoded)
    val pattern = Pat.Var(name)
    val projected = ScalametaDefinitionNameProjection.project(name).toOption.get

    assertEquals(name.value, "type")
    assertEquals(name.tokens.map(_.text).mkString, "`type`")
    assertEquals(projected, expected)
    assertEquals(projected.source, "`type`")
    assertEquals(projected.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertEquals(pattern.name, name)
    assertEquals(name.pos, Position.None)
    assertEquals(pattern.pos, Position.None)

  test("direct Defn.Val construction exposes the exact N020 immutable-val topology"):
    val name = Term.Name("answer")
    val pattern = Pat.Var(name)
    val declaredType = Type.Name("Int")
    val rhs = Lit.Int(42)
    val definition = Defn.Val(Nil, List(pattern), Some(declaredType), rhs)

    assertEquals(definition.productPrefix, "Defn.Val")
    assertEquals(definition.mods, Nil)
    assertEquals(definition.pats, List(pattern))
    assertEquals(definition.decltpe, Some(declaredType))
    assertEquals(definition.rhs, rhs)
    assertEquals(pattern.name, name)

  test("direct Defn.Val roots patterns names Types and RHS descendants are unpositioned"):
    val definition = Defn.Val(
      Nil,
      List(Pat.Var(Term.Name("answer"))),
      Some(
        Type.Apply(
          Type.Name("Option"),
          Type.ArgClause(List(Type.Name("Int")))
        )
      ),
      Term.Apply(
        Term.Select(Term.Name("service"), Term.Name("compute")),
        Term.ArgClause(List(Lit.Int(42)))
      )
    )

    assert(allTrees(definition).forall(_.pos == Position.None))

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
