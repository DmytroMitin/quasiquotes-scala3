package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaParameterlessDelegatedForwardingMethodCharacterizationTest
    extends munit.FunSuite:
  private val Canonical = "def empty[A](using inst: Empty[A]): A = inst.empty"

  test("pins parsed canonical and renamed structures field by field"):
    List(
      parse(Canonical) -> Vector("empty", "A", "inst", "Empty"),
      parse("def obtain[Element](using evidence: Provider[Element]): Element = evidence.obtain") ->
        Vector("obtain", "Element", "evidence", "Provider")
    ).foreach { case (definition, names) =>
      assertEquals(definition.mods, Nil)
      assertEquals(definition.name.value, names(0))
      definition.paramClauseGroups match
        case List(Member.ParamClauseGroup(Type.ParamClause(List(typeParameter)), List(clause))) =>
          assertExactTypeParameter(typeParameter, names(1))
          clause.mod match
            case Some(_: Mod.Using) => ()
            case other => fail("expected structural using clause, found " + other)
          clause.values match
            case List(parameter) =>
              parameter.mods match
                case List(_: Mod.Using) => ()
                case other => fail("expected parameter Mod.Using, found " + other)
              assertEquals(parameter.name.value, names(2))
              assertEquals(parameter.default, None)
              parameter.decltpe match
                case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
                  assertEquals(constructor.value, names(3))
                  assertEquals(argument.value, names(1))
                case other => fail("expected direct unary evidence Type, found " + other)
            case other => fail("expected one contextual parameter, found " + other)
        case other => fail("expected one exact parameter-clause group, found " + other)

      definition.decltpe match
        case Some(result: Type.Name) => assertEquals(result.value, names(1))
        case other => fail("expected direct result Type name, found " + other)
      definition.body match
        case Term.Select(receiver: Term.Name, selected: Term.Name) =>
          assertEquals(receiver.value, names(2))
          assertEquals(selected.value, names(0))
        case other => fail("expected direct stable selection, found " + other.productPrefix)
    }

  private def assertExactTypeParameter(parameter: Type.Param, name: String): Unit =
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.name.value, name)
    assertEquals(parameter.tparamClause.values, Nil)
    assertEquals(parameter.bounds.lo, None)
    assertEquals(parameter.bounds.hi, None)
    assertEquals(parameter.bounds.context, Nil)
    assertEquals(parameter.bounds.view, Nil)

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail("expected Defn.Def, found " + other.productPrefix)
