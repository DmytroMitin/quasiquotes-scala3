package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaTypeMemberInstanceFactoryCharacterizationTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A, Out0]: HasOut[A] { type Out = Out0 } =
      |  new HasOut[A]:
      |    type Out = Out0""".stripMargin

  test("pins the parsed outer definition and refined result field by field"):
    val definition = parse(Canonical)
    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "instance")
    definition.paramClauseGroups match
      case List(group) =>
        assertEquals(group.paramClauses, Nil)
        group.tparamClause.values match
          case List(first, second) =>
            assertTypeParameter(first, "A")
            assertTypeParameter(second, "Out0")
          case other => fail(s"expected two Type parameters, found $other")
      case other => fail(s"expected one parameter-clause group, found $other")

    definition.decltpe match
      case Some(Type.Refine(Some(target), List(alias: Defn.Type))) =>
        assertTarget(target, "HasOut", "A")
        assertConcreteAlias(alias, "Out", "Out0")
      case other => fail(s"expected exact refined result, found $other")

  test("pins the parsed anonymous template parent and concrete alias field by field"):
    val definition = parse(Canonical)
    definition.body match
      case Term.NewAnonymous(template) =>
        assertEquals(template.early, Nil)
        assertEquals(template.derives, Nil)
        assert(template.self.isEmpty, clues(template.self))
        template.inits match
          case List(parent) =>
            assertEquals(parent.name.value, "")
            assertEquals(parent.argClauses, Nil)
            assertTarget(parent.tpe, "HasOut", "A")
          case other => fail(s"expected one parent, found $other")
        template.stats match
          case List(alias: Defn.Type) => assertConcreteAlias(alias, "Out", "Out0")
          case other => fail(s"expected one concrete Type alias, found $other")
      case other => fail(s"expected Term.NewAnonymous, found ${other.productPrefix}")

  private def assertTypeParameter(parameter: Type.Param, expectedName: String): Unit =
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.name.value, expectedName)
    assertEquals(parameter.tparamClause.values, Nil)
    assertEquals(parameter.bounds.lo, None)
    assertEquals(parameter.bounds.hi, None)
    assertEquals(parameter.bounds.context, Nil)
    assertEquals(parameter.bounds.view, Nil)

  private def assertTarget(value: Type, constructor: String, argument: String): Unit =
    value match
      case Type.Apply(name: Type.Name, List(reference: Type.Name)) =>
        assertEquals(name.value, constructor)
        assertEquals(reference.value, argument)
      case other => fail(s"expected direct unary target, found $other")

  private def assertConcreteAlias(alias: Defn.Type, member: String, rhs: String): Unit =
    assertEquals(alias.mods, Nil)
    assertEquals(alias.name.value, member)
    assertEquals(alias.tparamClause.values, Nil)
    assertEquals(alias.bounds.lo, None)
    assertEquals(alias.bounds.hi, None)
    assertEquals(alias.bounds.context, Nil)
    assertEquals(alias.bounds.view, Nil)
    alias.body match
      case name: Type.Name => assertEquals(name.value, rhs)
      case other => fail(s"expected direct Type.Name RHS, found $other")

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")
