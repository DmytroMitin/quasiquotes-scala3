package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class C011InstanceFactoryScalametaCharacterizationTest extends munit.FunSuite:
  private val CanonicalSource =
    """def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] {
      |  override def empty: A = emptyValue
      |  override def combine(a: A, a1: A): A = combineFunction(a, a1)
      |}""".stripMargin

  test("characterizes the outer definition and parameter Types field by field"):
    val definition = parseDefinition(CanonicalSource)
    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "instance")
    assertEquals(definition.paramClauseGroups.size, 1)
    val group = definition.paramClauseGroups.head
    group.tparamClause.values match
      case List(parameter) =>
        assertEquals(parameter.mods, Nil)
        assertEquals(parameter.name.value, "A")
        assertEquals(parameter.tparamClause.values, Nil)
        assertEquals(parameter.bounds.lo, None)
        assertEquals(parameter.bounds.hi, None)
        assertEquals(parameter.bounds.context, Nil)
        assertEquals(parameter.bounds.view, Nil)
      case other => fail(s"expected one Type parameter, found $other")

    group.paramClauses match
      case List(clause) =>
        assertEquals(clause.mod, None)
        clause.values match
          case List(emptyValue, combineFunction) =>
            assertParameterShell(emptyValue, "emptyValue")
            assertParameterShell(combineFunction, "combineFunction")
            emptyValue.decltpe match
              case Some(Type.ByName(name: Type.Name)) => assertEquals(name.value, "A")
              case other => fail(s"expected by-name A, found $other")
            combineFunction.decltpe match
              case Some(function: Type.Function) =>
                assertEquals(function.params.map(typeName), List("A", "A"))
                assertEquals(typeName(function.res), "A")
              case other => fail(s"expected binary Type.Function, found $other")
          case other => fail(s"expected two ordinary parameters, found $other")
      case other => fail(s"expected one ordinary value clause, found $other")

    definition.decltpe match
      case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        assertEquals(constructor.value, "Monoid")
        assertEquals(argument.value, "A")
      case other => fail(s"expected unary Monoid[A] result, found $other")

  test("characterizes the anonymous target and template topology field by field"):
    val definition = parseDefinition(CanonicalSource)
    definition.body match
      case anonymous: Term.NewAnonymous =>
        val template = anonymous.templ
        assertEquals(template.early, Nil)
        assertEquals(template.derives, Nil)
        assert(template.self.isEmpty, clues(template.self))
        template.inits match
          case List(parent) =>
            assertEquals(parent.name.value, "")
            assertEquals(parent.argClauses, Nil)
            parent.tpe match
              case Type.Apply(constructor: Type.Name, List(argument: Type.Name)) =>
                assertEquals(constructor.value, "Monoid")
                assertEquals(argument.value, "A")
              case other => fail(s"expected unary Monoid[A] parent, found $other")
          case other => fail(s"expected one anonymous parent, found $other")
        assertEquals(template.stats.size, 2)
        assert(template.stats.forall(_.isInstanceOf[Defn.Def]), clues(template.stats))
      case other => fail(s"expected Term.NewAnonymous, found ${other.productPrefix}")

  test("characterizes both ordered override members and their direct bodies"):
    val List(emptyMember: Defn.Def, combineMember: Defn.Def) =
      anonymousMembers(parseDefinition(CanonicalSource)): @unchecked

    assertOnlyOverride(emptyMember)
    assertEquals(emptyMember.name.value, "empty")
    assertEquals(emptyMember.paramClauseGroups, Nil)
    assertEquals(emptyMember.decltpe.map(typeName), Some("A"))
    emptyMember.body match
      case name: Term.Name => assertEquals(name.value, "emptyValue")
      case other => fail(s"expected direct empty-value name, found ${other.productPrefix}")

    assertOnlyOverride(combineMember)
    assertEquals(combineMember.name.value, "combine")
    combineMember.paramClauseGroups match
      case List(group) =>
        assertEquals(group.tparamClause.values, Nil)
        group.paramClauses match
          case List(clause) =>
            assertEquals(clause.mod, None)
            clause.values match
              case List(first, second) =>
                assertParameterShell(first, "a")
                assertParameterShell(second, "a1")
                assertEquals(first.decltpe.map(typeName), Some("A"))
                assertEquals(second.decltpe.map(typeName), Some("A"))
              case other => fail(s"expected two combine parameters, found $other")
          case other => fail(s"expected one combine clause, found $other")
      case other => fail(s"expected one combine parameter group, found $other")
    assertEquals(combineMember.decltpe.map(typeName), Some("A"))
    combineMember.body match
      case Term.Apply(callee: Term.Name, List(first: Term.Name, second: Term.Name)) =>
        assertEquals(callee.value, "combineFunction")
        assertEquals(first.value, "a")
        assertEquals(second.value, "a1")
      case other => fail(s"expected direct two-argument application, found $other")

  test("dynamic legal names preserve every structural category"):
    val renamed = parseDefinition(
      """def make[Element](fallbackValue: => Element, selection: (Element, Element) => Element): Choice[Element] = new Choice[Element] {
        |  override def fallback: Element = fallbackValue
        |  override def select(left: Element, right: Element): Element = selection(left, right)
        |}""".stripMargin
    )

    assertEquals(
      categorySignature(renamed),
      categorySignature(parseDefinition(CanonicalSource))
    )

  private def parseDefinition(source: String): Defn.Def =
    Input.String(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def categorySignature(definition: Defn.Def): List[String] =
    definition.collect {
      case tree: Tree => tree.productPrefix
    }

  private def anonymousMembers(definition: Defn.Def): List[Stat] =
    definition.body match
      case anonymous: Term.NewAnonymous => anonymous.templ.stats
      case other => fail(s"expected Term.NewAnonymous, found ${other.productPrefix}")

  private def assertOnlyOverride(definition: Defn.Def): Unit =
    definition.mods match
      case List(_: Mod.Override) => ()
      case other => fail(s"expected exactly Mod.Override, found $other")

  private def assertParameterShell(parameter: Term.Param, expectedName: String): Unit =
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.name.value, expectedName)
    assertEquals(parameter.default, None)

  private def typeName(value: Type): String =
    value match
      case name: Type.Name => name.value
      case other => fail(s"expected direct Type.Name, found ${other.productPrefix}")
