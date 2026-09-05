package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaInstanceFactoryAuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh direct constructors produce the exact N017 instance-factory topology"):
    val definition = directFactory()

    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "instance")
    definition.paramClauseGroups match
      case List(Member.ParamClauseGroup(Type.ParamClause(List(typeParameter)), List(clause))) =>
        assertEquals(typeParameter.mods, Nil)
        assertEquals(typeParameter.name.value, "A")
        assertEquals(typeParameter.tparamClause.values, Nil)
        assertEmptyBounds(typeParameter.bounds)
        assertEquals(clause.mod, None)
        clause.values match
          case List(emptyValue, combineFunction) =>
            assertParameterShell(emptyValue, "emptyValue")
            assertParameterShell(combineFunction, "combineFunction")
            emptyValue.decltpe match
              case Some(Type.ByName(name: Type.Name)) => assertEquals(name.value, "A")
              case other => fail(s"expected Type.ByName(Type.Name(A)), found $other")
            combineFunction.decltpe match
              case Some(function: Type.Function) =>
                assertEquals(function.params.map(typeName), List("A", "A"))
                assertEquals(typeName(function.res), "A")
              case other => fail(s"expected binary Type.Function, found $other")
          case other => fail(s"expected two ordinary outer parameters, found $other")
      case other => fail(s"expected one exact parameter-clause group, found $other")

    assertUnaryTarget(definition.decltpe, "Target", "A")
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
            assertUnaryTarget(Some(parent.tpe), "Target", "A")
          case other => fail(s"expected one parent, found $other")
        template.stats match
          case List(emptyMember: Defn.Def, combineMember: Defn.Def) =>
            assertEmptyMember(emptyMember)
            assertCombineMember(combineMember)
          case other => fail(s"expected two ordered Defn.Def members, found $other")
      case other => fail(s"expected Term.NewAnonymous, found ${other.productPrefix}")

  test("the exact fresh tree and every descendant are unpositioned and project without provenance"):
    val definition = directFactory()

    assert(allTrees(definition).forall(_.pos == Position.None))
    val projected = ScalametaInstanceFactoryProjection
      .project(definition)
      .fold(problem => fail(problem.message), identity)
    assertEquals(projected.sourceSpan, None)

  private def directFactory(): Defn.Def =
    val typeName = "A"
    val typeParameter = Type.Param(
      Nil,
      Type.Name(typeName),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val emptyValue = Term.Param(
      Nil,
      Term.Name("emptyValue"),
      Some(Type.ByName(Type.Name(typeName))),
      None
    )
    val combineFunction = Term.Param(
      Nil,
      Term.Name("combineFunction"),
      Some(Type.Function(List(Type.Name(typeName), Type.Name(typeName)), Type.Name(typeName))),
      None
    )
    val emptyMember = Defn.Def(
      List(Mod.Override()),
      Term.Name("emptyValueMember"),
      Nil,
      Some(Type.Name(typeName)),
      Term.Name("emptyValue")
    )
    val combineMember = Defn.Def(
      List(Mod.Override()),
      Term.Name("combineValues"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(
                Term.Param(Nil, Term.Name("x"), Some(Type.Name(typeName)), None),
                Term.Param(Nil, Term.Name("y"), Some(Type.Name(typeName)), None)
              )
            )
          )
        )
      ),
      Some(Type.Name(typeName)),
      Term.Apply(
        Term.Name("combineFunction"),
        Term.ArgClause(List(Term.Name("x"), Term.Name("y")))
      )
    )
    val target = Type.Apply(Type.Name("Target"), Type.ArgClause(List(Type.Name(typeName))))
    val template = Template(
      Nil,
      List(Init(target, Name.Anonymous(), List.empty[Term.ArgClause])),
      Self(Name.Anonymous(), None),
      List(emptyMember, combineMember),
      Nil
    )
    Defn.Def(
      Nil,
      Term.Name("instance"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(emptyValue, combineFunction)))
        )
      ),
      Some(Type.Apply(Type.Name("Target"), Type.ArgClause(List(Type.Name(typeName))))),
      Term.NewAnonymous(template)
    )

  private def assertEmptyMember(member: Defn.Def): Unit =
    assertOnlyOverride(member)
    assertEquals(member.name.value, "emptyValueMember")
    assertEquals(member.paramClauseGroups, Nil)
    assertEquals(member.decltpe.map(typeName), Some("A"))
    member.body match
      case name: Term.Name => assertEquals(name.value, "emptyValue")
      case other => fail(s"expected direct empty body name, found ${other.productPrefix}")

  private def assertCombineMember(member: Defn.Def): Unit =
    assertOnlyOverride(member)
    assertEquals(member.name.value, "combineValues")
    member.paramClauseGroups match
      case List(Member.ParamClauseGroup(Type.ParamClause(Nil), List(clause))) =>
        assertEquals(clause.mod, None)
        clause.values match
          case List(first, second) =>
            assertParameterShell(first, "x")
            assertParameterShell(second, "y")
            assertEquals(first.decltpe.map(typeName), Some("A"))
            assertEquals(second.decltpe.map(typeName), Some("A"))
          case other => fail(s"expected two nested parameters, found $other")
      case other => fail(s"expected one ordinary combine clause, found $other")
    assertEquals(member.decltpe.map(typeName), Some("A"))
    member.body match
      case Term.Apply(callee: Term.Name, List(first: Term.Name, second: Term.Name)) =>
        assertEquals(callee.value, "combineFunction")
        assertEquals(first.value, "x")
        assertEquals(second.value, "y")
      case other => fail(s"expected direct combine application, found $other")

  private def assertParameterShell(parameter: Term.Param, expectedName: String): Unit =
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.name.value, expectedName)
    assertEquals(parameter.default, None)

  private def assertUnaryTarget(value: Option[Type], constructor: String, argument: String): Unit =
    value match
      case Some(Type.Apply(name: Type.Name, List(reference: Type.Name))) =>
        assertEquals(name.value, constructor)
        assertEquals(reference.value, argument)
      case other => fail(s"expected $constructor[$argument], found $other")

  private def assertOnlyOverride(definition: Defn.Def): Unit =
    definition.mods match
      case List(_: Mod.Override) => ()
      case other => fail(s"expected exactly Mod.Override, found $other")

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)

  private def typeName(value: Type): String =
    value match
      case name: Type.Name => name.value
      case other => fail(s"expected direct Type.Name, found ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
