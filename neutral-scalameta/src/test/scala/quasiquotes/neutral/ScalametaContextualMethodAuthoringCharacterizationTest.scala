package quasiquotes.neutral

import _root_.quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionConstruction}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaContextualMethodAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("fresh direct constructors expose the exact bounded contextual-method topology"):
    val definition = directContextualMethod()

    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "provide")
    definition.paramClauseGroups match
      case List(Member.ParamClauseGroup(Type.ParamClause(List(typeParameter)), List(clause))) =>
        assertEquals(typeParameter.mods, Nil)
        assertEquals(typeParameter.name.value, "A")
        assertEquals(typeParameter.tparamClause.values, Nil)
        assertEmptyBounds(typeParameter.bounds)
        clause.mod match
          case Some(_: Mod.Using) => ()
          case other => fail(s"expected one structural using clause, found $other")
        clause.values match
          case List(parameter) =>
            assertEquals(parameter.mods, Nil)
            assertEquals(parameter.name.value, "ctx")
            assertEquals(parameter.default, None)
            assertNestedApplication(parameter.decltpe, "Ctx", "List", "A")
          case other => fail(s"expected one contextual parameter, found $other")
      case other => fail(s"expected one exact parameter-clause group, found $other")

    assertNestedApplication(definition.decltpe, "Out", "Option", "A")
    definition.body match
      case name: Term.Name => assertEquals(name.value, "ctx")
      case other => fail(s"expected one direct Term.Name body, found ${other.productPrefix}")

    val leaves = allTrees(definition).collect { case name: Type.Name => name.value }
    assertEquals(leaves, List("A", "Ctx", "List", "A", "Out", "Option", "A"))

  test("the exact direct candidate is wholly unpositioned and reprojects without provenance"):
    val definition = directContextualMethod()
    val expected = DefinitionConstruction
      .contextualMethod(
        "provide",
        "A",
        "ctx",
        applied("Ctx", applied("List", typeParameter("A"))),
        applied("Out", applied("Option", typeParameter("A"))),
        reference("ctx")
      )
      .fold(problem => fail(problem.message), identity)

    assert(allTrees(definition).forall(_.pos == Position.None))
    assertEquals(
      ScalametaContextualMethodProjection.project(definition),
      Right(ProjectedContextualMethod(expected, None))
    )

  private def directContextualMethod(): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val contextualType = Type.Apply(
      Type.Name("Ctx"),
      Type.ArgClause(
        List(Type.Apply(Type.Name("List"), Type.ArgClause(List(Type.Name("A")))))
      )
    )
    val resultType = Type.Apply(
      Type.Name("Out"),
      Type.ArgClause(
        List(Type.Apply(Type.Name("Option"), Type.ArgClause(List(Type.Name("A")))))
      )
    )
    val contextualParameter =
      Term.Param(Nil, Term.Name("ctx"), Some(contextualType), None)
    Defn.Def(
      Nil,
      Term.Name("provide"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(Term.ParamClause(List(contextualParameter), Some(Mod.Using())))
        )
      ),
      Some(resultType),
      Term.Name("ctx")
    )

  private def assertNestedApplication(
      value: Option[Type],
      outer: String,
      inner: String,
      leaf: String
  ): Unit =
    value match
      case Some(
            Type.Apply(
              outerName: Type.Name,
              List(Type.Apply(innerName: Type.Name, List(leafName: Type.Name)))
            )
          ) =>
        assertEquals(outerName.value, outer)
        assertEquals(innerName.value, inner)
        assertEquals(leafName.value, leaf)
      case other => fail(s"expected $outer[$inner[$leaf]], found $other")

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)

  private def named(value: String): CompletedType =
    CompletedType.named(value).fold(problem => fail(problem.message), identity)

  private def typeParameter(value: String): CompletedType =
    CompletedType.typeParameter(value).fold(problem => fail(problem.message), identity)

  private def applied(constructor: String, argument: CompletedType): CompletedType =
    CompletedType
      .applied(named(constructor), Vector(argument))
      .fold(problem => fail(problem.message), identity)

  private def reference(value: String): CompletedTerm =
    CompletedTerm.reference(value).fold(problem => fail(problem.message), identity)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
