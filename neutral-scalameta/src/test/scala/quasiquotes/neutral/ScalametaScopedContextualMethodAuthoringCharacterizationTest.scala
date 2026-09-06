package quasiquotes.neutral

import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaScopedContextualMethodAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("fresh direct constructors expose the exact scoped-037 refinement topology"):
    val definition = directScopedMethod(
      "scoped",
      "A",
      "B",
      "Bound",
      "ctx",
      "Context",
      "Out"
    )

    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "scoped")
    definition.paramClauseGroups match
      case List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(first, second)),
              List(contextualClause)
            )
          ) =>
        assertBoundedParameter(first, "A", "Bound")
        assertBoundedParameter(second, "B", "Bound")
        contextualClause.mod match
          case Some(_: Mod.Using) => ()
          case other => fail(s"expected one structural using clause, found $other")
        contextualClause.values match
          case List(parameter) =>
            assertEquals(parameter.mods, Nil)
            assertEquals(parameter.name.value, "ctx")
            assertEquals(parameter.default, None)
            parameter.decltpe match
              case Some(Type.Apply(constructor: Type.Name, List(first: Type.Name, second: Type.Name))) =>
                assertEquals(constructor.value, "Context")
                assertEquals(first.value, "A")
                assertEquals(second.value, "B")
              case other => fail(s"expected Context[A, B], found $other")
          case other => fail(s"expected one contextual parameter, found $other")
      case other => fail(s"expected one exact parameter-clause group, found $other")

    definition.decltpe match
      case Some(
            Type.Refine(
              Some(Type.Apply(constructor: Type.Name, List(first: Type.Name, second: Type.Name))),
              List(member: Defn.Type)
            )
          ) =>
        assertEquals(constructor.value, "Context")
        assertEquals(List(first.value, second.value), List("A", "B"))
        assertEquals(member.mods, Nil)
        assertEquals(member.name.value, "Out")
        assertEquals(member.tparamClause.values, Nil)
        assertEmptyBounds(member.bounds)
        member.body match
          case Type.Select(prefix: Term.Name, selected: Type.Name) =>
            assertEquals(prefix.value, "ctx")
            assertEquals(selected.value, "Out")
          case other => fail(s"expected ctx.Out, found $other")
      case other => fail(s"expected one exact refinement, found $other")

    definition.body match
      case name: Term.Name => assertEquals(name.value, "ctx")
      case other => fail(s"expected direct contextual body, found ${other.productPrefix}")

  test("the direct candidate is wholly unpositioned and projects canonical declaration roles"):
    val definition = directScopedMethod(
      "scoped",
      "A",
      "B",
      "Bound",
      "ctx",
      "Context",
      "Out"
    )

    assert(allTrees(definition).forall(_.pos == Position.None))
    val projected = ScalametaScopedContextualMethodProjection
      .project(definition)
      .fold(problem => fail(problem.message), identity)

    assertEquals(projected.sourceSpan, None)
    assertEquals(projected.plan.methodDisplayName, "scoped")
    assertEquals(
      projected.plan.typeParameters.map(parameter => (parameter.binderId, parameter.displayName, parameter.upperBound)),
      Vector(
        (BinderId(0), "A", SourceName("Bound")),
        (BinderId(1), "B", SourceName("Bound"))
      )
    )
    assertEquals(projected.plan.contextualTermBinderId, BinderId(2))
    assertEquals(projected.plan.contextualDisplayName, "ctx")
    assertEquals(projected.plan.typeArgumentBinderPositions, Vector(0, 1))
    assertEquals(projected.plan.contextualType.constructor, SourceName("Context"))
    assertEquals(projected.plan.contextualType, projected.plan.resultType.base)
    assertEquals(projected.plan.refinementMember.memberName, "Out")
    assertEquals(projected.plan.selectedResult, DirectStableSelected(BinderId(2), "Out"))
    assertEquals(projected.plan.bodyTermBinderId, BinderId(2))

    ScalametaContextualMethodDispatch.project(definition) match
      case Right(ProjectedContextualMethodRoute.Scoped037(value)) =>
        assertEquals(value.sourceSpan, None)
        assertEquals(value.plan.alphaKey, projected.plan.alphaKey)
      case other => fail(s"expected the Scoped037 dispatch route, found $other")

  private def directScopedMethod(
      methodName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String,
      upperBoundName: String,
      contextualParameterName: String,
      contextualConstructorName: String,
      memberName: String
  ): Defn.Def =
    val first = boundedTypeParameter(firstTypeParameterName, upperBoundName)
    val second = boundedTypeParameter(secondTypeParameterName, upperBoundName)
    val applied = Type.Apply(
      Type.Name(contextualConstructorName),
      Type.ArgClause(
        List(Type.Name(firstTypeParameterName), Type.Name(secondTypeParameterName))
      )
    )
    val member = Defn.Type(
      Nil,
      Type.Name(memberName),
      Type.ParamClause(Nil),
      Type.Select(Term.Name(contextualParameterName), Type.Name(memberName)),
      Type.Bounds.empty
    )
    val result = Type.Refine(Some(applied), Stat.Block(List(member)))
    val contextualParameter = Term.Param(
      Nil,
      Term.Name(contextualParameterName),
      Some(applied),
      None
    )
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(first, second)),
          List(Term.ParamClause(List(contextualParameter), Some(Mod.Using())))
        )
      ),
      Some(result),
      Term.Name(contextualParameterName)
    )

  private def boundedTypeParameter(name: String, upperBound: String): Type.Param =
    Type.Param(
      Nil,
      Type.Name(name),
      Type.ParamClause(Nil),
      Type.Bounds(None, Some(Type.Name(upperBound)), Nil, Nil)
    )

  private def assertBoundedParameter(
      parameter: Type.Param,
      expectedName: String,
      expectedUpperBound: String
  ): Unit =
    assertEquals(parameter.mods, Nil)
    assertEquals(parameter.name.value, expectedName)
    assertEquals(parameter.tparamClause.values, Nil)
    assertEquals(parameter.bounds.lo, None)
    assertEquals(parameter.bounds.hi.map(_.asInstanceOf[Type.Name].value), Some(expectedUpperBound))
    assertEquals(parameter.bounds.context, Nil)
    assertEquals(parameter.bounds.view, Nil)

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
