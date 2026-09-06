package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaDelegatedForwardingMethodAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("fresh direct constructors expose the exact delegated-forwarding topology"):
    val definition = directForwarder("forward", "A", "value", "ctx", "Context", "Result")

    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "forward")
    definition.paramClauseGroups match
      case List(
            Member.ParamClauseGroup(
              Type.ParamClause(List(typeParameter)),
              List(ordinaryClause, contextualClause)
            )
          ) =>
        assertEquals(typeParameter.mods, Nil)
        assertEquals(typeParameter.name.value, "A")
        assertEquals(typeParameter.tparamClause.values, Nil)
        assertEmptyBounds(typeParameter.bounds)
        assertEquals(ordinaryClause.mod, None)
        ordinaryClause.values match
          case List(parameter) =>
            assertEquals(parameter.mods, Nil)
            assertEquals(parameter.name.value, "value")
            assertEquals(parameter.decltpe.map(_.asInstanceOf[Type.Name].value), Some("A"))
            assertEquals(parameter.default, None)
          case other => fail(s"expected one ordinary parameter, found $other")
        contextualClause.mod match
          case Some(_: Mod.Using) => ()
          case other => fail(s"expected one structural using clause, found $other")
        contextualClause.values match
          case List(parameter) =>
            assertEquals(parameter.mods, Nil)
            assertEquals(parameter.name.value, "ctx")
            assertEquals(parameter.default, None)
            parameter.decltpe match
              case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
                assertEquals(constructor.value, "Context")
                assertEquals(argument.value, "A")
              case other => fail(s"expected Context[A], found $other")
          case other => fail(s"expected one contextual parameter, found $other")
      case other => fail(s"expected one exact parameter-clause group, found $other")

    assertEquals(definition.decltpe.map(_.asInstanceOf[Type.Name].value), Some("Result"))
    definition.body match
      case Term.Apply(
            Term.Select(receiver: Term.Name, selected: Term.Name),
            List(argument: Term.Name)
          ) =>
        assertEquals(receiver.value, "ctx")
        assertEquals(selected.value, "forward")
        assertEquals(argument.value, "value")
      case other => fail(s"expected ctx.forward(value), found ${other.productPrefix}")

  test("the direct candidate is wholly unpositioned and projects canonical role binders"):
    val definition = directForwarder("forward", "A", "value", "ctx", "Context", "Result")

    assert(allTrees(definition).forall(_.pos == Position.None))
    val projected = ScalametaDelegatedForwardingMethodProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

    assertEquals(projected.sourceSpan, None)
    assertEquals(projected.plan.methodIdentity.sourceName, "forward")
    assertEquals(projected.plan.typeParameter, TypeParameter(BinderId(0), "A"))
    assertEquals(
      projected.plan.ordinaryParameter,
      OrdinaryParameter(BinderId(1), "value", TypeParameterReference(BinderId(0), "A"))
    )
    assertEquals(
      projected.plan.contextualParameter,
      ContextualParameter(
        BinderId(2),
        "ctx",
        Applied(SourceName("Context"), Vector(TypeParameterReference(BinderId(0), "A")))
      )
    )
    assertEquals(projected.plan.resultType, SourceName("Result"))
    assertEquals(projected.plan.body.receiver, ContextualReference(BinderId(2)))
    assert(projected.plan.methodIdentity eq projected.plan.body.selectedMethodIdentity)
    assertEquals(projected.plan.body.argument, OrdinaryReference(BinderId(1)))

  test("fresh shared name authoring decodes backticks that the 043 value projector cannot retain"):
    val plain = DefinitionName.fromSource("forward").fold(problem => fail(problem.message), identity)
    val backticked =
      DefinitionName.fromSource("`type`").fold(problem => fail(problem.message), identity)

    assertEquals(ScalametaTermDefinitionNameAuthoring.author(plain).map(_.value), Some("forward"))
    assertEquals(ScalametaTermDefinitionNameAuthoring.author(backticked).map(_.value), Some("type"))
    assertNotEquals(
      ScalametaTermDefinitionNameAuthoring.author(backticked).map(_.value),
      Some(backticked.source)
    )

  private def directForwarder(
      methodName: String,
      typeParameterName: String,
      ordinaryParameterName: String,
      contextualParameterName: String,
      contextualConstructorName: String,
      resultTypeName: String
  ): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name(typeParameterName),
      Type.ParamClause(Nil),
      Type.Bounds.empty
    )
    val ordinaryParameter = Term.Param(
      Nil,
      Term.Name(ordinaryParameterName),
      Some(Type.Name(typeParameterName)),
      None
    )
    val contextualParameter = Term.Param(
      Nil,
      Term.Name(contextualParameterName),
      Some(
        Type.Apply(
          Type.Name(contextualConstructorName),
          Type.ArgClause(List(Type.Name(typeParameterName)))
        )
      ),
      None
    )
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(List(typeParameter)),
          List(
            Term.ParamClause(List(ordinaryParameter)),
            Term.ParamClause(List(contextualParameter), Some(Mod.Using()))
          )
        )
      ),
      Some(Type.Name(resultTypeName)),
      Term.Apply(
        Term.Select(Term.Name(contextualParameterName), Term.Name(methodName)),
        Term.ArgClause(List(Term.Name(ordinaryParameterName)))
      )
    )

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
