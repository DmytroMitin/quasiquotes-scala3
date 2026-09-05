package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaPrimitiveTypedDefinitionAuthoringDifferentialTest
    extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")

  test("immutable val gains a primitive Typed right-hand side only through Term delegation"):
    val shape = DefinitionShape
      .immutableVal(
        plain("answer"),
        intType,
        TermShape.Typed(TermShape.Identifier("value", false), "Int")
      )
      .toOption
      .get
    val direct = ScalametaTypedImmutableValAuthoring.author(shape).toOption.get
    val dispatched = dispatch(shape).asInstanceOf[Defn.Val]

    assertTypedBody(direct.rhs, "value")
    assertTypedBody(dispatched.rhs, "value")
    assertEquals(
      ScalametaTypedImmutableValProjection.project(direct),
      Right(ProjectedDefinitionShape(shape, None))
    )
    assertEquals(
      ScalametaTypedImmutableValProjection.project(dispatched),
      ScalametaTypedImmutableValProjection.project(direct)
    )
    assert(allTrees(direct).forall(_.pos == Position.None))
    assert(allTrees(dispatched).forall(_.pos == Position.None))

  test("parameterless def gains a primitive Typed body only through Term delegation"):
    val shape = DefinitionShape
      .parameterlessDef(
        plain("answer"),
        intType,
        TermShape.Typed(TermShape.Identifier("value", false), "Int")
      )
      .toOption
      .get
    val direct = ScalametaTypedParameterlessDefAuthoring.author(shape).toOption.get
    val dispatched = dispatch(shape).asInstanceOf[Defn.Def]

    assertEquals(direct.paramClauseGroups, Nil)
    assertTypedBody(direct.body, "value")
    assertTypedBody(dispatched.body, "value")
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(direct),
      Right(ProjectedDefinitionShape(shape, None))
    )
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(dispatched),
      ScalametaTypedParameterlessDefProjection.project(direct)
    )
    assert(allTrees(direct).forall(_.pos == Position.None))
    assert(allTrees(dispatched).forall(_.pos == Position.None))

  test("single-parameter def preserves Typed bound-reference alpha semantics through delegation"):
    val binder = BinderId(7)
    val shape = DefinitionShape
      .singleParameterDef(
        plain("id"),
        binder,
        plain("x"),
        intType,
        intType,
        TermShape.Typed(TermShape.BoundReference(binder, "stale"), "Int")
      )
      .toOption
      .get
    val direct = ScalametaTypedSingleParameterDefAuthoring.author(shape).toOption.get
    val dispatched = dispatch(shape).asInstanceOf[Defn.Def]

    assertTypedBody(direct.body, "x")
    assertTypedBody(dispatched.body, "x")
    val directProjection = ScalametaTypedSingleParameterDefProjection.project(direct)
    val dispatchedProjection = ScalametaTypedSingleParameterDefProjection.project(dispatched)
    assertEquals(directProjection, Right(ProjectedDefinitionShape(shape, None)))
    assertEquals(dispatchedProjection, directProjection)
    val projected = directProjection.toOption.get.shape.asInstanceOf[DefinitionShape.SingleParameterDef]
    assertEquals(projected.parameterBinderId, BinderId(0))
    assertEquals(
      projected.body,
      TermShape.Typed(TermShape.BoundReference(BinderId(0), "x"), "Int")
    )
    assert(allTrees(direct).forall(_.pos == Position.None))
    assert(allTrees(dispatched).forall(_.pos == Position.None))

  test("two-parameter def preserves the selected Typed binder through delegation"):
    val firstBinder = BinderId(7)
    val secondBinder = BinderId(2)
    val shape = DefinitionShape
      .twoParameterDef(
        plain("choose"),
        firstBinder,
        plain("x"),
        intType,
        secondBinder,
        plain("y"),
        intType,
        intType,
        TermShape.Typed(TermShape.BoundReference(secondBinder, "stale"), "Int")
      )
      .toOption
      .get
    val direct = ScalametaTypedTwoParameterDefAuthoring.author(shape).toOption.get
    val dispatched = dispatch(shape).asInstanceOf[Defn.Def]

    assertTypedBody(direct.body, "y")
    assertTypedBody(dispatched.body, "y")
    val directProjection = ScalametaTypedTwoParameterDefProjection.project(direct)
    val dispatchedProjection = ScalametaTypedTwoParameterDefProjection.project(dispatched)
    assertEquals(directProjection, Right(ProjectedDefinitionShape(shape, None)))
    assertEquals(dispatchedProjection, directProjection)
    val projected = directProjection.toOption.get.shape.asInstanceOf[DefinitionShape.TwoParameterDef]
    assertEquals(projected.firstParameterBinderId, BinderId(0))
    assertEquals(projected.secondParameterBinderId, BinderId(1))
    assertEquals(
      projected.body,
      TermShape.Typed(TermShape.BoundReference(BinderId(1), "y"), "Int")
    )
    assert(allTrees(direct).forall(_.pos == Position.None))
    assert(allTrees(dispatched).forall(_.pos == Position.None))

  private def assertTypedBody(body: Term, expressionName: String): Unit =
    val typed = body.asInstanceOf[Term.Ascribe]
    assertEquals(typed.expr.asInstanceOf[Term.Name].value, expressionName)
    assertEquals(typed.tpe.asInstanceOf[Type.Name].value, "Int")

  private def dispatch(shape: DefinitionShape): Defn =
    ScalametaDefinitionAuthoring.author(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.toString)

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
