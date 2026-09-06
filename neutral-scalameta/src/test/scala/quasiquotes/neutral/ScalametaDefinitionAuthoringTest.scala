package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.{Defn, Position}

@nowarn("cat=deprecation")
final class ScalametaDefinitionAuthoringTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  test("dispatches a recursive backticked simple alias without changing family semantics"):
    val shape = DefinitionShape
      .simpleTypeAlias(
        DefinitionName.backticked("`type`").toOption.get,
        TypeShape.Apply(
          TypeShape.Identifier("Either"),
          List(
            TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
            TypeShape.Apply(TypeShape.Identifier("Option"), List(stringType))
          )
        )
      )
      .toOption
      .get

    val direct = ScalametaSimpleTypeAliasAuthoring.author(shape).toOption.get
    val dispatched = author(shape)

    assert(dispatched.isInstanceOf[Defn.Type])
    assertEquals(dispatched.pos, Position.None)
    assertEquals(
      ScalametaSimpleTypeAliasProjection.project(dispatched.asInstanceOf[Defn.Type]),
      ScalametaSimpleTypeAliasProjection.project(direct)
    )
    assertEquals(
      ScalametaSimpleTypeAliasProjection.project(dispatched.asInstanceOf[Defn.Type]),
      Right(ProjectedDefinitionShape(shape, None))
    )
    val projected = ScalametaSimpleTypeAliasProjection
      .project(dispatched.asInstanceOf[Defn.Type])
      .toOption
      .get
      .shape
      .asInstanceOf[DefinitionShape.SimpleTypeAlias]
    assertEquals(projected.name.source, "`type`")

  test("dispatches an immutable val without changing Type Term or position semantics"):
    val recursiveType = TypeShape.Apply(
      TypeShape.Identifier("Either"),
      List(
        TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
        TypeShape.Apply(TypeShape.Identifier("Option"), List(stringType))
      )
    )
    val shape = immutable(
      plain("answer"),
      recursiveType,
      TermShape.Infix(
        TermShape.Identifier("left", false),
        "+",
        TermShape.Identifier("right", false)
      )
    )
    val direct = ScalametaTypedImmutableValAuthoring.author(shape).toOption.get
    val dispatched = author(shape)
    val projected = ScalametaTypedImmutableValProjection
      .project(dispatched.asInstanceOf[Defn.Val])
      .toOption
      .get
      .shape
      .asInstanceOf[DefinitionShape.ImmutableVal]

    assert(dispatched.isInstanceOf[Defn.Val])
    assertEquals(dispatched.pos, Position.None)
    assertEquals(
      ScalametaTypedImmutableValProjection.project(dispatched.asInstanceOf[Defn.Val]),
      ScalametaTypedImmutableValProjection.project(direct)
    )
    assertEquals(
      ScalametaTypedImmutableValProjection.project(dispatched.asInstanceOf[Defn.Val]),
      Right(ProjectedDefinitionShape(shape, None))
    )
    assertEquals(projected.declaredType, recursiveType)

  test("dispatches a true parameterless def without adding an ordinary parameter clause"):
    val shape = parameterless(
      plain("answer"),
      intType,
      TermShape.Select(TermShape.Identifier("service", false), "answer")
    )
    val direct = ScalametaTypedParameterlessDefAuthoring.author(shape).toOption.get
    val dispatched = author(shape)

    assert(dispatched.isInstanceOf[Defn.Def])
    assertEquals(dispatched.asInstanceOf[Defn.Def].paramClauseGroups, Nil)
    assertEquals(dispatched.pos, Position.None)
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(dispatched.asInstanceOf[Defn.Def]),
      ScalametaTypedParameterlessDefProjection.project(direct)
    )
    assertEquals(
      ScalametaTypedParameterlessDefProjection.project(dispatched.asInstanceOf[Defn.Def]),
      Right(ProjectedDefinitionShape(shape, None))
    )

  test("dispatches a single-parameter def while leaving BinderId normalization to its family"):
    val inputBinder = BinderId(7)
    val shape = single(
      plain("identity"),
      inputBinder,
      plain("value"),
      intType,
      intType,
      TermShape.BoundReference(inputBinder, "stale")
    )
    val direct = ScalametaTypedSingleParameterDefAuthoring.author(shape).toOption.get
    val dispatched = author(shape)
    val projected = ScalametaTypedSingleParameterDefProjection
      .project(dispatched.asInstanceOf[Defn.Def])
      .toOption
      .get

    assert(dispatched.isInstanceOf[Defn.Def])
    assertEquals(dispatched.pos, Position.None)
    assertEquals(
      ScalametaTypedSingleParameterDefProjection.project(dispatched.asInstanceOf[Defn.Def]),
      ScalametaTypedSingleParameterDefProjection.project(direct)
    )
    assertEquals(projected.shape, shape)
    assertEquals(
      projected.shape.asInstanceOf[DefinitionShape.SingleParameterDef].parameterName,
      plain("value")
    )
    assertEquals(
      projected.shape.asInstanceOf[DefinitionShape.SingleParameterDef].parameterBinderId,
      BinderId(0)
    )
    assertEquals(projected.sourceSpan, None)

  test("dispatches a two-parameter def with source order distinct Types and reverse references"):
    val firstBinder = BinderId(9)
    val secondBinder = BinderId(2)
    val shape = two(
      plain("pair"),
      firstBinder,
      plain("left"),
      intType,
      secondBinder,
      plain("right"),
      stringType,
      TypeShape.Tuple(List(stringType, intType)),
      TermShape.Tuple(
        List(
          TermShape.BoundReference(secondBinder, "stale-right"),
          TermShape.BoundReference(firstBinder, "stale-left")
        )
      )
    )
    val direct = ScalametaTypedTwoParameterDefAuthoring.author(shape).toOption.get
    val dispatched = author(shape)
    val projected = ScalametaTypedTwoParameterDefProjection
      .project(dispatched.asInstanceOf[Defn.Def])
      .toOption
      .get
    val projectedShape = projected.shape.asInstanceOf[DefinitionShape.TwoParameterDef]

    assert(dispatched.isInstanceOf[Defn.Def])
    assertEquals(dispatched.pos, Position.None)
    assertEquals(
      ScalametaTypedTwoParameterDefProjection.project(dispatched.asInstanceOf[Defn.Def]),
      ScalametaTypedTwoParameterDefProjection.project(direct)
    )
    assertEquals(projectedShape, shape)
    assertEquals(projectedShape.firstParameterBinderId, BinderId(0))
    assertEquals(projectedShape.secondParameterBinderId, BinderId(1))
    assertEquals(projectedShape.firstParameterName, plain("left"))
    assertEquals(projectedShape.secondParameterName, plain("right"))
    assertEquals(projectedShape.firstParameterType, intType)
    assertEquals(projectedShape.secondParameterType, stringType)
    assertEquals(projectedShape.resultType, TypeShape.Tuple(List(stringType, intType)))
    assertEquals(
      projectedShape.body,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(BinderId(1), "right"),
          TermShape.BoundReference(BinderId(0), "left")
        )
      )
    )
    assertEquals(projected.sourceSpan, None)

  test("rejects a missing root at the dispatcher boundary"):
    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(null),
      Left(ScalametaDefinitionAuthoring.ShapeError.Missing)
    )

  test("preserves the exact delegated simple-alias error object and family"):
    val shape = DefinitionShape.simpleTypeAlias(null, intType).toOption.get
    val problem = ScalametaSimpleTypeAliasAuthoring.author(shape).left.toOption.get

    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(shape),
      Left(ScalametaDefinitionAuthoring.ShapeError.SimpleTypeAlias(problem))
    )

  test("preserves the exact delegated immutable-val error object and family"):
    val shape = immutable(
      plain("answer"),
      intType,
      TermShape.Parenthesized(TermShape.Identifier("source", false))
    )
    val problem = ScalametaTypedImmutableValAuthoring.author(shape).left.toOption.get

    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(shape),
      Left(ScalametaDefinitionAuthoring.ShapeError.ImmutableVal(problem))
    )

  test("preserves the exact delegated parameterless-def error object and family"):
    val shape = parameterless(
      plain("answer"),
      intType,
      TermShape.Identifier("answer", false)
    )
    val problem = ScalametaTypedParameterlessDefAuthoring.author(shape).left.toOption.get

    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(shape),
      Left(ScalametaDefinitionAuthoring.ShapeError.ParameterlessDef(problem))
    )

  test("preserves the exact delegated single-parameter-def error object and family"):
    val shape = single(
      plain("identity"),
      BinderId(7),
      plain("value"),
      intType,
      intType,
      TermShape.Identifier("value", false)
    )
    val problem = ScalametaTypedSingleParameterDefAuthoring.author(shape).left.toOption.get

    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(shape),
      Left(ScalametaDefinitionAuthoring.ShapeError.SingleParameterDef(problem))
    )

  test("preserves the exact delegated two-parameter-def error object and family"):
    val shape = two(
      plain("pair"),
      BinderId(9),
      plain("left"),
      intType,
      BinderId(2),
      plain("right"),
      intType,
      booleanType,
      TermShape.Identifier("right", false)
    )
    val problem = ScalametaTypedTwoParameterDefAuthoring.author(shape).left.toOption.get

    assertEquals(
      ScalametaDefinitionAuthoring.authorShape(shape),
      Left(ScalametaDefinitionAuthoring.ShapeError.TwoParameterDef(problem))
    )

  test("does not collapse or cross-classify family failures as Missing"):
    val aliasProblem = ScalametaSimpleTypeAliasAuthoring
      .author(DefinitionShape.simpleTypeAlias(null, intType).toOption.get)
      .left
      .toOption
      .get
    val valueShape = immutable(
      plain("answer"),
      intType,
      TermShape.Parenthesized(TermShape.Identifier("source", false))
    )

    assertNotEquals(
      ScalametaDefinitionAuthoring.authorShape(valueShape),
      Left(ScalametaDefinitionAuthoring.ShapeError.SimpleTypeAlias(aliasProblem))
    )
    assertNotEquals(
      ScalametaDefinitionAuthoring.authorShape(valueShape),
      Left(ScalametaDefinitionAuthoring.ShapeError.Missing)
    )

  private def author(shape: DefinitionShape): Defn =
    ScalametaDefinitionAuthoring.authorShape(shape) match
      case Right(value) => value
      case Left(problem) => fail(problem.toString)

  private def immutable(
      name: DefinitionName,
      declaredType: TypeShape,
      rhs: TermShape
  ): DefinitionShape.ImmutableVal =
    DefinitionShape.immutableVal(name, declaredType, rhs).toOption.get

  private def parameterless(
      name: DefinitionName,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.ParameterlessDef =
    DefinitionShape.parameterlessDef(name, resultType, body).toOption.get

  private def single(
      methodName: DefinitionName,
      binderId: BinderId,
      parameterName: DefinitionName,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.SingleParameterDef =
    DefinitionShape
      .singleParameterDef(
        methodName,
        binderId,
        parameterName,
        parameterType,
        resultType,
        body
      )
      .toOption
      .get

  private def two(
      methodName: DefinitionName,
      firstBinderId: BinderId,
      firstName: DefinitionName,
      firstType: TypeShape,
      secondBinderId: BinderId,
      secondName: DefinitionName,
      secondType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.TwoParameterDef =
    DefinitionShape
      .twoParameterDef(
        methodName,
        firstBinderId,
        firstName,
        firstType,
        secondBinderId,
        secondName,
        secondType,
        resultType,
        body
      )
      .toOption
      .get

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get
