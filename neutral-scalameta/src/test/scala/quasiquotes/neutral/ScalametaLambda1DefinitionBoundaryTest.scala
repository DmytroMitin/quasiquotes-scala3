package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

final class ScalametaLambda1DefinitionBoundaryTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val lambda = TermShape.Lambda1(
    BinderId(7),
    "x",
    "Int",
    TermShape.BoundReference(BinderId(7), "x")
  )

  test("generic Lambda authoring does not widen any concrete Definition body family"):
    assert(ScalametaTermShapeAuthoring.author(lambda).isRight)

    assert(DefinitionShape.immutableVal(plain("value"), intType, lambda).isLeft)
    assert(DefinitionShape.parameterlessDef(plain("method"), intType, lambda).isLeft)
    assert(
      DefinitionShape
        .singleParameterDef(
          plain("single"),
          BinderId(0),
          plain("arg"),
          intType,
          intType,
          lambda
        )
        .isLeft
    )
    assert(
      DefinitionShape
        .twoParameterDef(
          plain("two"),
          BinderId(0),
          plain("left"),
          intType,
          BinderId(1),
          plain("right"),
          intType,
          intType,
          lambda
        )
        .isLeft
    )

    assertEquals(
      ScalametaTypedImmutableValAuthoring.author(null).left.toOption.map(_.code),
      Some("NEUTRAL_TYPED_VAL_AUTHORING_MISSING")
    )
    assertEquals(
      ScalametaTypedParameterlessDefAuthoring.author(null).left.toOption.map(_.code),
      Some("NEUTRAL_PARAMETERLESS_DEF_AUTHORING_MISSING")
    )
    assertEquals(
      ScalametaTypedSingleParameterDefAuthoring.author(null).left.toOption.map(_.code),
      Some("NEUTRAL_SINGLE_PARAMETER_DEF_AUTHORING_MISSING")
    )
    assertEquals(
      ScalametaTypedTwoParameterDefAuthoring.author(null).left.toOption.map(_.code),
      Some("NEUTRAL_TWO_PARAMETER_DEF_AUTHORING_MISSING")
    )

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get
