package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}

final class ScalametaP2LocalValDefinitionDifferentialTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val localId = BinderId(7)
  private val p2Body = TermShape.Block(
    List(
      BlockStatement.LocalVal(
        localId,
        "local",
        "Int",
        TermShape.Literal("1")
      )
    ),
    TermShape.BoundReference(localId, "stale")
  )

  test("generic P2 Term authoring does not widen existing Definition Core admission"):
    assert(DefinitionShape.immutableVal(plain("value"), intType, p2Body).isLeft)
    assert(DefinitionShape.parameterlessDef(plain("method"), intType, p2Body).isLeft)
    assert(
      DefinitionShape
        .singleParameterDef(
          plain("single"),
          BinderId(0),
          plain("arg"),
          intType,
          intType,
          p2Body
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
          p2Body
        )
        .isLeft
    )

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get
