package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}

final class ScalametaP3LocalIdentityDefinitionDifferentialTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val methodId = BinderId(7)
  private val parameterId = BinderId(8)
  private val p3Body = TermShape.Block(
    List(
      BlockStatement.LocalDef(
        methodId,
        "identity",
        parameterId,
        "value",
        intType,
        intType,
        TermShape.BoundReference(parameterId, "stale")
      )
    ),
    TermShape.BoundReference(methodId, "stale")
  )

  test("generic P3 Term authoring does not widen existing Definition Core admission"):
    assert(DefinitionShape.immutableVal(plain("value"), intType, p3Body).isLeft)
    assert(DefinitionShape.parameterlessDef(plain("method"), intType, p3Body).isLeft)
    assert(
      DefinitionShape.singleParameterDef(
        plain("single"),
        BinderId(0),
        plain("arg"),
        intType,
        intType,
        p3Body
      ).isLeft
    )
    assert(
      DefinitionShape.twoParameterDef(
        plain("two"),
        BinderId(0),
        plain("left"),
        intType,
        BinderId(1),
        plain("right"),
        intType,
        intType,
        p3Body
      ).isLeft
    )

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get
