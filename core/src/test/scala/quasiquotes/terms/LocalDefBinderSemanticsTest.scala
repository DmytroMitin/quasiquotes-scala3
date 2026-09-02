package quasiquotes.terms

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.types.TypeNormalForm

class LocalDefBinderSemanticsTest extends munit.FunSuite:
  private def shape(methodId: Int, parameterId: Int, methodName: String, parameterName: String): TermShape =
    TermShape.Block(
      List(
        BlockStatement.LocalDef(
          BinderId(methodId),
          methodName,
          BinderId(parameterId),
          parameterName,
          TypeShape.Identifier("Int"),
          TypeShape.Identifier("Int"),
          TermShape.BoundReference(BinderId(parameterId), parameterName)
        )
      ),
      TermShape.Apply(
        TermShape.BoundReference(BinderId(methodId), methodName),
        List(TermShape.Literal("1"))
      )
    )

  test("alpha normalization preserves distinct method and parameter binding roles") {
    val first = TermShapeTraversal.alphaNormalize(shape(7, 8, "id", "value"))
    val renamed = TermShapeTraversal.alphaNormalize(shape(20, 21, "renamed", "argument"))
    assertEquals(first, renamed)
    first match
      case TermShape.Block(List(local: BlockStatement.LocalDef), result) =>
        assertNotEquals(local.methodBinderId, local.parameterBinderId)
        assertEquals(local.body, TermShape.BoundReference(local.parameterBinderId, ""))
        assertEquals(
          result,
          TermShape.Apply(
            TermShape.BoundReference(local.methodBinderId, ""),
            List(TermShape.Literal("1"))
          )
        )
      case other => fail(s"unexpected normalized local-def shape: ${other.render}")
  }

  test("scope validation rejects a body reference to the method binder and duplicate binder identities") {
    val methodInBody = shape(0, 1, "id", "value") match
      case TermShape.Block(List(local: BlockStatement.LocalDef), result) =>
        TermShape.Block(
          List(local.copy(body = TermShape.BoundReference(local.methodBinderId, "id"))),
          result
        )
      case other => fail(other.render)
    val duplicate = shape(2, 2, "id", "value")

    assert(TermShapeTraversal.validateSupported(methodInBody).isLeft)
    assert(TermShapeTraversal.validateSupported(duplicate).isLeft)
  }

  test("completion derives local-def sidecars from structured type evidence") {
    val localDef = shape(7, 8, "id", "value")
    val completed = ConstructedTerm.fromShape(localDef)

    assertEquals(TypeShape.Identifier("Int").render, "TypeIdent(Int)")
    assertEquals(TermShapeTraversal.typedNames(localDef), Vector("Int", "Int"))

    assertEquals(
      completed.map(_.ascriptionTypes),
      Right(
        Vector(
          TypeNormalForm.STypeIdent("Int"),
          TypeNormalForm.STypeIdent("Int")
        )
      )
    )
  }

  test("completion preserves existing structured applied-type authority for local defs") {
    val parameterType = TypeShape.Apply(
      TypeShape.Identifier("Option"),
      List(TypeShape.Identifier("Int"))
    )
    val resultType = TypeShape.Apply(
      TypeShape.Identifier("Either"),
      List(TypeShape.Identifier("String"), TypeShape.Identifier("Boolean"))
    )
    val localDef = shape(7, 8, "id", "value") match
      case TermShape.Block(List(local: BlockStatement.LocalDef), result) =>
        TermShape.Block(
          List(local.copy(parameterType = parameterType, resultType = resultType)),
          result
        )
      case other => fail(other.render)

    assertEquals(
      ConstructedTerm.fromShape(localDef).map(_.ascriptionTypes),
      Right(
        Vector(
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent("Option"),
            List(TypeNormalForm.STypeIdent("Int"))
          ),
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent("Either"),
            List(
              TypeNormalForm.STypeIdent("String"),
              TypeNormalForm.STypeIdent("Boolean")
            )
          )
        )
      )
    )
  }
