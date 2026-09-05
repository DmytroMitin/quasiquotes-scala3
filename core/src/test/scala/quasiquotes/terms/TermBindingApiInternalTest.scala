package quasiquotes.terms

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.types.TypeNormalForm

final class TermBindingApiInternalTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")

  test("inspection rejects unbound private references and binder collisions"):
    assertFailure(
      TermShapeBindingView.inspect(TermShape.BoundReference(BinderId(7), "x")),
      "TERM_BINDER_UNBOUND"
    )

    val collidingLambda = TermShape.Lambda1(
      BinderId(0),
      "outer",
      "Int",
      TermShape.Lambda1(
        BinderId(0),
        "inner",
        "Int",
        TermShape.BoundReference(BinderId(0), "inner")
      )
    )
    assertFailure(
      TermShapeBindingView.inspect(collidingLambda),
      "TERM_BINDER_COLLISION"
    )

    val collidingMethod = TermShape.Block(
      List(
        BlockStatement.LocalDef(
          BinderId(0),
          "id",
          BinderId(0),
          "x",
          TypeShape.Identifier("Int"),
          TypeShape.Identifier("Int"),
          TermShape.BoundReference(BinderId(0), "x")
        )
      ),
      TermShape.BoundReference(BinderId(0), "id")
    )
    assertFailure(
      TermShapeBindingView.inspect(collidingMethod),
      "TERM_BINDER_COLLISION"
    )

    val repeatedSiblingIdentity = TermShape.Tuple(
      List(
        TermShape.Lambda1(BinderId(0), "first", "Int", TermShape.Literal("1")),
        TermShape.Lambda1(BinderId(0), "second", "Int", TermShape.Literal("2"))
      )
    )
    assertFailure(
      TermShapeBindingView.inspect(repeatedSiblingIdentity),
      "TERM_BINDER_COLLISION"
    )

  test("inspection rejects malformed private children and unsupported nodes"):
    assertFailure(
      TermShapeBindingView.inspect(
        TermShape.Lambda1(BinderId(0), "x", "Int", null)
      ),
      "TERM_BINDING_MISSING"
    )
    assertFailure(
      TermShapeBindingView.inspect(TermShape.Unsupported("future", "not admitted")),
      "TERM_BINDING_UNSUPPORTED"
    )

    assertFailure(
      TermShapeBindings.lambda(
        Vector(
          TermParameterSpec(
            "x",
            TypeNormalForm.STypeApply(null, List(intType))
          )
        )
      )(_ => Right(TermShape.Literal("0"))),
      "TERM_BINDING_INVALID_TYPE"
    )

  test("inspection recovers current applied P2 normal forms"):
    val shape = TermShape.Block(
      List(
        BlockStatement.LocalVal(
          BinderId(0),
          "xs",
          "List[Int]",
          TermShape.Identifier("source", false)
        )
      ),
      TermShape.BoundReference(BinderId(0), "xs")
    )
    val local = inspect(shape).block.get.locals.head

    assertEquals(
      local.resultType,
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(TypeNormalForm.STypeIdent("Int"))
      )
    )

  test("public builders preserve existing alpha-aware ConstructedTerm semantics"):
    val first = constructed(identityLambda("x"))
    val renamed = constructed(identityLambda("renamed"))
    assertEquals(first, renamed)
    assertEquals(first.hashCode, renamed.hashCode)

    val bound = identityLambda("x")
    val free = right(
      TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { _ =>
        Right(TermShape.Identifier("x", false))
      }
    )
    assertNotEquals(constructed(bound), constructed(free))

  test("same-spelling references to different nested binders remain distinct"):
    val outerReference = TermShape.Lambda1(
      BinderId(0),
      "x",
      "Int",
      TermShape.Lambda1(
        BinderId(1),
        "x",
        "Int",
        TermShape.BoundReference(BinderId(0), "x")
      )
    )
    val innerReference = TermShape.Lambda1(
      BinderId(0),
      "x",
      "Int",
      TermShape.Lambda1(
        BinderId(1),
        "x",
        "Int",
        TermShape.BoundReference(BinderId(1), "x")
      )
    )

    assertNotEquals(constructed(outerReference), constructed(innerReference))

  test("a completed nested declaration is not live in its enclosing scope"):
    val result =
      TermShapeBindings.lambda(Vector(TermParameterSpec("outer", intType))) { outer =>
        var innerBinder: Option[TermBinder] = None
        TermShapeBindings.localValue("inner", intType, TermShape.Literal("1")) { inner =>
          innerBinder = inner.declaredBinder
          inner.reference(inner.declaredBinder.get)
        }.flatMap(_ => outer.reference(innerBinder.get))
      }
    assertFailure(result, "TERM_BINDER_UNBOUND")

  private def identityLambda(name: String): TermShape =
    right(
      TermShapeBindings.lambda(Vector(TermParameterSpec(name, intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
    )

  private def inspect(shape: TermShape): TermBindingView =
    right(TermShapeBindingView.inspect(shape))

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def right[A](value: Either[TermBindingFailure, A]): A =
    value.fold(failure => fail(failure.message), identity)

  private def assertFailure[A](
      value: Either[TermBindingFailure, A],
      expectedCode: String
  ): Unit =
    assertEquals(value.left.toOption.map(_.code), Some(expectedCode))
