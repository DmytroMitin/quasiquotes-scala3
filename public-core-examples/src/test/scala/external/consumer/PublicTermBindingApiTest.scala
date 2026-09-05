package external.consumer

import quasiquotes.parser.TermShape
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.compiletime.testing.typeCheckErrors

final class PublicTermBindingApiTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")

  test("foreign callers build and inspect an explicitly bound lambda"):
    val shape = identityLambda("x")
    val view = inspect(shape)
    val lambda = view.lambda.getOrElse(fail("expected lambda view"))
    val parameter = lambda.parameters.head
    val reference = inspect(lambda.body).boundReference
      .getOrElse(fail("expected bound-reference view"))

    assertEquals(view.category.code, "lambda")
    assertEquals(parameter.name, "x")
    assertEquals(parameter.declaredType, intType)
    assertEquals(reference.binder, parameter.binder)
    assertEquals(inspect(shape).lambda.get.parameters.head.binder, parameter.binder)

  test("same-spelling identifiers remain free unless reference is explicit"):
    val shape = right(
      TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { _ =>
        Right(TermShape.Identifier("x", isPlaceholder = false))
      }
    )
    val lambda = inspect(shape).lambda.get
    val body = inspect(lambda.body)

    assertEquals(body.category.code, "ordinary")
    assertEquals(body.boundReference, None)
    assertEquals(lambda.body, TermShape.Identifier("x", isPlaceholder = false))

  test("independent graphs use distinct handles while retaining alpha semantics"):
    val first = identityLambda("x")
    val renamed = identityLambda("renamed")
    val firstBinder = inspect(first).lambda.get.parameters.head.binder
    val renamedBinder = inspect(renamed).lambda.get.parameters.head.binder

    assertNotEquals(firstBinder, renamedBinder)
    assertEquals(first.render, "Lambda1(x: Int, BoundRef(x))")
    assertEquals(renamed.render, "Lambda1(renamed: Int, BoundRef(renamed))")

  test("local value keeps its rhs outside scope and binds its continuation"):
    val shape = right(
      TermShapeBindings.localValue(
        "x",
        intType,
        TermShape.Identifier("x", isPlaceholder = false)
      ) { scope => scope.reference(scope.declaredBinder.get) }
    )
    val block = inspect(shape).block.getOrElse(fail("expected block view"))
    val local = block.locals.head
    val result = inspect(block.result).boundReference.get

    assertEquals(local.kind.code, "immutable-value")
    assertEquals(local.name, "x")
    assertEquals(local.parameterClauses, Vector.empty)
    assertEquals(local.resultType, intType)
    assertEquals(local.body, Some(TermShape.Identifier("x", false)))
    assertEquals(result.binder, local.binder)

  test("local method gives only its parameter to the body and itself to continuation"):
    val shape = right(
      TermShapeBindings.localMethod(
        "id",
        Vector(Vector(TermParameterSpec("x", intType))),
        intType
      )(
        bodyScope =>
          assertEquals(bodyScope.declaredBinder, None)
          bodyScope.reference(bodyScope.parameterBinders.head.head)
      )(
        continuationScope =>
          assertEquals(continuationScope.parameterBinders, Vector.empty)
          continuationScope.reference(continuationScope.declaredBinder.get)
      )
    )
    val block = inspect(shape).block.get
    val local = block.locals.head
    val parameter = local.parameterClauses.head.head

    assertEquals(local.kind.code, "method")
    assertEquals(local.name, "id")
    assertEquals(local.resultType, intType)
    assertEquals(
      inspect(local.body.get).boundReference.get.binder,
      parameter.binder
    )
    assertEquals(inspect(block.result).boundReference.get.binder, local.binder)
    assertNotEquals(parameter.binder, local.binder)

  test("nested builders retain the live outer lexical scope"):
    val shape = right(
      TermShapeBindings.lambda(Vector(TermParameterSpec("outer", intType))) { outer =>
        for
          rhs <- outer.reference(outer.parameterBinders.head.head)
          block <- TermShapeBindings.localValue("inner", intType, rhs) { inner =>
            inner.reference(inner.declaredBinder.get)
          }
        yield block
      }
    )
    val lambda = inspect(shape).lambda.get
    val outerBinder = lambda.parameters.head.binder
    val block = inspect(lambda.body).block.get

    assertEquals(
      inspect(block.locals.head.body.get).boundReference.get.binder,
      outerBinder
    )
    assertEquals(
      inspect(block.result).boundReference.get.binder,
      block.locals.head.binder
    )

  test("foreign and escaped binders fail closed with stable codes"):
    var captured: Option[(TermBindingScope, TermBinder)] = None
    identityLambda("first", scope => captured = Some(scope -> scope.parameterBinders.head.head))
    val (escapedScope, foreignBinder) = captured.get

    assertEquals(
      escapedScope.reference(foreignBinder).left.toOption.map(_.code),
      Some("TERM_BINDER_UNBOUND")
    )

    val foreignFailure = right(
      TermShapeBindings.lambda(Vector(TermParameterSpec("second", intType))) { scope =>
        scope.reference(foreignBinder) match
          case Left(failure) =>
            assertEquals(failure.code, "TERM_BINDER_SCOPE_MISMATCH")
            scope.reference(scope.parameterBinders.head.head)
          case Right(_) => fail("foreign binder must be rejected")
      }
    )
    assertEquals(inspect(foreignFailure).category.code, "lambda")

    val foreignGraph = identityLambda("foreign")
    assertFailure(
      TermShapeBindings.lambda(Vector(TermParameterSpec("current", intType))) { _ =>
        Right(
          TermShape.Apply(
            TermShape.Identifier("consume", false),
            List(foreignGraph)
          )
        )
      },
      "TERM_BINDER_SCOPE_MISMATCH"
    )

  test("unsupported topology and malformed inputs return stable failures"):
    assertFailure(
      TermShapeBindings.lambda(Vector.empty)(_ => Right(TermShape.Literal("0"))),
      "TERM_BINDING_UNSUPPORTED"
    )
    assertFailure(
      TermShapeBindings.lambda(null)(_ => Right(TermShape.Literal("0"))),
      "TERM_BINDING_MISSING"
    )
    assertFailure(
      TermShapeBindings.lambda(
        Vector(TermParameterSpec("x", intType), TermParameterSpec("y", intType))
      )(_ => Right(TermShape.Literal("0"))),
      "TERM_BINDING_UNSUPPORTED"
    )
    assertFailure(
      TermShapeBindings.lambda(Vector(TermParameterSpec("bad-name", intType)))(_ =>
        Right(TermShape.Literal("0"))
      ),
      "TERM_BINDING_INVALID_NAME"
    )
    assertFailure(
      TermShapeBindings.localValue("x", intType, null)(_ => Right(TermShape.Literal("0"))),
      "TERM_BINDING_MISSING"
    )
    assertFailure(
      TermShapeBindings.localMethod("id", null, intType)(_ => Right(TermShape.Literal("0")))(
        _ => Right(TermShape.Literal("0"))
      ),
      "TERM_BINDING_MISSING"
    )
    assertFailure(
      TermShapeBindingView.inspect(null),
      "TERM_BINDING_MISSING"
    )

  test("failure message combines its stable code and detail"):
    val failure = TermBindingFailure("TERM_BINDING_INVALID_BODY", "body was absent")
    assertEquals(failure.message, "TERM_BINDING_INVALID_BODY: body was absent")

  test("opaque handles and views have no foreign-callable constructors"):
    assert(
      typeCheckErrors(
        "new quasiquotes.terms.TermBinder(null, null)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "new quasiquotes.terms.TermBindingView(null, None, None, None)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "new quasiquotes.terms.TermBindingCategory(\"invented\")"
      ).nonEmpty
    )

  private def identityLambda(
      name: String,
      observe: TermBindingScope => Unit = _ => ()
  ): TermShape =
    right(
      TermShapeBindings.lambda(Vector(TermParameterSpec(name, intType))) { scope =>
        observe(scope)
        scope.reference(scope.parameterBinders.head.head)
      }
    )

  private def inspect(shape: TermShape): TermBindingView =
    right(TermShapeBindingView.inspect(shape))

  private def right[A](value: Either[TermBindingFailure, A]): A =
    value.fold(failure => fail(failure.message), identity)

  private def assertFailure[A](
      value: Either[TermBindingFailure, A],
      expectedCode: String
  ): Unit =
    assertEquals(value.left.toOption.map(_.code), Some(expectedCode))
