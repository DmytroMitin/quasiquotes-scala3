package external.consumer

import quasiquotes.neutral.{ScalametaTermProjection, ScalametaTermShapeAuthoring}
import quasiquotes.parser.TermShape
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class PublicTermBindingProjectionIntegrationTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")

  test("foreign consumer correlates projected lambda declaration and reference"):
    val shape = project("(x: Int) => x")
    val lambda = inspect(shape).lambda.get

    assertEquals(lambda.parameters.map(_.name), Vector("x"))
    assertEquals(
      inspect(lambda.body).boundReference.get.binder,
      lambda.parameters.head.binder
    )

  test("foreign consumer correlates projected P2 declaration and result"):
    val block = inspect(project("{ val x: Int = 1; x }")).block.get
    val local = block.locals.head

    assertEquals(local.kind.code, "immutable-value")
    assertEquals(local.body, Some(TermShape.Literal("1")))
    assertEquals(inspect(block.result).boundReference.get.binder, local.binder)

  test("foreign consumer correlates projected P3 method and parameter scopes"):
    val block = inspect(project("{ def id(x: Int): Int = x; id }")).block.get
    val local = block.locals.head
    val parameter = local.parameterClauses.head.head

    assertEquals(local.kind.code, "method")
    assertEquals(inspect(local.body.get).boundReference.get.binder, parameter.binder)
    assertEquals(inspect(block.result).boundReference.get.binder, local.binder)

  test("public lambda, P2, and P3 builders feed the unchanged neutral authoring entry point"):
    val lambda = right(
      TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
    )
    val local = right(
      TermShapeBindings.localValue("x", intType, TermShape.Literal("1")) { scope =>
        scope.reference(scope.declaredBinder.get)
      }
    )
    val method = right(
      TermShapeBindings.localMethod(
        "id",
        Vector(Vector(TermParameterSpec("x", intType))),
        intType
      )(
        scope => scope.reference(scope.parameterBinders.head.head)
      )(
        scope => scope.reference(scope.declaredBinder.get)
      )
    )

    assertEquals(author(lambda).syntax, "(x: Int) => x")
    assertEquals(author(local).syntax, "{\n  val x: Int = 1\n  x\n}")
    assertEquals(author(method).syntax, "{\n  def id(x: Int): Int = x\n  id\n}")

  private def project(source: String): TermShape =
    val term = Input.String(source).parse[Term].get
    ScalametaTermProjection.project(term)
      .fold(error => fail(error.message), _.shape)

  private def inspect(shape: TermShape): TermBindingView =
    right(TermShapeBindingView.inspect(shape))

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape)
      .fold(error => fail(error.message), identity)

  private def right[A](value: Either[TermBindingFailure, A]): A =
    value.fold(failure => fail(failure.message), identity)
