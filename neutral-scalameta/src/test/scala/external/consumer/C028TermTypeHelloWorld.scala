package external.consumer

// snippet:c028-term-type:start
import quasiquotes.neutral.*
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

object C028TermTypeHelloWorld:
  def check(): Unit =
    val projectedTerm = ScalametaTermProjection.project(q"1 + 2")
    val termShape = projectedTerm.fold(error => sys.error(error.message), _.shape)
    val authoredTerm = ScalametaTermShapeAuthoring
      .author(termShape)
      .fold(error => sys.error(error.message), identity)
    val reprojectedTerm = ScalametaTermProjection
      .project(authoredTerm)
      .fold(error => sys.error(error.message), _.shape)

    assert(reprojectedTerm == termShape)
    assert(authoredTerm.pos == Position.None)

    val intType = TypeNormalForm.STypeIdent("Int")
    val lambda = TermShapeBindings
      .lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
      .fold(error => sys.error(error.message), identity)
    val lambdaView = TermShapeBindingView
      .inspect(lambda)
      .fold(error => sys.error(error.message), identity)
      .lambda
      .get
    val bodyBinder = TermShapeBindingView
      .inspect(lambdaView.body)
      .fold(error => sys.error(error.message), identity)
      .boundReference
      .get
      .binder

    assert(bodyBinder == lambdaView.parameters.head.binder)

    val projectedType = ScalametaTypeNormalFormProjection.project(t"List[Int]")
    val normalForm = projectedType.fold(error => sys.error(error.message), _.normalForm)
    val authoredType = ScalametaTypeNormalFormAuthoring
      .author(normalForm)
      .fold(error => sys.error(error.message), identity)
    val reprojectedType = ScalametaTypeNormalFormProjection
      .project(authoredType)
      .fold(error => sys.error(error.message), _.normalForm)

    assert(reprojectedType == normalForm)
    assert(authoredType.pos == Position.None)
// snippet:c028-term-type:end

final class C028TermTypeHelloWorldTest extends munit.FunSuite:
  test("C028 public Term and Type hello world"):
    C028TermTypeHelloWorld.check()
