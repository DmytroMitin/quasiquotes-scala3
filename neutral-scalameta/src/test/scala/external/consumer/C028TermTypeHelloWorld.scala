package external.consumer

// snippet:semantic-term-type:start
import quasiquotes.definitions.*
import quasiquotes.neutral.*
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.meta.*
import scala.meta.dialects.Scala3

object SemanticTermTypeHelloWorld:
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

    val definitions = Vector(
      q"val foo: Int = 42".asInstanceOf[Defn],
      q"def foo(x: Int): String = x.toString".asInstanceOf[Defn],
      q"type T = Int".asInstanceOf[Defn]
    )

    definitions.foreach { source =>
      val projected = ScalametaDefinitionProjection
        .project(source)
        .fold(error => sys.error(error.message), identity)
      val authored = ScalametaDefinitionAuthoring
        .author(projected.definition)
        .fold(error => sys.error(error.message), identity)
      val reprojected = ScalametaDefinitionProjection
        .project(authored)
        .fold(error => sys.error(error.message), identity)

      assert(reprojected.definition == projected.definition)
      assert(reprojected.sourceSpan.isEmpty)
      assert(authored.pos == Position.None)
    }
// snippet:semantic-term-type:end

final class C028TermTypeHelloWorldTest extends munit.FunSuite:
  test("public Term, Type, and Definition conversion hello world"):
    SemanticTermTypeHelloWorld.check()
