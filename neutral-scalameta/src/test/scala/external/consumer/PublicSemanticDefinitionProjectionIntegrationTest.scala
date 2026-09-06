package external.consumer

import quasiquotes.definitions.*
import quasiquotes.neutral.{ProjectedDefinition, ScalametaDefinitionProjection}
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBindingFailure, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class PublicSemanticDefinitionProjectionIntegrationTest extends munit.FunSuite:
  test("foreign consumer projects and inspects value method and alias without private carriers"):
    val value = project("val answer: Int = 42").definition
    val method = project("def id(x: Int): Int = x").definition
    val alias = project("type Result = Either[List[Int], Option[String]]").definition

    assertEquals(value.kind, DefinitionKind.Value)
    assertEquals(value.asValue.get.declaredType, TypeNormalForm.STypeIdent("Int"))
    assertEquals(value.asValue.get.body, Some(TermShape.Literal("42")))

    val methodView = method.asMethod.get
    val parameterBinder = definitionRight(methodView.parameterScope.binder(0, 0))
    val bodyBinder = bindingRight(TermShapeBindingView.inspect(methodView.body.get))
      .boundReference.get.binder
    val persistentReferenceBinder = bindingRight(
      TermShapeBindingView.inspect(definitionRight(methodView.parameterScope.reference(0, 0)))
    ).boundReference.get.binder
    assertEquals(method.kind, DefinitionKind.Method)
    assertEquals(methodView.parameterClauses.head.parameters.head.name.source, "x")
    assert(bodyBinder == parameterBinder)
    assert(persistentReferenceBinder == parameterBinder)

    assertEquals(alias.kind, DefinitionKind.TypeMember)
    assertEquals(alias.name.source, "Result")
    assert(alias.asType.get.aliasedType.nonEmpty)

  private def project(source: String): ProjectedDefinition =
    val definition = Scala3(source).parse[Stat].get.asInstanceOf[Defn]
    ScalametaDefinitionProjection.project(definition)
      .fold(problem => fail(problem.message), identity)

  private def definitionRight[A](result: Either[DefinitionSemanticError, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def bindingRight[A](result: Either[TermBindingFailure, A]): A =
    result.fold(problem => fail(problem.message), identity)
