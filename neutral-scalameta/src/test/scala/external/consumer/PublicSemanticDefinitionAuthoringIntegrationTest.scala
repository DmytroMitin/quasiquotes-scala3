package external.consumer

import quasiquotes.definitions.*
import quasiquotes.neutral.{ScalametaDefinitionAuthoring, ScalametaDefinitionProjection}
import quasiquotes.parser.TermShape
import quasiquotes.terms.{TermBindingFailure, TermShapeBindingView}
import quasiquotes.types.TypeNormalForm

import scala.compiletime.testing.typeCheckErrors
import scala.meta.Defn

final class PublicSemanticDefinitionAuthoringIntegrationTest extends munit.FunSuite:
  test("foreign consumer authors value method and alias using public APIs only"):
    val intType = TypeNormalForm.STypeIdent("Int")
    val value = definitionRight(
      SemanticDefinition.immutableValue(
        name("answer"),
        intType,
        TermShape.Literal("42")
      )
    )
    val clause = definitionRight(
      DefinitionParameterClause.ordinary(
        Vector(DefinitionParameter(name("value"), intType))
      )
    )
    val method = definitionRight(
      SemanticDefinition.concreteMethod(name("identity"), Vector(clause), intType) { scope =>
        scope.reference(0, 0)
      }
    )
    val alias = definitionRight(
      SemanticDefinition.typeAlias(name("Result"), intType)
    )

    List(value, method, alias).foreach { definition =>
      val authored = ScalametaDefinitionAuthoring.author(definition)
        .fold(problem => fail(problem.message), identity)
      val projected = ScalametaDefinitionProjection.project(authored)
        .fold(problem => fail(problem.message), identity)
      assertEquals(projected.definition, definition)
      assertEquals(projected.sourceSpan, None)
    }

    val methodView = method.asMethod.get
    val parameter = bindingRight(
      TermShapeBindingView.inspect(definitionRight(methodView.parameterScope.reference(0, 0)))
    ).boundReference.get.binder
    val body = bindingRight(TermShapeBindingView.inspect(methodView.body.get))
      .boundReference.get.binder
    assert(parameter == body)

  test("foreign API exposes only the public semantic authoring signature"):
    val api: SemanticDefinition => Either[ScalametaDefinitionAuthoring.Error, Defn] =
      ScalametaDefinitionAuthoring.author

    assert(api != null)
    assert(
      typeCheckErrors(
        "quasiquotes.neutral.ScalametaDefinitionAuthoring.authorShape(null)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "val hidden: quasiquotes.neutral.ScalametaDefinitionAuthoring.ShapeError = null"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "val hidden: quasiquotes.definitions.DefinitionShape = null"
      ).nonEmpty
    )
    assert(typeCheckErrors("quasiquotes.parser.BinderId(0)").nonEmpty)

  private def name(source: String): DefinitionName =
    definitionRight(DefinitionName.fromSource(source))

  private def definitionRight[A](result: Either[DefinitionSemanticError, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def bindingRight[A](result: Either[TermBindingFailure, A]): A =
    result.fold(problem => fail(problem.message), identity)
