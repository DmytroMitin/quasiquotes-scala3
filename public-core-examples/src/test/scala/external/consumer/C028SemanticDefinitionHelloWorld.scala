package external.consumer

// snippet:semantic-definition-core:start
import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.terms.TermShapeBindingView
import quasiquotes.types.TypeNormalForm

object SemanticDefinitionCoreHelloWorld:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(error => sys.error(error.message), identity)

  def check(): Unit =
    val value = SemanticDefinition
      .immutableValue(name("foo"), intType, TermShape.Literal("42"))
      .fold(error => sys.error(error.message), identity)

    val parameter = DefinitionParameter(name("x"), intType)
    val ordinaryClause = DefinitionParameterClause
      .ordinary(Vector(parameter))
      .fold(error => sys.error(error.message), identity)
    val method = SemanticDefinition
      .concreteMethod(name("foo"), Vector(ordinaryClause), stringType) { scope =>
        scope.reference(0, 0).map(reference => TermShape.Select(reference, "toString"))
      }
      .fold(error => sys.error(error.message), identity)

    val alias = SemanticDefinition
      .typeAlias(name("T"), intType)
      .fold(error => sys.error(error.message), identity)

    assert(value.asValue.get.body.contains(TermShape.Literal("42")))
    assert(alias.asType.get.aliasedType.contains(intType))

    val methodView = method.asMethod.get
    val parameterBinder = methodView.parameterScope
      .binder(0, 0)
      .fold(error => sys.error(error.message), identity)
    val bodyQualifier = methodView.body.get.asInstanceOf[TermShape.Select].qualifier
    val bodyBinder = TermShapeBindingView
      .inspect(bodyQualifier)
      .fold(error => sys.error(error.message), identity)
      .boundReference
      .get
      .binder

    assert(parameterBinder == bodyBinder)
// snippet:semantic-definition-core:end

final class C028SemanticDefinitionHelloWorldTest extends munit.FunSuite:
  test("public SemanticDefinition hello world"):
    SemanticDefinitionCoreHelloWorld.check()
