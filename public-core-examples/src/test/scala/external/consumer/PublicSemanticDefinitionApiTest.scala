package external.consumer

import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.terms.*
import quasiquotes.types.TypeNormalForm

import scala.compiletime.testing.typeCheckErrors

final class PublicSemanticDefinitionApiTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  test("validated public names retain ordinary and backticked source spelling"):
    assertEquals(name("answer").source, "answer")
    assertEquals(name("`type`").source, "`type`")
    assertFailure(DefinitionName.fromSource("bad-name"), "DEFINITION_SEMANTIC_INVALID_NAME")
    assertFailure(DefinitionName.fromSource(null), "DEFINITION_SEMANTIC_MISSING")

  test("public category and empty-modifier tokens describe values without an exhaustive hierarchy"):
    val value = immutableValue()
    val method = parameterlessMethod()
    val alias = typeAlias()

    assertEquals(value.kind.code, "value")
    assertEquals(method.kind.code, "method")
    assertEquals(alias.kind.code, "type-member")
    assertEquals(value.modifiers, DefinitionModifiers.empty)
    assertEquals(method.modifiers, DefinitionModifiers.empty)

  test("ordinary clauses validate parameter structure without an arity-coded public type"):
    val ordinary = clause(parameter("x", intType), parameter("label", stringType))

    assertEquals(ordinary.kind.code, "ordinary")
    assertEquals(ordinary.parameters.map(_.name.source), Vector("x", "label"))
    assertEquals(ordinary.parameters.map(_.declaredType), Vector(intType, stringType))
    assertFailure(DefinitionParameterClause.ordinary(Vector.empty), "DEFINITION_SEMANTIC_INVALID_PARAMETER")
    assertFailure(DefinitionParameterClause.ordinary(null), "DEFINITION_SEMANTIC_MISSING")
    assertFailure(
      DefinitionParameterClause.ordinary(Vector(null)),
      "DEFINITION_SEMANTIC_INVALID_PARAMETER"
    )

  test("immutable values expose normalized type and optional concrete body"):
    val definition = immutableValue()
    val view = definition.asValue.getOrElse(fail("expected value view"))

    assertEquals(definition.name, name("answer"))
    assertEquals(view.declaredType, intType)
    assertEquals(view.body, Some(TermShape.Literal("42")))
    assertEquals(definition.asMethod, None)
    assertEquals(definition.asType, None)

  test("parameterless concrete methods expose an empty persistent parameter scope"):
    val definition = parameterlessMethod()
    val view = definition.asMethod.getOrElse(fail("expected method view"))

    assertEquals(view.parameterClauses, Vector.empty)
    assertEquals(view.resultType, intType)
    assertEquals(view.body, Some(TermShape.Literal("42")))
    assertFailure(view.parameterScope.binder(0, 0), "DEFINITION_SEMANTIC_UNBOUND_PARAMETER")
    assertFailure(view.parameterScope.reference(0, 0), "DEFINITION_SEMANTIC_UNBOUND_PARAMETER")

  test("one-parameter methods correlate persistent scope binders with body references"):
    val definition = identityMethod("x")
    val view = definition.asMethod.get
    val parameterBinder = right(view.parameterScope.binder(0, 0))
    val bodyBinder = inspect(view.body.get).boundReference.get.binder
    val persistentReferenceBinder = inspect(right(view.parameterScope.reference(0, 0)))
      .boundReference.get.binder

    assertEquals(parameterBinder, bodyBinder)
    assertEquals(persistentReferenceBinder, parameterBinder)
    assertEquals(view.parameterClauses.head.parameters.head.name.source, "x")

  test("two-parameter methods retain distinct ordered binder co-reference"):
    val parameters = clause(parameter("x", intType), parameter("label", stringType))
    val definition = right(
      SemanticDefinition.concreteMethod(
        name("pair"),
        Vector(parameters),
        intType
      ) { scope =>
        for
          first <- scope.reference(0, 0)
          second <- scope.reference(0, 1)
        yield TermShape.Tuple(List(first, second))
      }
    )
    val view = definition.asMethod.get
    val tuple = view.body.get.asInstanceOf[TermShape.Tuple]
    val first = right(view.parameterScope.binder(0, 0))
    val second = right(view.parameterScope.binder(0, 1))

    assertNotEquals(first, second)
    assertEquals(inspect(tuple.elements.head).boundReference.get.binder, first)
    assertEquals(inspect(tuple.elements(1)).boundReference.get.binder, second)

  test("same-spelling ordinary identifiers remain free beside method parameters"):
    val free = right(
      SemanticDefinition.concreteMethod(
        name("id"),
        Vector(clause(parameter("x", intType))),
        intType
      )(_ => Right(TermShape.Identifier("x", isPlaceholder = false)))
    )
    val body = free.asMethod.get.body.get

    assertEquals(body, TermShape.Identifier("x", false))
    assertEquals(inspect(body).category.code, "ordinary")
    assertNotEquals(free, identityMethod("x"))

  test("unused parameters still expose persistent opaque binders"):
    val definition = right(
      SemanticDefinition.concreteMethod(
        name("constant"),
        Vector(clause(parameter("unused", intType))),
        intType
      )(_ => Right(TermShape.Literal("1")))
    )
    val view = definition.asMethod.get

    assert(right(view.parameterScope.binder(0, 0)) != null)
    assertEquals(view.body, Some(TermShape.Literal("1")))

  test("method equality and hashing are alpha-aware while binder handles remain graph-local"):
    val first = identityMethod("x")
    val renamed = identityMethod("renamed")
    val firstBinder = right(first.asMethod.get.parameterScope.binder(0, 0))
    val renamedBinder = right(renamed.asMethod.get.parameterScope.binder(0, 0))

    assertEquals(first, renamed)
    assertEquals(first.hashCode, renamed.hashCode)
    assertNotEquals(firstBinder, renamedBinder)

    val useSecond = right(
      SemanticDefinition.concreteMethod(
        name("choose"),
        Vector(clause(parameter("x", intType), parameter("y", intType))),
        intType
      )(_.reference(0, 1))
    )
    val useFirst = right(
      SemanticDefinition.concreteMethod(
        name("choose"),
        Vector(clause(parameter("a", intType), parameter("b", intType))),
        intType
      )(_.reference(0, 0))
    )
    assertNotEquals(useFirst, useSecond)

  test("type aliases expose optional normalized alias semantics"):
    val definition = typeAlias()
    val view = definition.asType.getOrElse(fail("expected type view"))

    assertEquals(definition.name.source, "T")
    assertEquals(view.aliasedType, Some(intType))
    assertEquals(definition.asValue, None)
    assertEquals(definition.asMethod, None)

  test("unsupported method topology and malformed semantic children fail with stable codes"):
    val three = clause(
      parameter("a", intType),
      parameter("b", intType),
      parameter("c", intType)
    )
    assertFailure(
      SemanticDefinition.concreteMethod(name("wide"), Vector(three), intType)(
        _ => Right(TermShape.Literal("0"))
      ),
      "DEFINITION_SEMANTIC_UNSUPPORTED"
    )
    assertFailure(
      SemanticDefinition.concreteMethod(
        name("manyClauses"),
        Vector(clause(parameter("a", intType)), clause(parameter("b", intType))),
        intType
      )(_ => Right(TermShape.Literal("0"))),
      "DEFINITION_SEMANTIC_UNSUPPORTED"
    )
    assertFailure(
      SemanticDefinition.immutableValue(
        name("badType"),
        TypeNormalForm.STypeTuple(null),
        TermShape.Literal("0")
      ),
      "DEFINITION_SEMANTIC_INVALID_TYPE"
    )
    assertFailure(
      SemanticDefinition.immutableValue(name("badBody"), intType, TermShape.Identifier(null, false)),
      "DEFINITION_SEMANTIC_INVALID_BODY"
    )
    assertFailure(
      SemanticDefinition.concreteMethod(
        name("badIndex"),
        Vector(clause(parameter("x", intType))),
        intType
      )(_.reference(0, 1)),
      "DEFINITION_SEMANTIC_UNBOUND_PARAMETER"
    )
    assertFailure(
      SemanticDefinition.concreteMethod(name("missingBody"), Vector.empty, intType)(null),
      "DEFINITION_SEMANTIC_MISSING"
    )

  test("foreign binder graphs and unsupported bodies fail at the public Definition boundary"):
    val foreign = termRight(
      TermShapeBindings.lambda(Vector(TermParameterSpec("x", intType))) { scope =>
        scope.reference(scope.parameterBinders.head.head)
      }
    )
    assertFailure(
      SemanticDefinition.concreteMethod(name("foreign"), Vector.empty, intType)(
        _ => Right(foreign)
      ),
      "DEFINITION_SEMANTIC_SCOPE_MISMATCH"
    )
    assertFailure(
      SemanticDefinition.concreteMethod(name("unsupported"), Vector.empty, intType)(
        _ => Right(TermShape.Unsupported("While", "not admitted"))
      ),
      "DEFINITION_SEMANTIC_UNSUPPORTED"
    )

  test("nested accepted Term builders share the method parameter graph"):
    val definition = right(
      SemanticDefinition.concreteMethod(
        name("nested"),
        Vector(clause(parameter("x", intType))),
        intType
      ) { parameterScope =>
        for
          parameterReference <- parameterScope.reference(0, 0)
          nested <- termToDefinition(
            TermShapeBindings.localValue("copy", intType, parameterReference) { localScope =>
              localScope.reference(localScope.declaredBinder.get)
            }
          )
        yield nested
      }
    )
    val method = definition.asMethod.get
    val parameterBinder = right(method.parameterScope.binder(0, 0))
    val block = inspect(method.body.get).block.get

    assertEquals(inspect(block.locals.head.body.get).boundReference.get.binder, parameterBinder)
    assertEquals(inspect(block.result).boundReference.get.binder, block.locals.head.binder)

  test("errors have stable code-first messages"):
    val error = DefinitionSemanticError("DEFINITION_SEMANTIC_INVALID_BODY", "body was absent")
    assertEquals(error.message, "DEFINITION_SEMANTIC_INVALID_BODY: body was absent")

  test("opaque Definition values and views have no foreign-callable constructors or subclasses"):
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.DefinitionName.plain(\"legacy\")"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.DefinitionName.fromSource(\"x\").toOption.get.decoded"
      ).nonEmpty
    )
    assert(typeCheckErrors("new quasiquotes.definitions.DefinitionKind(\"invented\")").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.DefinitionModifiers()").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.DefinitionParameterClauseKind(\"x\")").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.DefinitionParameterScope(null)").nonEmpty)
    assert(typeCheckErrors("new quasiquotes.definitions.SemanticDefinition(null)").nonEmpty)
    assert(
      typeCheckErrors(
        "class Forged extends quasiquotes.definitions.SemanticDefinition(null)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.SemanticDefinition.method1"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "quasiquotes.definitions.SemanticDefinitionShapeAdapter.adapt(null)"
      ).nonEmpty
    )
    assert(typeCheckErrors("classOf[quasiquotes.definitions.DefinitionShape]").nonEmpty)
    assert(typeCheckErrors("quasiquotes.parser.BinderId(0)").nonEmpty)

  private def name(source: String): DefinitionName = right(DefinitionName.fromSource(source))

  private def parameter(source: String, declaredType: TypeNormalForm): DefinitionParameter =
    DefinitionParameter(name(source), declaredType)

  private def clause(parameters: DefinitionParameter*): DefinitionParameterClause =
    right(DefinitionParameterClause.ordinary(parameters.toVector))

  private def immutableValue(): SemanticDefinition =
    right(
      SemanticDefinition.immutableValue(
        name("answer"),
        intType,
        TermShape.Literal("42")
      )
    )

  private def parameterlessMethod(): SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(name("answer"), Vector.empty, intType)(
        _ => Right(TermShape.Literal("42"))
      )
    )

  private def identityMethod(parameterName: String): SemanticDefinition =
    right(
      SemanticDefinition.concreteMethod(
        name("id"),
        Vector(clause(parameter(parameterName, intType))),
        intType
      )(_.reference(0, 0))
    )

  private def typeAlias(): SemanticDefinition =
    right(SemanticDefinition.typeAlias(name("T"), intType))

  private def inspect(shape: TermShape): TermBindingView =
    termRight(TermShapeBindingView.inspect(shape))

  private def right[A](value: Either[DefinitionSemanticError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def termRight[A](value: Either[TermBindingFailure, A]): A =
    value.fold(error => fail(error.message), identity)

  private def termToDefinition[A](
      value: Either[TermBindingFailure, A]
  ): Either[DefinitionSemanticError, A] =
    value.left.map(error => DefinitionSemanticError("DEFINITION_SEMANTIC_INVALID_BODY", error.message))

  private def assertFailure[A](
      value: Either[DefinitionSemanticError, A],
      expectedCode: String
  ): Unit =
    assertEquals(value.left.toOption.map(_.code), Some(expectedCode))
