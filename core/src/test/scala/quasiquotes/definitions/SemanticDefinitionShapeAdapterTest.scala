package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

class SemanticDefinitionShapeAdapterTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  test("adapts all five public V1 families through the retained private shapes"):
    val value = adapt(
      definition(
        SemanticDefinition.immutableValue(
          name("answer"),
          intType,
          TermShape.Literal("42")
        )
      )
    ).asInstanceOf[DefinitionShape.ImmutableVal]
    assertEquals(value.name, name("answer"))
    assertEquals(value.declaredType, TypeShape.Identifier("Int"))
    assertEquals(value.rhs, TermShape.Literal("42"))

    val parameterless = adapt(
      definition(
        SemanticDefinition.concreteMethod(name("answer"), Vector.empty, intType)(
          _ => Right(TermShape.Literal("42"))
        )
      )
    ).asInstanceOf[DefinitionShape.ParameterlessDef]
    assertEquals(parameterless.resultType, TypeShape.Identifier("Int"))
    assertEquals(parameterless.body, TermShape.Literal("42"))

    val oneDefinition = definition(
      SemanticDefinition.concreteMethod(
        name("show"),
        Vector(clause(parameter("x", intType))),
        stringType
      ) { scope =>
        scope.reference(0, 0).map(reference => TermShape.Select(reference, "toString"))
      }
    )
    val oneView = oneDefinition.asMethod.get
    val oneReference = oneView.parameterScope.reference(0, 0).toOption.get
      .asInstanceOf[TermShape.BoundReference]
    val one = adapt(oneDefinition).asInstanceOf[DefinitionShape.SingleParameterDef]
    assertEquals(one.parameterBinderId, oneReference.binderId)
    assertEquals(one.parameterName, name("x"))
    assertEquals(one.parameterType, TypeShape.Identifier("Int"))
    assertEquals(one.resultType, TypeShape.Identifier("String"))
    assertEquals(
      one.body,
      TermShape.Select(TermShape.BoundReference(one.parameterBinderId, "x"), "toString")
    )

    val twoDefinition = definition(
      SemanticDefinition.concreteMethod(
        name("pair"),
        Vector(clause(parameter("first", intType), parameter("second", stringType))),
        TypeNormalForm.STypeTuple(List(intType, stringType))
      ) { scope =>
        for
          first <- scope.reference(0, 0)
          second <- scope.reference(0, 1)
        yield TermShape.Tuple(List(first, second))
      }
    )
    val twoView = twoDefinition.asMethod.get
    val firstReference = twoView.parameterScope.reference(0, 0).toOption.get
      .asInstanceOf[TermShape.BoundReference]
    val secondReference = twoView.parameterScope.reference(0, 1).toOption.get
      .asInstanceOf[TermShape.BoundReference]
    val two = adapt(twoDefinition).asInstanceOf[DefinitionShape.TwoParameterDef]
    assertEquals(two.firstParameterBinderId, firstReference.binderId)
    assertEquals(two.secondParameterBinderId, secondReference.binderId)
    assertNotEquals(two.firstParameterBinderId, two.secondParameterBinderId)
    assertEquals(
      two.body,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(two.firstParameterBinderId, "first"),
          TermShape.BoundReference(two.secondParameterBinderId, "second")
        )
      )
    )

    val alias = adapt(
      definition(SemanticDefinition.typeAlias(name("T"), intType))
    ).asInstanceOf[DefinitionShape.SimpleTypeAlias]
    assertEquals(alias.rhs, TypeShape.Identifier("Int"))

  test("structurally inverts unresolved normal forms and proves exact round-trip"):
    val forms = List[TypeNormalForm](
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intType)),
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(stringType)),
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Either"),
        List(intType, stringType)
      ),
      TypeNormalForm.STypeTuple(List(intType, stringType)),
      TypeNormalForm.STypeFunction(List(intType), stringType),
      TypeNormalForm.STypeFunction(List(intType, stringType), intType)
    )

    forms.zipWithIndex.foreach { (normalForm, index) =>
      val alias = adapt(
        definition(SemanticDefinition.typeAlias(name(s"T$index"), normalForm))
      ).asInstanceOf[DefinitionShape.SimpleTypeAlias]
      assertEquals(TypeNormalForm.fromShape(alias.rhs), Right(normalForm))
    }

    // C027's current public factory is narrower than TypeNormalForm for AnyVal,
    // so this package-private forged V1 storage exercises the adapter's required
    // future-proof lossless boundary without widening that factory.
    val anyVal = TypeNormalForm.STypeIdent("AnyVal")
    val anyValDefinition = new SemanticDefinition(
      DefinitionKind.Value,
      name("boxed"),
      DefinitionModifiers.empty,
      storage("ValueStorage", anyVal, TermShape.Literal("1"))
    )
    val anyValShape = adapt(anyValDefinition).asInstanceOf[DefinitionShape.ImmutableVal]
    assertEquals(anyValShape.declaredType, TypeShape.Identifier("AnyVal"))
    assertEquals(TypeNormalForm.fromShape(anyValShape.declaredType), Right(anyVal))

  test("preserves binder roles across alpha-renaming free spelling and unused parameters"):
    val first = identityMethod("x")
    val renamed = identityMethod("renamed")
    val firstShape = adapt(first).asInstanceOf[DefinitionShape.SingleParameterDef]
    val renamedShape = adapt(renamed).asInstanceOf[DefinitionShape.SingleParameterDef]
    val firstHandle = first.asMethod.get.parameterScope.binder(0, 0).toOption.get
    val renamedHandle = renamed.asMethod.get.parameterScope.binder(0, 0).toOption.get

    assertEquals(firstShape, renamedShape)
    assertNotEquals(firstHandle, renamedHandle)

    val free = definition(
      SemanticDefinition.concreteMethod(
        name("identity"),
        Vector(clause(parameter("x", intType))),
        intType
      )(_ => Right(TermShape.Identifier("x", isPlaceholder = false)))
    )
    val freeShape = adapt(free).asInstanceOf[DefinitionShape.SingleParameterDef]
    assertEquals(freeShape.body, TermShape.Identifier("x", isPlaceholder = false))
    assertNotEquals(freeShape, firstShape)

    val unused = definition(
      SemanticDefinition.concreteMethod(
        name("constant"),
        Vector(clause(parameter("unused", intType))),
        intType
      )(_ => Right(TermShape.Literal("1")))
    )
    val unusedView = unused.asMethod.get
    val unusedReference = unusedView.parameterScope.reference(0, 0).toOption.get
      .asInstanceOf[TermShape.BoundReference]
    val unusedShape = adapt(unused).asInstanceOf[DefinitionShape.SingleParameterDef]
    assertEquals(unusedShape.parameterBinderId, unusedReference.binderId)
    assertEquals(unusedShape.body, TermShape.Literal("1"))

  test("keeps first second and combined two-parameter references distinct"):
    def method(label: String, bodyIndex: Option[Int]): DefinitionShape.TwoParameterDef =
      val semantic = definition(
        SemanticDefinition.concreteMethod(
          name(label),
          Vector(clause(parameter("first", intType), parameter("second", intType))),
          intType
        ) { scope =>
          bodyIndex match
            case Some(index) => scope.reference(0, index)
            case None =>
              for
                first <- scope.reference(0, 0)
                second <- scope.reference(0, 1)
              yield TermShape.Tuple(List(first, second))
        }
      )
      adapt(semantic).asInstanceOf[DefinitionShape.TwoParameterDef]

    val first = method("firstOnly", Some(0))
    val second = method("secondOnly", Some(1))
    val both = method("both", None)

    assertEquals(first.body, TermShape.BoundReference(first.firstParameterBinderId, "first"))
    assertEquals(second.body, TermShape.BoundReference(second.secondParameterBinderId, "second"))
    assertEquals(
      both.body,
      TermShape.Tuple(
        List(
          TermShape.BoundReference(both.firstParameterBinderId, "first"),
          TermShape.BoundReference(both.secondParameterBinderId, "second")
        )
      )
    )

  test("classifies null resolved malformed unknown and unsupported topology distinctly"):
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(null),
      "MISSING_INPUT"
    )

    val scalaOwner = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")
    val resolved = TypeNormalForm.STypeResolved(
      ResolvedTypeNameId(Vector(scalaOwner), "Int")
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(
        definition(SemanticDefinition.typeAlias(name("Resolved"), resolved))
      ),
      "UNSUPPORTED_SEMANTIC_VALUE"
    )

    val malformedType = new SemanticDefinition(
      DefinitionKind.Value,
      name("broken"),
      DefinitionModifiers.empty,
      storage("ValueStorage", TypeNormalForm.STypeTuple(null), TermShape.Literal("0"))
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(malformedType),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val wrongView = new SemanticDefinition(
      DefinitionKind.Method,
      name("wrongView"),
      DefinitionModifiers.empty,
      storage("ValueStorage", intType, TermShape.Literal("0"))
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(wrongView),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val unknownKind = new SemanticDefinition(
      forgedKind("future-kind"),
      name("future"),
      DefinitionModifiers.empty,
      storage("ValueStorage", intType, TermShape.Literal("0"))
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(unknownKind),
      "UNSUPPORTED_SEMANTIC_VALUE"
    )

    val threeParameters = clause(
      parameter("a", intType),
      parameter("b", intType),
      parameter("c", intType)
    )
    val sourceMethod = identityMethod("scope")
    val sourceView = sourceMethod.asMethod.get
    val wideMethod = new SemanticDefinition(
      DefinitionKind.Method,
      name("wide"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector(threeParameters),
        sourceView.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(wideMethod),
      "UNSUPPORTED_SEMANTIC_VALUE"
    )

  test("fails closed when required payloads or persistent binder relations are corrupt"):
    val missingBody = new SemanticDefinition(
      DefinitionKind.Value,
      name("missing"),
      DefinitionModifiers.empty,
      storage("ValueStorage", intType, null)
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(missingBody),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val sourceMethod = identityMethod("x")
    val sourceView = sourceMethod.asMethod.get
    val corruptBody = TermShape.BoundReference(BinderId(999), "x")
    val corruptMethod = new SemanticDefinition(
      DefinitionKind.Method,
      name("identity"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        sourceView.parameterClauses,
        sourceView.parameterScope,
        sourceView.resultType,
        corruptBody,
        corruptBody
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(corruptMethod),
      "SEMANTIC_ADAPTER_FAILED"
    )

    val twoParameters = clause(parameter("x", intType), parameter("y", intType))
    val scopeMismatch = new SemanticDefinition(
      DefinitionKind.Method,
      name("pair"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector(twoParameters),
        sourceView.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(scopeMismatch),
      "SEMANTIC_ADAPTER_FAILED"
    )

    val corruptScope = new DefinitionParameterScope(null)
    val corruptScopeMethod = new SemanticDefinition(
      DefinitionKind.Method,
      name("corruptScope"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        sourceView.parameterClauses,
        corruptScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(corruptScopeMethod),
      "SEMANTIC_ADAPTER_FAILED"
    )

  test("rejects cross-graph numeric binder coincidence and mismatched scope topology"):
    val firstGraph = identityMethod("first")
    val secondGraph = identityMethod("second")
    val firstView = firstGraph.asMethod.get
    val secondView = secondGraph.asMethod.get
    val firstReference = firstView.body.get.asInstanceOf[TermShape.BoundReference]
    val secondReference = secondView.body.get.asInstanceOf[TermShape.BoundReference]
    assertEquals(firstReference.binderId, secondReference.binderId)

    val crossGraph = new SemanticDefinition(
      DefinitionKind.Method,
      name("identity"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        secondView.parameterClauses,
        secondView.parameterScope,
        secondView.resultType,
        firstView.body.get,
        firstView.body.get
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(crossGraph),
      "SEMANTIC_ADAPTER_FAILED"
    )

    val twoParameterGraph = definition(
      SemanticDefinition.concreteMethod(
        name("pair"),
        Vector(clause(parameter("x", intType), parameter("y", intType))),
        intType
      )(_ => Right(TermShape.Literal("0")))
    )
    val extraScope = new SemanticDefinition(
      DefinitionKind.Method,
      name("identity"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        secondView.parameterClauses,
        twoParameterGraph.asMethod.get.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(extraScope),
      "SEMANTIC_ADAPTER_FAILED"
    )

    val parameterlessWithScope = new SemanticDefinition(
      DefinitionKind.Method,
      name("constant"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector.empty,
        secondView.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(parameterlessWithScope),
      "SEMANTIC_ADAPTER_FAILED"
    )

  test("rejects resolved types in every Definition role and unsupported semantic bodies"):
    val scalaOwner = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")
    val resolved = TypeNormalForm.STypeResolved(
      ResolvedTypeNameId(Vector(scalaOwner), "Int")
    )
    val resolvedValue = definition(
      SemanticDefinition.immutableValue(
        name("resolvedValue"),
        resolved,
        TermShape.Literal("0")
      )
    )
    val resolvedResult = definition(
      SemanticDefinition.concreteMethod(name("resolvedResult"), Vector.empty, resolved)(
        _ => Right(TermShape.Literal("0"))
      )
    )
    val resolvedParameter = definition(
      SemanticDefinition.concreteMethod(
        name("resolvedParameter"),
        Vector(clause(parameter("x", resolved))),
        intType
      )(_ => Right(TermShape.Literal("0")))
    )

    List(resolvedValue, resolvedResult, resolvedParameter).foreach { candidate =>
      assertFailure(
        SemanticDefinitionShapeAdapter.adapt(candidate),
        "UNSUPPORTED_SEMANTIC_VALUE"
      )
    }

    val lambdaBody = quasiquotes.terms.TermShapeBindings.lambda(
      Vector(quasiquotes.terms.TermParameterSpec("x", intType))
    ) { scope =>
      scope.reference(scope.parameterBinders.head.head)
    }.fold(error => fail(error.message), identity)
    val unsupportedBody = definition(
      SemanticDefinition.immutableValue(name("lambda"), intType, lambdaBody)
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(unsupportedBody),
      "UNSUPPORTED_SEMANTIC_VALUE"
    )

    val nullResolved = TypeNormalForm.STypeResolved(null)
    val malformedResolvedTypes = List[TypeNormalForm](
      nullResolved,
      TypeNormalForm.STypeTuple(List(intType, nullResolved)),
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(nullResolved))
    )
    malformedResolvedTypes.zipWithIndex.foreach { (candidateType, index) =>
      val candidate = new SemanticDefinition(
        DefinitionKind.Value,
        name(s"nullResolved$index"),
        DefinitionModifiers.empty,
        storage("ValueStorage", candidateType, TermShape.Literal("0"))
      )
      assertFailure(
        SemanticDefinitionShapeAdapter.adapt(candidate),
        "MALFORMED_SEMANTIC_VALUE"
      )
    }

  test("validates malformed recursive Term payloads and parameterless binder ownership"):
    val malformedBodies = List[TermShape](
      TermShape.Identifier(null, isPlaceholder = false),
      TermShape.Tuple(null),
      TermShape.Apply(TermShape.Identifier("f", isPlaceholder = false), List(null))
    )
    malformedBodies.zipWithIndex.foreach { (body, index) =>
      val candidate = new SemanticDefinition(
        DefinitionKind.Value,
        name(s"malformedBody$index"),
        DefinitionModifiers.empty,
        storage("ValueStorage", intType, body)
      )
      assertFailure(
        SemanticDefinitionShapeAdapter.adapt(candidate),
        "MALFORMED_SEMANTIC_VALUE"
      )
    }

    val foreignMethod = identityMethod("foreign")
    val emptyScopeMethod = definition(
      SemanticDefinition.concreteMethod(name("constant"), Vector.empty, intType)(
        _ => Right(TermShape.Literal("0"))
      )
    )
    val parameterlessForeignReference = new SemanticDefinition(
      DefinitionKind.Method,
      name("constant"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector.empty,
        emptyScopeMethod.asMethod.get.parameterScope,
        intType,
        foreignMethod.asMethod.get.body.get,
        foreignMethod.asMethod.get.body.get
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(parameterlessForeignReference),
      "SEMANTIC_ADAPTER_FAILED"
    )

  test("contains forged null storage and alias payload while accepting semantic empty modifiers"):
    val nullStorage = new SemanticDefinition(
      DefinitionKind.Value,
      name("nullStorage"),
      DefinitionModifiers.empty,
      null
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(nullStorage),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val missingAlias = new SemanticDefinition(
      DefinitionKind.TypeMember,
      name("MissingAlias"),
      DefinitionModifiers.empty,
      storage("TypeStorage", null)
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(missingAlias),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val forgedModifiers =
      val constructor = classOf[DefinitionModifiers].getDeclaredConstructors.head
      constructor.setAccessible(true)
      constructor.newInstance().asInstanceOf[DefinitionModifiers]
    val unsupportedModifiers = new SemanticDefinition(
      DefinitionKind.Value,
      name("modified"),
      forgedModifiers,
      storage("ValueStorage", intType, TermShape.Literal("0"))
    )
    assertEquals(
      adapt(unsupportedModifiers).asInstanceOf[DefinitionShape.ImmutableVal].name,
      name("modified")
    )

  test("classifies malformed recursive types and forged clause kinds"):
    val malformedTypes = List[TypeNormalForm](
      TypeNormalForm.STypeApply(null, List(intType)),
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(null)),
      TypeNormalForm.STypeTuple(List(intType, null)),
      TypeNormalForm.STypeFunction(List(intType), null)
    )
    malformedTypes.zipWithIndex.foreach { (malformedType, index) =>
      val candidate = new SemanticDefinition(
        DefinitionKind.Value,
        name(s"malformed$index"),
        DefinitionModifiers.empty,
        storage("ValueStorage", malformedType, TermShape.Literal("0"))
      )
      assertFailure(
        SemanticDefinitionShapeAdapter.adapt(candidate),
        "MALFORMED_SEMANTIC_VALUE"
      )
    }

    val sourceMethod = identityMethod("x")
    val sourceView = sourceMethod.asMethod.get
    val nullKindClause = forgedClause(null, Vector(parameter("x", intType)))
    val nullKindMethod = new SemanticDefinition(
      DefinitionKind.Method,
      name("nullClauseKind"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector(nullKindClause),
        sourceView.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(nullKindMethod),
      "MALFORMED_SEMANTIC_VALUE"
    )

    val unknownClauseKind = forgedClause(
      forgedClauseKind("future-clause"),
      Vector(parameter("x", intType))
    )
    val unknownClauseMethod = new SemanticDefinition(
      DefinitionKind.Method,
      name("unknownClauseKind"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        Vector(unknownClauseKind),
        sourceView.parameterScope,
        intType,
        TermShape.Literal("0"),
        TermShape.Literal("0")
      )
    )
    assertFailure(
      SemanticDefinitionShapeAdapter.adapt(unknownClauseMethod),
      "UNSUPPORTED_SEMANTIC_VALUE"
    )

  private def adapt(definition: SemanticDefinition): DefinitionShape =
    SemanticDefinitionShapeAdapter.adapt(definition)
      .fold(error => fail(error.message), identity)

  private def definition(
      value: Either[DefinitionSemanticError, SemanticDefinition]
  ): SemanticDefinition =
    value.fold(error => fail(error.message), identity)

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(error => fail(error.message), identity)

  private def parameter(
      source: String,
      declaredType: TypeNormalForm
  ): DefinitionParameter =
    DefinitionParameter(name(source), declaredType)

  private def clause(parameters: DefinitionParameter*): DefinitionParameterClause =
    DefinitionParameterClause.ordinary(parameters.toVector)
      .fold(error => fail(error.message), identity)

  private def identityMethod(parameterName: String): SemanticDefinition =
    definition(
      SemanticDefinition.concreteMethod(
        name("identity"),
        Vector(clause(parameter(parameterName, intType))),
        intType
      )(_.reference(0, 0))
    )

  private def assertFailure[A](
      result: Either[SemanticDefinitionShapeAdapter.Error, A],
      expectedCode: String
  ): Unit =
    result match
      case Left(error) =>
        assertEquals(error.code, expectedCode)
        assertEquals(error.message, s"${error.code}: ${error.detail}")
      case Right(value) => fail(s"expected $expectedCode, got $value")

  private def storage(suffix: String, arguments: AnyRef*): AnyRef =
    val storageClass = Class.forName(
      s"quasiquotes.definitions.SemanticDefinition$$$suffix"
    )
    val constructor = storageClass.getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[AnyRef]

  private def forgedKind(code: String): DefinitionKind =
    val constructor = classOf[DefinitionKind].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(code).asInstanceOf[DefinitionKind]

  private def forgedClauseKind(code: String): DefinitionParameterClauseKind =
    val constructor = classOf[DefinitionParameterClauseKind].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(code).asInstanceOf[DefinitionParameterClauseKind]

  private def forgedClause(
      kind: DefinitionParameterClauseKind,
      parameters: Vector[DefinitionParameter]
  ): DefinitionParameterClause =
    val constructor = classOf[DefinitionParameterClause].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(kind, parameters).asInstanceOf[DefinitionParameterClause]
