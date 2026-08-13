package quasiquotes.publicapi

import quasiquotes.parser.TermShape

final class BoundedPublicCoreTest extends munit.FunSuite:
  private def rightValue[A](value: Either[PublicFailure, A]): A =
    value match
      case Right(result) => result
      case Left(failure) => fail(failure.toString)

  private def showMethod(
      declared: String = "A",
      occurrence: String = "A",
      bodyName: String = "instance"
  ): Either[PublicFailure, DefinitionResultView] =
    for
      show <- CompletedType.named("Show")
      parameter <- CompletedType.typeParameter(occurrence)
      showOfParameter <- CompletedType.applied(show, Vector(parameter))
      body <- CompletedTerm.reference(bodyName)
      result <- DefinitionConstruction.contextualMethod(
        "apply",
        declared,
        "instance",
        showOfParameter,
        showOfParameter,
        body
      )
    yield result

  test("constructs and projects the exact Show contextual method"):
    val result = rightValue(showMethod())

    assertEquals(result.kindCode, "method")
    assertEquals(result.name, "apply")
    assertEquals(result.typeParameterName, "A")
    assertEquals(result.contextualParameterName, "instance")
    assertEquals(result.contextualParameterType.source, "Show[A]")
    assertEquals(result.resultType.source, "Show[A]")
    assertEquals(result.body.source, "instance")

  test("completed values use structural name-sensitive equality"):
    val first = rightValue(CompletedType.typeParameter("A"))
    val second = rightValue(CompletedType.typeParameter("A"))
    val different = rightValue(CompletedType.typeParameter("B"))

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)
    assertNotEquals(first, different)

  test("completed type projections hide the representation"):
    val show = rightValue(CompletedType.named("Show"))
    val parameter = rightValue(CompletedType.typeParameter("A"))
    val applied = rightValue(CompletedType.applied(show, Vector(parameter)))

    assertEquals(show.kindCode, "named")
    assertEquals(parameter.kindCode, "type-parameter")
    assertEquals(applied.kindCode, "applied")
    assertEquals(applied.constructor, Some(show))
    assertEquals(applied.arguments, Vector(parameter))

  test("rejects invalid standalone names with stable anchors"):
    val failures = Vector(
      CompletedType.named(""),
      CompletedType.typeParameter("class"),
      CompletedTerm.reference("bad-name")
    ).map(_.left.toOption.getOrElse(fail("expected failure")))

    assert(failures.forall(_.code == "invalid-name"))
    assertEquals(failures(1).anchor.map(_.componentCode), Some("type-parameter"))
    assertEquals(failures(2).anchor.map(_.componentCode), Some("body"))

  test("rejects empty and non-named type applications"):
    val show = rightValue(CompletedType.named("Show"))
    val parameter = rightValue(CompletedType.typeParameter("A"))

    assertEquals(
      CompletedType.applied(show, Vector.empty).left.map(_.code),
      Left("invalid-type-application")
    )
    assertEquals(
      CompletedType.applied(parameter, Vector(show)).left.map(_.code),
      Left("invalid-type-application")
    )

  test("rejects invalid method and declared type parameter names"):
    val validType = rightValue(CompletedType.named("Show"))
    val body = rightValue(CompletedTerm.reference("instance"))
    val invalidMethod = DefinitionConstruction.contextualMethod(
      "bad-name", "A", "instance", validType, validType, body
    )
    val invalidParameter = DefinitionConstruction.contextualMethod(
      "apply", "bad-name", "instance", validType, validType, body
    )

    assertEquals(invalidMethod.left.map(_.code), Left("invalid-name"))
    assertEquals(
      invalidMethod.left.toOption.flatMap(_.anchor).map(_.componentCode),
      Some("method-name")
    )
    assertEquals(
      invalidParameter.left.toOption.flatMap(_.anchor).map(_.componentCode),
      Some("type-parameter")
    )

  test("rejects invalid contextual parameter name"):
    val validType = rightValue(CompletedType.named("Show"))
    val body = rightValue(CompletedTerm.reference("instance"))
    val result = DefinitionConstruction.contextualMethod(
      "apply", "A", "bad-name", validType, validType, body
    )

    assertEquals(result.left.map(_.code), Left("invalid-name"))
    assertEquals(
      result.left.toOption.flatMap(_.anchor).map(_.componentCode),
      Some("contextual-parameter-name")
    )

  test("rejects dangling type parameter in contextual parameter type"):
    val failure = showMethod(declared = "A", occurrence = "B")
      .left.toOption.getOrElse(fail("expected failure"))

    assertEquals(failure.code, "undeclared-type-parameter")
    assertEquals(
      failure.anchor.map(_.componentCode),
      Some("contextual-parameter-type")
    )

  test("rejects a body that does not reference the contextual parameter"):
    val failure = showMethod(bodyName = "other")
      .left.toOption.getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-contextual-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("body"))

  test("result equality and hash are structural"):
    val first = rightValue(showMethod())
    val second = rightValue(showMethod())

    assertEquals(first, second)
    assertEquals(first.hashCode, second.hashCode)

  test("constructs and projects a bound single-parameter identity method"):
    val intType = rightValue(CompletedType.named("Int"))
    val body = rightValue(CompletedTerm.definitionParameterReference("x"))
    val result = rightValue(
      DefinitionConstruction.singleParameterMethod(
        "id",
        "x",
        intType,
        intType,
        body
      )
    )

    assertEquals(result.kindCode, "single-parameter-method")
    assertEquals(result.name, "id")
    assertEquals(result.parameterName, "x")
    assertEquals(result.parameterType, intType)
    assertEquals(result.resultType, intType)
    assertEquals(result.body.kindCode, "definition-parameter-reference")
    assertEquals(result.body.source, "x")
    assertEquals(result.source, "def id(x: Int): Int = x")

  test("keeps free and definition-parameter references semantically distinct"):
    val free = rightValue(CompletedTerm.reference("x"))
    val bound = rightValue(CompletedTerm.definitionParameterReference("x"))

    assertEquals(free.kindCode, "reference")
    assertEquals(bound.kindCode, "definition-parameter-reference")
    assertNotEquals(free, bound)

  test("constructs the public body as a real internal bound reference"):
    val intType = rightValue(CompletedType.named("Int"))
    val body = rightValue(CompletedTerm.definitionParameterReference("x"))
    val constructed = rightValue(
      DefinitionConstruction.constructSingleParameterMethod(
        "id",
        "x",
        intType,
        intType,
        body
      )
    )

    constructed.body.root match
      case TermShape.BoundReference(binderId, "x") =>
        assertEquals(binderId, constructed.parameterBinderId)
      case other =>
        fail(s"expected the method binder reference, found ${other.render}")

  test("rejects a free same-text body instead of guessing binding by name"):
    val intType = rightValue(CompletedType.named("Int"))
    val freeBody = rightValue(CompletedTerm.reference("x"))
    val failure = DefinitionConstruction
      .singleParameterMethod("id", "x", intType, intType, freeBody)
      .left
      .toOption
      .getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-single-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("body"))

  test("rejects a mismatched explicit parameter-reference name"):
    val intType = rightValue(CompletedType.named("Int"))
    val body = rightValue(CompletedTerm.definitionParameterReference("other"))
    val failure = DefinitionConstruction
      .singleParameterMethod("id", "x", intType, intType, body)
      .left
      .toOption
      .getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-single-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("body"))

  test("rejects an unsupported parameter type with a stable anchor"):
    val unsupported = rightValue(CompletedType.named("Show"))
    val intType = rightValue(CompletedType.named("Int"))
    val body = rightValue(CompletedTerm.definitionParameterReference("x"))
    val failure = DefinitionConstruction
      .singleParameterMethod("id", "x", unsupported, intType, body)
      .left
      .toOption
      .getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-single-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("parameter-type"))

  test("rejects an identity body whose result type differs from its parameter"):
    val intType = rightValue(CompletedType.named("Int"))
    val stringType = rightValue(CompletedType.named("String"))
    val body = rightValue(CompletedTerm.definitionParameterReference("x"))
    val failure = DefinitionConstruction
      .singleParameterMethod("id", "x", intType, stringType, body)
      .left
      .toOption
      .getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-single-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("result-type"))
