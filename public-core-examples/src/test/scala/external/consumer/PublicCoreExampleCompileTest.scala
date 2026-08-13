package external.consumer

import quasiquotes.publicapi.CompletedTerm
import quasiquotes.publicapi.CompletedType
import quasiquotes.publicapi.DefinitionConstruction
import quasiquotes.publicapi.DefinitionResultView
import quasiquotes.publicapi.PublicFailure
import quasiquotes.parser.TermShape
import quasiquotes.parser.TypeShape
import quasiquotes.types.{ConstructedType, TypeNormalForm, TypePattern, TypeTemplate}

final class PublicCoreExampleCompileTest extends munit.FunSuite:
  private def showMethod(
      methodName: String = "apply",
      typeParameterName: String = "A",
      occurrenceName: String = "A",
      contextualName: String = "instance",
      bodyName: String = "instance"
  ): Either[PublicFailure, DefinitionResultView] =
    for
      show <- CompletedType.named("Show")
      parameter <- CompletedType.typeParameter(occurrenceName)
      showOfParameter <- CompletedType.applied(show, Vector(parameter))
      instance <- CompletedTerm.reference(bodyName)
      method <- DefinitionConstruction.contextualMethod(
        methodName,
        typeParameterName,
        contextualName,
        showOfParameter,
        showOfParameter,
        instance
      )
    yield method

  test("external core-only consumer constructs and projects Show[A]"):
    val result = showMethod().toOption.getOrElse(fail("expected success"))

    assertEquals(result.kindCode, "method")
    assertEquals(result.name, "apply")
    assertEquals(result.typeParameterName, "A")
    assertEquals(result.contextualParameterName, "instance")
    assertEquals(result.contextualParameterType.source, "Show[A]")
    assertEquals(result.resultType.source, "Show[A]")
    assertEquals(result.body.referenceName, "instance")

  test("external core-only consumer constructs and projects an identity method"):
    val result = DefinitionFirstUseSnippet.identity.toOption
      .getOrElse(fail("expected identity method"))

    assertEquals(result.kindCode, "single-parameter-method")
    assertEquals(result.name, "id")
    assertEquals(result.parameterName, "x")
    assertEquals(result.parameterType.source, "Int")
    assertEquals(result.resultType.source, "Int")
    assertEquals(result.body.kindCode, "definition-parameter-reference")
    assertEquals(result.source, "def id(x: Int): Int = x")

  test("external core-only consumer receives an actionable unsupported parameter type"):
    val result =
      for
        unsupported <- CompletedType.named("Show")
        intType <- CompletedType.named("Int")
        parameter <- CompletedTerm.definitionParameterReference("x")
        method <- DefinitionConstruction.singleParameterMethod(
          "id", "x", unsupported, intType, parameter
        )
      yield method
    val failure = result.left.toOption.getOrElse(fail("expected failure"))

    assertEquals(failure.code, "invalid-single-parameter-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("parameter-type"))
    assert(
      failure.message.contains(
        "Unsupported type-construction identifier `Show`"
      )
    )

  test("external consumer receives stable method-name failure"):
    val failure = showMethod(methodName = "bad-name").left.toOption.get
    assertEquals(failure.code, "invalid-name")
    assertEquals(failure.anchor.map(_.componentCode), Some("method-name"))

  test("external consumer receives stable declared-parameter failure"):
    val failure = showMethod(typeParameterName = "bad-name").left.toOption.get
    assertEquals(failure.code, "invalid-name")
    assertEquals(failure.anchor.map(_.componentCode), Some("type-parameter"))

  test("external consumer receives stable dangling-parameter failure"):
    val failure = showMethod(
      typeParameterName = "A",
      occurrenceName = "B"
    ).left.toOption.get
    assertEquals(failure.code, "undeclared-type-parameter")
    assertEquals(
      failure.anchor.map(_.componentCode),
      Some("contextual-parameter-type")
    )

  test("external consumer receives stable contextual-name failure"):
    val failure = showMethod(contextualName = "bad-name").left.toOption.get
    assertEquals(failure.code, "invalid-name")
    assertEquals(
      failure.anchor.map(_.componentCode),
      Some("contextual-parameter-name")
    )

  test("external consumer receives stable body-contract failure"):
    val failure = showMethod(bodyName = "otherInstance").left.toOption.get
    assertEquals(failure.code, "invalid-contextual-method-contract")
    assertEquals(failure.anchor.map(_.componentCode), Some("body"))
    assertEquals(
      failure.message,
      "The body must reference contextual parameter `instance`."
    )

  test("external consumer receives stable invalid-application failure"):
    val show = CompletedType.named("Show").toOption.get
    val failure = CompletedType.applied(show, Vector.empty).left.toOption.get
    assertEquals(failure.code, "invalid-type-application")
    assertEquals(failure.anchor.map(_.componentCode), Some("type-application"))
    assertEquals(
      failure.message,
      "A type application requires at least one argument."
    )

  test("external consumer can inspect the experimental interpolation shape"):
    val shape = TermShape.InterpolatedString(
      "s",
      List("hello ", ""),
      List(TermShape.Identifier("name", isPlaceholder = false))
    )
    assertEquals(
      shape.render,
      "InterpolatedString(s, [\"hello \", \"\"], [Ident(name)])"
    )

  test("external core-only consumer can inspect constructor structure"):
    assertEquals(
      CoreFirstUseSnippet.constructorShape.render,
      "New(java.lang.StringBuilder, [Literal(16)])"
    )

  test("external core-only consumer uses recursive applied-type structures"):
    val shape = TypeShape.Apply(
      TypeShape.Identifier("Either"),
      List(
        TypeShape.Apply(
          TypeShape.Identifier("List"),
          List(TypeShape.Identifier("Int"))
        ),
        TypeShape.Apply(
          TypeShape.Identifier("Option"),
          List(TypeShape.Identifier("String"))
        )
      )
    )
    val normalForm = TypeNormalForm.fromShape(shape).toOption.get
    assertEquals(
      ConstructedType(normalForm).source,
      "Either[List[Int], Option[String]]"
    )
    assertEquals(TypeTemplate.validateConstructed(normalForm), Right(()))
    assertEquals(
      TypePattern.matchNormalForm(
        TypePattern.TPApply(
          TypePattern.TPIdent("Either"),
          List(TypePattern.TPHole("left"), TypePattern.TPHole("right"))
        ),
        normalForm
      ).map(_.bindings.keySet),
      Some(Set("left", "right"))
    )
