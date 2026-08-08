package external.consumer

import quasiquotes.publicapi.CompletedTerm
import quasiquotes.publicapi.CompletedType
import quasiquotes.publicapi.DefinitionConstruction
import quasiquotes.publicapi.DefinitionResultView
import quasiquotes.publicapi.PublicFailure
import quasiquotes.parser.TermShape

final class PublicCoreExampleCompileTest extends munit.FunSuite:
  private def showMethod(
      methodName: String = "apply",
      typeParameterName: String = "A",
      occurrenceName: String = "A",
      contextualName: String = "instance"
  ): Either[PublicFailure, DefinitionResultView] =
    for
      show <- CompletedType.named("Show")
      parameter <- CompletedType.typeParameter(occurrenceName)
      showOfParameter <- CompletedType.applied(show, Vector(parameter))
      instance <- CompletedTerm.reference("instance")
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
