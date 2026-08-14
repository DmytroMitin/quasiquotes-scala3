package external.consumer

// snippet:two-parameter-definition-first-use:start
import quasiquotes.publicapi.*

object TwoParameterDefinitionFirstUseSnippet:
  private val intType = CompletedType.named("Int")
  private val stringType = CompletedType.named("String")

  val first: Either[PublicFailure, TwoParameterMethodResultView] =
    for
      firstType <- intType
      secondType <- stringType
      parameter <- CompletedTerm.definitionParameterReference("x")
      method <- DefinitionConstruction.twoParameterMethod(
        "first", "x", firstType, "y", secondType, firstType, parameter
      )
    yield method

  val second: Either[PublicFailure, TwoParameterMethodResultView] =
    for
      firstType <- intType
      secondType <- stringType
      parameter <- CompletedTerm.definitionParameterReference("y")
      method <- DefinitionConstruction.twoParameterMethod(
        "second", "x", firstType, "y", secondType, secondType, parameter
      )
    yield method
// snippet:two-parameter-definition-first-use:end
