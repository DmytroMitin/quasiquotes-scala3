package external.consumer

// snippet:definition-first-use:start
import quasiquotes.publicapi.*

object DefinitionFirstUseSnippet:
  val identity: Either[PublicFailure, SingleParameterMethodResultView] =
    for
      intType <- CompletedType.named("Int")
      parameter <- CompletedTerm.definitionParameterReference("x")
      method <- DefinitionConstruction.singleParameterMethod(
        "id", "x", intType, intType, parameter
      )
    yield method
// snippet:definition-first-use:end
