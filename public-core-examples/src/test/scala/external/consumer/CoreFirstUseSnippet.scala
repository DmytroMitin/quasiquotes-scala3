package external.consumer

// snippet:core-first-use:start
import quasiquotes.publicapi.*

object CoreFirstUseSnippet:
  val method: Either[PublicFailure, DefinitionResultView] =
    for
      show <- CompletedType.named("Show")
      a <- CompletedType.typeParameter("A")
      showA <- CompletedType.applied(show, Vector(a))
      instance <- CompletedTerm.reference("instance")
      result <- DefinitionConstruction.contextualMethod(
        "apply", "A", "instance", showA, showA, instance
      )
    yield result
// snippet:core-first-use:end
