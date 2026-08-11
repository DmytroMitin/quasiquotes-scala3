package external.consumer

// snippet:core-first-use:start
import quasiquotes.publicapi.*
import quasiquotes.parser.TermShape

object CoreFirstUseSnippet:
  val constructorShape = TermShape.New(
    "java.lang.StringBuilder",
    List(TermShape.Literal("16"))
  )

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
