package quasiquotes.publicapi

/** A compiler-free completed stable reference. */
final class CompletedTerm private (
    val referenceName: String,
    private val definitionParameter: Boolean
) derives CanEqual:
  def kindCode: String =
    if definitionParameter then "definition-parameter-reference"
    else "reference"

  def source: String = referenceName

  private[publicapi] def isDefinitionParameterReference: Boolean =
    definitionParameter

  override def equals(other: Any): Boolean =
    other match
      case that: CompletedTerm =>
        referenceName == that.referenceName &&
          definitionParameter == that.definitionParameter
      case _ => false

  override def hashCode: Int =
    (referenceName, definitionParameter).hashCode

  override def toString: String = source

object CompletedTerm:
  def reference(name: String): Either[PublicFailure, CompletedTerm] =
    createReference(name, definitionParameter = false)

  /** An explicit reference to the ordinary parameter of a definition call. */
  def definitionParameterReference(
      name: String
  ): Either[PublicFailure, CompletedTerm] =
    createReference(name, definitionParameter = true)

  private def createReference(
      name: String,
      definitionParameter: Boolean
  ): Either[PublicFailure, CompletedTerm] =
    Either.cond(
      name != null && PublicIdentifier.isValid(name),
      new CompletedTerm(name, definitionParameter),
      PublicFailure.invalidName(String.valueOf(name), FailureAnchor.Body)
    )
