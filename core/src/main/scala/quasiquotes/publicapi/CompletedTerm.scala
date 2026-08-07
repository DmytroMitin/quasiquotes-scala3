package quasiquotes.publicapi

/** A compiler-free completed stable reference. */
final class CompletedTerm private (val referenceName: String) derives CanEqual:
  def kindCode: String = "reference"
  def source: String = referenceName

  override def equals(other: Any): Boolean =
    other match
      case that: CompletedTerm => referenceName == that.referenceName
      case _ => false

  override def hashCode: Int = referenceName.hashCode

  override def toString: String = source

object CompletedTerm:
  def reference(name: String): Either[PublicFailure, CompletedTerm] =
    Either.cond(
      name != null && PublicIdentifier.isValid(name),
      new CompletedTerm(name),
      PublicFailure.invalidName(String.valueOf(name), FailureAnchor.Body)
    )
