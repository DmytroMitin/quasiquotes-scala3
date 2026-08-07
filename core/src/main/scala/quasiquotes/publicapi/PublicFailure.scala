package quasiquotes.publicapi

/** Stable failure identity with presentation-only message text. */
final class PublicFailure private (
    val code: String,
    val message: String,
    val anchor: Option[FailureAnchor]
) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: PublicFailure =>
        code == that.code && anchor == that.anchor
      case _ => false

  override def hashCode: Int =
    (code, anchor).hashCode

  override def toString: String =
    anchor match
      case Some(value) => s"$code at $value: $message"
      case None => s"$code: $message"

private[publicapi] object PublicFailure:
  def invalidName(value: String, anchor: FailureAnchor): PublicFailure =
    new PublicFailure(
      "invalid-name",
      s"Expected a plain ASCII Scala identifier, but received `$value`.",
      Some(anchor)
    )

  def invalidTypeApplication(detail: String): PublicFailure =
    new PublicFailure(
      "invalid-type-application",
      detail,
      Some(FailureAnchor.TypeApplication)
    )

  def undeclaredTypeParameter(
      name: String,
      anchor: FailureAnchor
  ): PublicFailure =
    new PublicFailure(
      "undeclared-type-parameter",
      s"Type parameter `$name` is not declared by this contextual method.",
      Some(anchor)
    )

  def invalidContextualMethodContract(
      detail: String,
      anchor: FailureAnchor
  ): PublicFailure =
    new PublicFailure(
      "invalid-contextual-method-contract",
      detail,
      Some(anchor)
    )

  def internalInvariant(detail: String): PublicFailure =
    new PublicFailure("internal-invariant", detail, None)
