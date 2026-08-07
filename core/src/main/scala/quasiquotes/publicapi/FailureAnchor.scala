package quasiquotes.publicapi

/** A compact, read-only location within the bounded public construction call. */
final class FailureAnchor private (
    val componentCode: String,
    val ordinal: Option[Int]
) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: FailureAnchor =>
        componentCode == that.componentCode && ordinal == that.ordinal
      case _ => false

  override def hashCode: Int =
    (componentCode, ordinal).hashCode

  override def toString: String =
    ordinal.fold(componentCode)(index => s"$componentCode[$index]")

private[publicapi] object FailureAnchor:
  val MethodName = new FailureAnchor("method-name", None)
  val TypeParameter = new FailureAnchor("type-parameter", None)
  val TypeName = new FailureAnchor("type-name", None)
  val TypeApplication = new FailureAnchor("type-application", None)
  val ContextualParameterName =
    new FailureAnchor("contextual-parameter-name", None)
  val ContextualParameterType =
    new FailureAnchor("contextual-parameter-type", None)
  val ResultType = new FailureAnchor("result-type", None)
  val Body = new FailureAnchor("body", None)
