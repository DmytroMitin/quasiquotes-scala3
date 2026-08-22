package quasiquotes.definitions.dotty

private[quasiquotes] final case class ScalametaContextualMethodBackendError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
