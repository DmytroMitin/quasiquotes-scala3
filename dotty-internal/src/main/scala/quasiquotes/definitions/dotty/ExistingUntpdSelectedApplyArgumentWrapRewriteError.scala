package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdSelectedApplyArgumentWrapRewriteError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
