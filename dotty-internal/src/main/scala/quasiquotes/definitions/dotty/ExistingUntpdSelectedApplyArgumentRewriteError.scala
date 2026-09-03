package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdSelectedApplyArgumentRewriteError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
