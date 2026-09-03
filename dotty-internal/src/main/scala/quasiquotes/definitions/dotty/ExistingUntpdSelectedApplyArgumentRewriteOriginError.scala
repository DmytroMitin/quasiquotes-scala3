package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdSelectedApplyArgumentRewriteOriginError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
