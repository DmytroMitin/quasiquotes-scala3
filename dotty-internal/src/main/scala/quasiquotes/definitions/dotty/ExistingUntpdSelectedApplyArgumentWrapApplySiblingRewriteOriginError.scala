package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteOriginError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
