package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdSelectedApplyArgumentWrapSelectedApplySiblingRewriteOriginError(
    code: String,
    detail: String
):
  def message: String = s"$code: $detail"
