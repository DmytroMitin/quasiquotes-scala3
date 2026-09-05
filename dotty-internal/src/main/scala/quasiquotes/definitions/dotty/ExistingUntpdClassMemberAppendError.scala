package quasiquotes.definitions.dotty

private[quasiquotes] final case class ExistingUntpdClassMemberAppendError(
    code: String,
    detail: String
) derives CanEqual:
  def message: String = s"$code: $detail"
