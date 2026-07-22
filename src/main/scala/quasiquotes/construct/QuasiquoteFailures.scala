package quasiquotes.construct

import quasiquotes.source.{DiagnosticLocation, SourceSpan}

private[construct] final case class QuasiquoteLoweringFailure(
    error: QuasiquoteError,
    generatedSpan: Option[SourceSpan]
) derives CanEqual

private[construct] final case class QuasiquoteBuildFailure(
    error: QuasiquoteError,
    location: Option[DiagnosticLocation]
) derives CanEqual
