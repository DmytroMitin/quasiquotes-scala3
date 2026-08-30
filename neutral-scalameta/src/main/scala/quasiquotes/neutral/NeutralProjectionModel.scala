package quasiquotes.neutral

import quasiquotes.parser.TermShape
import quasiquotes.publicapi.DefinitionResultView

/** Exact source offsets known to belong to the input Scalameta tree. */
final case class NeutralSourceSpan(start: Int, end: Int) derives CanEqual

/**
 * The admitted project IR together with truthful source offsets when the
 * input tree owns them. `None` means generated or otherwise unpositioned.
 */
final case class ProjectedContextualMethod(
    result: DefinitionResultView,
    sourceSpan: Option[NeutralSourceSpan]
) derives CanEqual

/** A projected core Term shape with truthful root source offsets when present. */
final case class ProjectedTermShape(
    shape: TermShape,
    sourceSpan: Option[NeutralSourceSpan]
) derives CanEqual

/** Stable category plus deterministic detail for a rejected source shape. */
final case class NeutralProjectionError(code: String, detail: String)
    derives CanEqual:
  def message: String = s"$code: $detail"
