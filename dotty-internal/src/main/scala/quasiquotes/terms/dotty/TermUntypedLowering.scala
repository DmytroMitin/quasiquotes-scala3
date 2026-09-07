package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import quasiquotes.parser.TermShape

/** Exact-version source-free lowering for public semantic Term values. */
object TermUntypedLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Lowers one admitted `TermShape` through the richer completed-Term path. */
  def lower(
      term: TermShape
  )(using Context): Either[Failure, untpd.Tree] =
    CheckedTermUntypedLowering.lower(term).map(_.raw)
