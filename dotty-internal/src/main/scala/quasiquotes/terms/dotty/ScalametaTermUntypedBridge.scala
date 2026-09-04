package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.ScalametaTermProjection

import scala.meta.Term

/** Public exact-version facade for the bounded direct Scalameta Term route. */
object ScalametaTermUntypedBridge:
  /** Stable diagnostic boundary for public programmatic lowering. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /**
   * Projects and lowers one admitted Scalameta Term to a fresh source-free
   * exact raw tree.
   */
  def lower(
      term: Term
  )(using Context): Either[Failure, untpd.Tree] =
    Option(term)
      .toRight(
        Failure(
          "MISSING_INPUT",
          "the Scalameta Term must be present."
        )
      )
      .flatMap(present =>
        ScalametaTermProjection
          .project(present)
          .left
          .map(problem =>
            Failure(
              "NEUTRAL_PROJECTION_FAILED",
              s"${problem.code}: ${problem.detail}"
            )
          )
      )
      .flatMap(projected =>
        CoreTermShapeUntypedLowerer
          .lower(projected.shape)
          .left
          .map(problem =>
            Failure(
              "EXACT_LOWERING_FAILED",
              problem.message
            )
          )
      )
