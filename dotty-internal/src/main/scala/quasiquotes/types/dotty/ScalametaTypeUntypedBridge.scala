package quasiquotes.types.dotty

import dotty.tools.dotc.ast.untpd

import quasiquotes.neutral.ScalametaTypeNormalFormProjection
import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer

import scala.meta.Type

/** Public exact-version facade for the bounded Scalameta Type route. */
object ScalametaTypeUntypedBridge:
  /** Stable diagnostic boundary for public programmatic lowering. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /**
   * Projects and lowers one admitted Scalameta Type to a fresh source-free
   * exact raw tree.
   */
  def lower(
      sourceType: Type
  ): Either[Failure, untpd.Tree] =
    Option(sourceType)
      .toRight(
        Failure(
          "MISSING_INPUT",
          "the Scalameta Type must be present."
        )
      )
      .flatMap(present =>
        ScalametaTypeNormalFormProjection
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
        CompletedTypeUntypedLowerer
          .lower(projected.normalForm)
          .left
          .map(problem =>
            Failure(
              "EXACT_LOWERING_FAILED",
              problem.message
            )
          )
      )
