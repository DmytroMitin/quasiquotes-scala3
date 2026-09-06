package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.ScalametaDefinitionProjection

import scala.meta.Defn

/** Exact-version source-free bridge for the bounded reusable Definition families. */
object ScalametaDefinitionUntypedBridge:
  /** Stable diagnostic boundary for projection and exact lowering. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /**
   * Projects and lowers one admitted Scalameta definition to a fresh source-free
   * raw member. Placement, generated origin, typing, and lifecycle remain
   * caller-owned.
   */
  def lower(
      definition: Defn
  )(using Context): Either[Failure, untpd.MemberDef] =
    for
      input <- Option(definition).toRight(
        Failure("MISSING_INPUT", "the Scalameta Defn must be present.")
      )
      projected <- ScalametaDefinitionProjection
        .projectShape(input)
        .left
        .map(problem =>
          Failure(
            "NEUTRAL_PROJECTION_FAILED",
            s"${problem.code}: ${problem.detail}"
          )
        )
      raw <- DefinitionShapeUntypedLowerer
        .lower(projected.shape)
        .left
        .map(problem => Failure("EXACT_LOWERING_FAILED", problem.message))
      member <- raw match
        case value: untpd.MemberDef => Right(value)
        case other =>
          Left(
            Failure(
              "EXACT_LOWERING_FAILED",
              s"exact Definition lowering returned ${other.getClass.getName}, not untpd.MemberDef."
            )
          )
    yield member
