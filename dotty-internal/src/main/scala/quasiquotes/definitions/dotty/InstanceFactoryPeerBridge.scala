package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  NeutralProjectionError,
  ScalametaInstanceFactoryProjection
}

import scala.meta.Defn

/** Exact-version bridge for the bounded instance-factory family. */
object InstanceFactoryPeerBridge:
  /** Stable diagnostic boundary for the one admitted peer operation. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Positioned insertion-ready factory and deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.DefDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Validates and lowers exactly one bounded Scalameta instance factory.
   * Authoring, target admission, placement, rollback, and ordinary typing
   * remain consumer-owned.
   */
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaInstanceFactoryProjection
        .project(definition)
        .left
        .map(classifyProjectionFailure)
      positioned <- InstanceFactoryGeneratedOriginAdapter
        .lower(projected.plan, virtualSourceName)
        .left
        .map(classifyLoweringFailure)
      result <- positioned.tree match
        case factory: untpd.DefDef =>
          Right(
            new Lowered(
              factory,
              positioned.generatedSource,
              positioned.virtualSourceName
            )
          )
        case other =>
          Left(
            Failure(
              "INTERNAL_INVARIANT_FAILED",
              s"the instance-factory backend returned ${other.getClass.getName}, not untpd.DefDef."
            )
          )
    yield result

  private def classifyProjectionFailure(
      problem: NeutralProjectionError
  ): Failure =
    val code =
      if problem.code == "DEFINITION_MISSING" then
        "INVALID_SCALAMETA_DEFINITION"
      else if problem.code.endsWith("_NAME_INVALID") then
        "INVALID_INSTANCE_FACTORY_NAME"
      else if problem.code.endsWith("_TOPOLOGY_UNSUPPORTED") ||
          problem.code == "ANONYMOUS_IMPLEMENTATION_REQUIRED"
      then "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY"
      else if problem.code.contains("_TYPE_") ||
          problem.code.startsWith("TARGET_TYPE_") ||
          problem.code.startsWith("PARENT_TARGET_")
      then "INVALID_INSTANCE_FACTORY_TYPE_ROLE"
      else if problem.code.contains("_BODY_") ||
          problem.code.contains("_CALLEE_") ||
          problem.code.contains("_ARGUMENT_") ||
          problem.code.contains("_BINDER_") ||
          problem.code.contains("_SCOPE_")
      then "INVALID_INSTANCE_FACTORY_TERM_ROLE"
      else "NEUTRAL_PROJECTION_FAILED"
    Failure(code, s"${problem.code}: ${problem.detail}")

  private def classifyLoweringFailure(
      problem: InstanceFactoryGeneratedOriginError
  ): Failure =
    problem.code match
      case "GENERATED_ORIGIN_INVALID" =>
        Failure("INVALID_VIRTUAL_SOURCE_NAME", problem.detail)
      case "EXACT_RAW_LOWERING_FAILED" =>
        Failure("EXACT_RAW_LOWERING_FAILED", problem.detail)
      case "GENERATED_ORIGIN_MISMATCH" =>
        Failure("GENERATED_ORIGIN_FAILED", problem.detail)
      case other =>
        Failure("INTERNAL_INVARIANT_FAILED", s"$other: ${problem.detail}")
