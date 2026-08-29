package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  NeutralProjectionError,
  ProjectedContextualMethodRoute,
  ScalametaContextualMethodDispatch
}

import scala.meta.Defn

/**
 * Experimental exact-compiler bridge for one tightly coupled peer operation.
 *
 * This is not a stable raw quasiquote family. Its signatures expose Dotty
 * compiler internals and therefore require the consumer to use the exact same
 * Scala compiler version as this artifact.
 */
object ContextualMethodPeerBridge:
  /** Deterministic diagnostic boundary for the admitted peer operation. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /**
   * Positioned insertion-ready output and its generated-source provenance.
   * Construction remains owned by this exact-version backend.
   */
  final class Lowered private[dotty] (
      val tree: untpd.DefDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Lowers exactly one admitted Scalameta contextual method to positioned raw
   * Dotty syntax. The caller retains companion placement, insertion, rollback,
   * and ordinary typing ownership.
   */
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaContextualMethodDispatch
        .project(definition)
        .left
        .map(classifyProjectionFailure)
      _ <- Option(virtualSourceName)
        .toRight(
          Failure(
            "INVALID_VIRTUAL_SOURCE_NAME",
            "the virtual source name must be present."
          )
        )
      lowered <- projected match
        case ProjectedContextualMethodRoute.Legacy(value) =>
          PublicContextualMethodGeneratedOriginAdapter
            .lower(value.result, virtualSourceName)
            .left
            .map(classifyLoweringFailure)
            .flatMap(result =>
              finish(result.tree, result.generatedSource, result.virtualSourceName)
            )
        case ProjectedContextualMethodRoute.Scoped037(value) =>
          ScopedContextualMethodGeneratedOriginAdapter
            .lower(value.plan, virtualSourceName)
            .left
            .map(classifyScopedLoweringFailure)
            .flatMap(result =>
              finish(result.tree, result.generatedSource, result.virtualSourceName)
            )
    yield lowered

  private def finish(
      tree: untpd.Tree,
      generatedSource: String,
      virtualSourceName: String
  ): Either[Failure, Lowered] =
    tree match
      case value: untpd.DefDef =>
        Right(new Lowered(value, generatedSource, virtualSourceName))
      case other =>
        Left(
          Failure(
            "INTERNAL_INVARIANT_FAILED",
            s"the contextual-method backend returned `${other.getClass.getName}` instead of untpd.DefDef."
          )
        )

  private def classifyProjectionFailure(
      error: NeutralProjectionError
  ): Failure =
    val code =
      if error.code == "NEUTRAL_DEFINITION_MISSING" then
        "INVALID_SCALAMETA_DEFINITION"
      else if error.code.endsWith("_UNSUPPORTED") || error.code.endsWith("_MISSING")
      then "UNSUPPORTED_SCALAMETA_CONTEXTUAL_METHOD"
      else "NEUTRAL_PROJECTION_FAILED"
    Failure(code, s"${error.code}: ${error.detail}")

  private def classifyLoweringFailure(
      error: PublicContextualMethodGeneratedOriginError
  ): Failure =
    import PublicContextualMethodGeneratedOriginError.*

    error match
      case InvalidVirtualSourceName(detail) =>
        Failure("INVALID_VIRTUAL_SOURCE_NAME", detail)
      case RawLoweringFailure(detail) =>
        Failure("EXACT_RAW_LOWERING_FAILED", detail)
      case ProjectionPlanningFailure(detail) =>
        Failure("GENERATED_ORIGIN_FAILED", detail)
      case RawTreePlanMismatch(detail) =>
        Failure("GENERATED_ORIGIN_FAILED", detail)
      case IncompletePositionMap(detail) =>
        Failure("GENERATED_ORIGIN_FAILED", detail)

  private def classifyScopedLoweringFailure(
      error: ScopedContextualMethodGeneratedOriginError
  ): Failure =
    Failure(error.code, error.detail)
