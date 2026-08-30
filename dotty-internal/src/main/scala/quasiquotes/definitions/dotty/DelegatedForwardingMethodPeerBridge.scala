package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.ScalametaDelegatedForwardingMethodProjection

import scala.meta.Defn

/** Exact-version bridge for the one admitted AUXify-043 forwarding method. */
object DelegatedForwardingMethodPeerBridge:
  final case class Failure(code: String, detail: String) derives CanEqual

  final class Lowered private[dotty] (
      val tree: untpd.DefDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Lowers one already-authored exact 043 definition. Target admission,
   * derivation, placement, rollback, and ordinary typing remain peer-owned.
   */
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaDelegatedForwardingMethodProjection
        .project(definition)
        .left
        .map(problem => Failure(problem.code, problem.detail))
      positioned <- DelegatedForwardingMethodGeneratedOriginAdapter
        .lower(projected.plan, virtualSourceName)
        .left
        .map(problem => Failure(problem.code, problem.detail))
      result <- positioned.tree match
        case method: untpd.DefDef =>
          Right(
            new Lowered(
              method,
              positioned.generatedSource,
              positioned.virtualSourceName
            )
          )
        case other =>
          Left(
            Failure(
              "INTERNAL_INVARIANT_FAILED",
              s"the 043 backend returned ${other.getClass.getName}, not untpd.DefDef."
            )
          )
    yield result
