package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.terms.ConstructedTerm

import scala.meta.Term

/** Exact-version generated-origin bridge for the bounded Scalameta Term family. */
object ScalametaTermGeneratedOriginBridge:
  /** Stable diagnostic boundary for projection, completion, and origin work. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Positioned insertion-ready term and its deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.Tree,
      val generatedSource: String,
      val sourceFile: SourceFile
  ):
    def virtualSourceName: String = sourceFile.path

    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Projects and lowers one admitted Scalameta term. Placement, ownership,
   * symbols, ordinary typing, and rollback remain caller-owned.
   */
  def lower(
      term: Term,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      input <- Option(term).toRight(
        Failure("MISSING_INPUT", "the Scalameta Term must be present.")
      )
      projected <- ScalametaTermProjection
        .project(input)
        .left
        .map(problem =>
          Failure(
            "NEUTRAL_PROJECTION_FAILED",
            s"${problem.code}: ${problem.detail}"
          )
        )
      completed <- ConstructedTerm
        .fromShape(projected.shape)
        .left
        .map(problem => Failure("TERM_COMPLETION_FAILED", problem.message))
      validatedVirtualSourceName <- Option(virtualSourceName).toRight(
        Failure("INVALID_VIRTUAL_SOURCE", "the name must be present")
      )
      positioned <- ConstructedTermGeneratedOriginAdapter
        .lower(completed, validatedVirtualSourceName)
        .left
        .map(classifyGeneratedOriginFailure)
    yield new Lowered(
      positioned.tree,
      positioned.generatedSource,
      positioned.sourceFile
    )

  private def classifyGeneratedOriginFailure(
      problem: ConstructedTermGeneratedOriginError
  ): Failure =
    problem match
      case ConstructedTermGeneratedOriginError.InvalidVirtualSourceName(detail) =>
        Failure("INVALID_VIRTUAL_SOURCE", detail)
      case other => Failure("GENERATED_ORIGIN_FAILED", other.message)
