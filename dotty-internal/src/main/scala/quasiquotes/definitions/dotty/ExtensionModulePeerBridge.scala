package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  NeutralProjectionError,
  ScalametaExtensionModuleProjection
}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

import scala.meta.Defn

/** Exact-version bridge for the bounded extension-module family. */
object ExtensionModulePeerBridge:
  /** Stable public diagnostic boundary for the admitted operation. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Positioned insertion-ready module and deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.ModuleDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Lowers one already-authored exact extension module. Target admission,
   * placement, lifecycle, rollback, and ordinary typing remain consumer-owned.
   */
  def lower(
      definition: Defn.Object,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaExtensionModuleProjection
        .project(definition)
        .left
        .map(classifyProjectionFailure)
      _ <- validateVirtualSourceName(virtualSourceName)
      positioned <- ExtensionModulePlanU024Adapter
        .lowerGenerated(projected.plan, virtualSourceName)
        .left
        .map(classifyLoweringFailure)
      result <- positioned.tree match
        case module: untpd.ModuleDef =>
          Right(
            new Lowered(
              module,
              positioned.generatedSource,
              positioned.virtualSourceName
            )
          )
        case null =>
          Left(
            Failure(
              "INTERNAL_INVARIANT_FAILED",
              "the extension-module backend returned null instead of untpd.ModuleDef."
            )
          )
        case other =>
          Left(
            Failure(
              "INTERNAL_INVARIANT_FAILED",
              s"the extension-module backend returned ${other.getClass.getName}, not untpd.ModuleDef."
            )
          )
    yield result

  private def validateVirtualSourceName(
      virtualSourceName: String
  ): Either[Failure, Unit] =
    Option(virtualSourceName)
      .toRight(
        Failure(
          "INVALID_VIRTUAL_SOURCE_NAME",
          "the virtual source name must be present."
        )
      )
      .flatMap(name =>
        GeneratedOriginFragmentSupport
          .validateVirtualSourceName(name)
          .left
          .map(problem =>
            Failure("INVALID_VIRTUAL_SOURCE_NAME", problem.message)
          )
      )

  private def classifyProjectionFailure(
      problem: NeutralProjectionError
  ): Failure =
    val code = problem.code match
      case "NEUTRAL_EXTENSION_MODULE_MISSING" =>
        "INVALID_SCALAMETA_DEFINITION"
      case "NEUTRAL_EXTENSION_MODULE_NAME_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_TERM_ROLE_COLLISION" |
          "NEUTRAL_EXTENSION_MODULE_LEXICAL_ROLE_UNSUPPORTED" =>
        "INVALID_EXTENSION_MODULE_NAME_ROLE"
      case "NEUTRAL_EXTENSION_MODULE_RECEIVER_TYPE_MISMATCH" |
          "NEUTRAL_EXTENSION_MODULE_ORDINARY_TYPE_MISMATCH" |
          "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_EVIDENCE_TYPE_ARGUMENT_MISMATCH" |
          "NEUTRAL_EXTENSION_MODULE_RESULT_TYPE_MISMATCH" =>
        "INVALID_EXTENSION_MODULE_TYPE_ROLE"
      case "NEUTRAL_EXTENSION_MODULE_BODY_APPLICATION_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_BODY_SELECTION_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_BODY_RECEIVER_MISMATCH" |
          "NEUTRAL_EXTENSION_MODULE_BODY_SELECTED_METHOD_MISMATCH" |
          "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENTS_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_BODY_ARGUMENT_ROLE_MISMATCH" =>
        "INVALID_EXTENSION_MODULE_TERM_ROLE"
      case "NEUTRAL_EXTENSION_MODULE_OBJECT_TOPOLOGY_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_OBJECT_BODY_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_EXTENSION_REQUIRED" |
          "NEUTRAL_EXTENSION_MODULE_EXTENSION_PARAMETERS_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_TYPE_PARAMETER_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_RECEIVER_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_METHOD_REQUIRED" |
          "NEUTRAL_EXTENSION_MODULE_METHOD_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_METHOD_CLAUSES_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_METHOD_TYPE_PARAMETERS_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_ORDINARY_CLAUSE_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_ORDINARY_PARAMETER_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_CLAUSE_UNSUPPORTED" |
          "NEUTRAL_EXTENSION_MODULE_CONTEXTUAL_PARAMETER_UNSUPPORTED" =>
        "UNSUPPORTED_EXTENSION_MODULE_TOPOLOGY"
      case "NEUTRAL_EXTENSION_MODULE_PLAN_UNSUPPORTED" =>
        "INTERNAL_INVARIANT_FAILED"
      case _ => "INTERNAL_INVARIANT_FAILED"
    Failure(code, s"${problem.code}: ${problem.detail}")

  private def classifyLoweringFailure(
      problem: ExtensionModulePlanU024Adapter.Error
  ): Failure =
    problem.code match
      case "BOUNDED_PLAN_ADAPTATION_FAILED" |
          "GENERATED_ORIGIN_LOWERING_FAILED" =>
        Failure(
          "EXACT_OR_GENERATED_LOWERING_FAILED",
          s"${problem.code}: ${problem.detail}"
        )
      case "PLAN_REQUIRED" | "CORE_PLAN_MALFORMED" |
          "INTERNAL_INVARIANT_FAILED" =>
        Failure(
          "INTERNAL_INVARIANT_FAILED",
          s"${problem.code}: ${problem.detail}"
        )
      case other =>
        Failure(
          "INTERNAL_INVARIANT_FAILED",
          s"unexpected lowering failure $other: ${problem.detail}"
        )
