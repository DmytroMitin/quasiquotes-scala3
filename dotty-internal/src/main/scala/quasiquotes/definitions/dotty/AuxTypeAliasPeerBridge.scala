package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.{
  AuxTypeAliasExpectation,
  AuxTypeParameterExpectation
}
import quasiquotes.neutral.{
  NeutralProjectionError,
  ScalametaAuxTypeAliasProjection
}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

import scala.meta.Defn

/** Exact-version bridge for the one admitted AUXify-039 Type-alias family. */
object AuxTypeAliasPeerBridge:
  /** Deterministic first-boundary diagnostic for the admitted operation. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /** Positioned insertion-ready TypeDef and its generated-source provenance. */
  final class Lowered private[dotty] (
      val tree: untpd.TypeDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Lowers one already-authored exact AUXify-039 alias. Fresh-name derivation,
   * target admission, placement, lifecycle, rollback, and ordinary typing
   * remain consumer-owned.
   */
  def lower(
      definition: Defn.Type,
      expectedAliasName: String,
      expectedFirstParameterName: String,
      expectedFirstUpperBoundName: String,
      expectedSecondParameterName: String,
      expectedSecondUpperBoundName: String,
      expectedOutputParameterName: String,
      expectedOutputUpperBoundName: String,
      expectedTargetName: String,
      expectedRefinementMemberName: String,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaAuxTypeAliasProjection
        .project(
          definition,
          AuxTypeAliasExpectation(
            aliasName = expectedAliasName,
            firstParameter = AuxTypeParameterExpectation(
              expectedFirstParameterName,
              expectedFirstUpperBoundName
            ),
            secondParameter = AuxTypeParameterExpectation(
              expectedSecondParameterName,
              expectedSecondUpperBoundName
            ),
            outputParameter = AuxTypeParameterExpectation(
              expectedOutputParameterName,
              expectedOutputUpperBoundName
            ),
            targetName = expectedTargetName,
            refinementMemberName = expectedRefinementMemberName
          )
        )
        .left
        .map(classifyProjectionFailure)
      _ <- validateVirtualSourceName(virtualSourceName)
      validated <- AuxTypeAliasPlanUntypedInputAdapter
        .adapt(projected.plan)
        .left
        .map(problem =>
          Failure(
            "INTERNAL_INVARIANT_FAILED",
            s"${problem.code}: ${problem.detail}"
          )
        )
      positioned <- AuxTypeAliasGeneratedOriginAdapter
        .lower(validated, virtualSourceName)
        .left
        .map(classifyLoweringFailure)
      result <- positioned.tree match
        case alias: untpd.TypeDef =>
          Right(
            new Lowered(
              alias,
              positioned.generatedSource,
              positioned.virtualSourceName
            )
          )
        case other =>
          Left(
            Failure(
              "INTERNAL_INVARIANT_FAILED",
              s"the AUXify-039 backend returned ${other.getClass.getName}, not untpd.TypeDef."
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
    Failure(problem.code, problem.detail)

  private def classifyLoweringFailure(
      problem: AuxTypeAliasUntypedLoweringError
  ): Failure =
    problem.code match
      case "EXACT_RAW_LOWERING_FAILED" =>
        Failure("EXACT_RAW_LOWERING_FAILED", problem.detail)
      case "GENERATED_ORIGIN_FAILED" =>
        Failure("GENERATED_ORIGIN_FAILED", problem.detail)
      case other =>
        Failure("INTERNAL_INVARIANT_FAILED", s"$other: ${problem.detail}")
