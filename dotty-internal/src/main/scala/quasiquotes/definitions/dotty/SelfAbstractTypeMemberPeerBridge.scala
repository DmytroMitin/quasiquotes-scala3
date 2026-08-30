package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  NeutralProjectionError,
  ScalametaSelfAbstractTypeMemberProjection
}

import scala.meta.Decl

/**
 * Experimental exact-compiler bridge for the bounded AUXify-046 abstract
 * self-Type member. This is not an arbitrary Decl.Type or raw TypeDef API.
 */
object SelfAbstractTypeMemberPeerBridge:
  /** Deterministic diagnostic boundary for the admitted peer operation. */
  final case class Failure(code: String, detail: String) derives CanEqual

  /**
   * Positioned insertion-ready output and its generated-source provenance.
   * Construction remains owned by this exact-version backend.
   */
  final class Lowered private[dotty] (
      val tree: untpd.TypeDef,
      val generatedSource: String,
      val virtualSourceName: String
  ):
    override def toString: String =
      s"Lowered(source=$virtualSourceName, length=${generatedSource.length})"

  /**
   * Lowers exactly one coherent self abstract-Type member. The caller retains
   * trait admission, self-alias preparation, insertion, rollback, and typing.
   */
  def lower(
      declaration: Decl.Type,
      expectedMemberName: String,
      expectedSelfAliasName: String,
      expectedUpperBaseName: String,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      projected <- ScalametaSelfAbstractTypeMemberProjection
        .project(
          declaration,
          expectedMemberName,
          expectedSelfAliasName,
          expectedUpperBaseName
        )
        .left
        .map(classifyProjectionFailure)
      lowered <- SelfAbstractTypeMemberGeneratedOriginAdapter
        .lower(projected.plan, virtualSourceName)
        .left
        .map(classifyLoweringFailure)
      result <- finish(
        lowered.tree,
        lowered.generatedSource,
        lowered.virtualSourceName
      )
    yield result

  private def finish(
      tree: untpd.Tree,
      generatedSource: String,
      virtualSourceName: String
  ): Either[Failure, Lowered] =
    tree match
      case value: untpd.TypeDef =>
        Right(new Lowered(value, generatedSource, virtualSourceName))
      case other =>
        Left(
          Failure(
            "INTERNAL_INVARIANT_FAILED",
            s"the self-member backend returned `${other.getClass.getName}` instead of untpd.TypeDef."
          )
        )

  private def classifyProjectionFailure(
      problem: NeutralProjectionError
  ): Failure =
    val code =
      if problem.code.startsWith("NEUTRAL_SELF_MEMBER_EXPECTED_") ||
          problem.code == "NEUTRAL_SELF_MEMBER_EXPECTATION_INVALID"
      then "INVALID_EXPECTATION"
      else if problem.code == "NEUTRAL_SELF_MEMBER_DECLARATION_MISSING" then
        "INVALID_SCALAMETA_DECLARATION"
      else if problem.code.endsWith("_UNSUPPORTED") ||
          problem.code.endsWith("_MISSING") ||
          problem.code == "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON"
      then "UNSUPPORTED_SCALAMETA_SELF_TYPE_MEMBER"
      else "NEUTRAL_PROJECTION_FAILED"
    Failure(code, s"${problem.code}: ${problem.detail}")

  private def classifyLoweringFailure(
      problem: SelfAbstractTypeMemberGeneratedOriginError
  ): Failure =
    problem.code match
      case "INVALID_VIRTUAL_SOURCE_NAME" =>
        Failure("INVALID_VIRTUAL_SOURCE_NAME", problem.detail)
      case "EXACT_RAW_LOWERING_FAILED" =>
        Failure("EXACT_RAW_LOWERING_FAILED", problem.detail)
      case "GENERATED_ORIGIN_FAILED" =>
        Failure("GENERATED_ORIGIN_FAILED", problem.detail)
      case "INTERNAL_INVARIANT_FAILED" =>
        Failure("INTERNAL_INVARIANT_FAILED", problem.detail)
      case other => Failure("INTERNAL_INVARIANT_FAILED", s"$other: ${problem.detail}")
