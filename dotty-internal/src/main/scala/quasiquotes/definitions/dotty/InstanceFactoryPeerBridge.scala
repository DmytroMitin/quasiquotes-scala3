package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  NeutralProjectionError,
  ScalametaInstanceFactoryProjection,
  ScalametaTypeMemberInstanceFactoryProjection,
  ScalametaValueInstanceFactoryProjection
}

import scala.meta.*

/** Exact-version bridge for the bounded instance-factory family. */
object InstanceFactoryPeerBridge:
  /** Stable diagnostic boundary for the bounded peer operation. */
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
   * Validates and lowers one of three closed Scalameta instance-factory families.
   * Authoring, target admission, placement, rollback, and ordinary typing
   * remain consumer-owned.
   */
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    ScalametaInstanceFactoryProjection.project(definition) match
      case Right(projected) =>
        InstanceFactoryGeneratedOriginAdapter
          .lower(projected.plan, virtualSourceName)
          .left
          .map(classifyLoweringFailure)
          .flatMap(complete)
      case Left(oldProblem) =>
        val oldFailure = classifyProjectionFailure(oldProblem)
        selectSiblingFamily(definition) match
          case Some(true) =>
            for
              projected <- ScalametaValueInstanceFactoryProjection
                .project(definition)
                .left
                .map(classifyValueProjectionFailure)
              positioned <- ValueInstanceFactoryGeneratedOriginAdapter
                .lower(projected.plan, virtualSourceName)
                .left
                .map(problem =>
                  classifySiblingLoweringFailure(problem.code, problem.detail)
                )
              result <- complete(positioned)
            yield result
          case Some(false) =>
            for
              projected <- ScalametaTypeMemberInstanceFactoryProjection
                .project(definition)
                .left
                .map(classifyTypeMemberProjectionFailure)
              positioned <- TypeMemberInstanceFactoryGeneratedOriginAdapter
                .lower(projected.plan, virtualSourceName)
                .left
                .map(problem =>
                  classifySiblingLoweringFailure(problem.code, problem.detail)
                )
              result <- complete(positioned)
            yield result
          case None => Left(oldFailure)

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

  private def classifyValueProjectionFailure(
      problem: NeutralProjectionError
  ): Failure =
    val code = problem.code match
      case "NEUTRAL_VALUE_INSTANCE_FACTORY_MISSING" =>
        "INVALID_SCALAMETA_DEFINITION"
      case "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED" =>
        "INVALID_INSTANCE_FACTORY_NAME"
      case "NEUTRAL_VALUE_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_ANONYMOUS_REQUIRED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED" =>
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY"
      case "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_TYPE_MISMATCH" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH" |
          "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_TYPE_MISMATCH" =>
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE"
      case "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH" =>
        "INVALID_INSTANCE_FACTORY_TERM_ROLE"
      case _ => "NEUTRAL_PROJECTION_FAILED"
    Failure(code, s"${problem.code}: ${problem.detail}")

  private def classifyTypeMemberProjectionFailure(
      problem: NeutralProjectionError
  ): Failure =
    val code = problem.code match
      case "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_MISSING" =>
        "INVALID_SCALAMETA_DEFINITION"
      case "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED" =>
        "INVALID_INSTANCE_FACTORY_NAME"
      case "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_VALUE_CLAUSES_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_REFINEMENT_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_REQUIRED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED" =>
        "UNSUPPORTED_INSTANCE_FACTORY_TOPOLOGY"
      case "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_RHS_MISMATCH" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_NAME_MISMATCH" |
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_RHS_MISMATCH" =>
        "INVALID_INSTANCE_FACTORY_TYPE_ROLE"
      case _ => "NEUTRAL_PROJECTION_FAILED"
    Failure(code, s"${problem.code}: ${problem.detail}")

  private def classifySiblingLoweringFailure(
      code: String,
      detail: String
  ): Failure =
    code match
      case "GENERATED_ORIGIN_INVALID" =>
        Failure("INVALID_VIRTUAL_SOURCE_NAME", detail)
      case "EXACT_RAW_LOWERING_FAILED" | "EXACT_RAW_INVARIANT_FAILED" =>
        Failure("EXACT_RAW_LOWERING_FAILED", detail)
      case "GENERATED_ORIGIN_MISMATCH" =>
        Failure("GENERATED_ORIGIN_FAILED", detail)
      case other =>
        Failure("INTERNAL_INVARIANT_FAILED", s"$other: $detail")

  private def complete(
      positioned: GeneratedOriginDefinitionResult
  ): Either[Failure, Lowered] =
    positioned.tree match
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

  private def selectSiblingFamily(
      definition: Defn.Def
  ): Option[Boolean] =
    Option(definition).flatMap { present =>
      val anonymousMembers = present.body match
        case anonymous: Term.NewAnonymous => Some(anonymous.templ.stats)
        case _ => None
      present.paramClauseGroups match
        case List(group)
            if group.tparamClause.values.size == 1 &&
              hasOneStrictCarrierCandidate(group.paramClauses) &&
              anonymousMembers.exists {
                case List(_: Defn.Val) => true
                case _ => false
              } =>
          Some(true)
        case List(group)
            if group.tparamClause.values.size == 2 &&
              group.paramClauses.isEmpty &&
              hasOneRefinementAliasCandidate(present.decltpe) &&
              anonymousMembers.exists {
                case List(_: Defn.Type) => true
                case _ => false
              } =>
          Some(false)
        case _ => None
    }

  private def hasOneStrictCarrierCandidate(
      clauses: List[Term.ParamClause]
  ): Boolean =
    clauses match
      case List(clause) if clause.mod.isEmpty =>
        clause.values match
          case List(parameter) =>
            parameter.mods.isEmpty &&
              parameter.default.isEmpty &&
              parameter.decltpe.exists {
                case _: Type.ByName | _: Type.Repeated => false
                case _ => true
              }
          case _ => false
      case _ => false

  private def hasOneRefinementAliasCandidate(
      declared: Option[Type]
  ): Boolean =
    declared.exists {
      case Type.Refine(Some(_), List(_: Defn.Type)) => true
      case _ => false
    }
