package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.neutral.{
  ScalametaDelegatedForwardingMethodProjection,
  ScalametaParameterlessDelegatedForwardingMethodProjection
}

import scala.meta.{Defn, Mod, Term}

/** Exact-version bridge for the two admitted bounded forwarding methods. */
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
   * Lowers one already-authored admitted definition. Target admission, derivation,
   * placement, rollback, and ordinary typing remain peer-owned.
   */
  def lower(
      definition: Defn.Def,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    val oldProjection = ScalametaDelegatedForwardingMethodProjection.project(definition)
    val old = for
      projected <- oldProjection
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
    oldProjection match
      case Right(_) => old
      case Left(_) if selectsParameterlessEnvelope(definition) =>
        for
          projected <- ScalametaParameterlessDelegatedForwardingMethodProjection
            .project(definition)
            .left
            .map(problem => mapParameterlessProjectionFailure(problem.code, problem.detail))
          positioned <- ParameterlessDelegatedForwardingMethodGeneratedOriginAdapter
            .lower(projected.plan, virtualSourceName)
            .left
            .map(problem => mapParameterlessBackendFailure(problem.code, problem.detail))
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
                  s"the parameterless backend returned ${other.getClass.getName}, not untpd.DefDef."
                )
              )
        yield result
      case Left(problem) => Left(Failure(problem.code, problem.detail))

  private def selectsParameterlessEnvelope(definition: Defn.Def): Boolean =
    Option(definition).exists { present =>
      present.paramClauseGroups match
        case group :: Nil =>
          group.paramClauses match
            case clause :: Nil =>
              clause.mod match
                case Some(_: Mod.Using) =>
                  present.body match
                    case _: Term.Select => true
                    case _ => false
                case _ => false
            case _ => false
        case _ => false
    }

  private def mapParameterlessProjectionFailure(
      code: String,
      detail: String
  ): Failure =
    val publicCode = code match
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_MISSING" |
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_OUTER_TOPOLOGY_UNSUPPORTED" =>
        "DEFINITION_TOPOLOGY_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_PARAMETER_CLAUSE_GROUPS_UNSUPPORTED" |
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED" =>
        "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED" =>
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_CLAUSE_UNSUPPORTED" =>
        "CONTEXTUAL_CLAUSE_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED" =>
        "CONTEXTUAL_PARAMETER_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED" |
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_LEXICAL_ROLE_UNSUPPORTED" =>
        "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_ARGUMENT_MISMATCH" =>
        "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_UNSUPPORTED" |
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_BINDER_MISMATCH" =>
        "RESULT_TYPE_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTION_UNSUPPORTED" =>
        "BODY_SELECTION_UNSUPPORTED"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_RECEIVER_MISMATCH" =>
        "BODY_RECEIVER_BINDER_MISMATCH"
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTED_METHOD_MISMATCH" =>
        "BODY_SELECTED_METHOD_MISMATCH"
      // One private name error spans four semantic roles. Reusing any one
      // role-specific topology code would misclassify the other three.
      case "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_NAME_UNSUPPORTED" =>
        "NAME_UNSUPPORTED"
      case _ => "INTERNAL_INVARIANT_FAILED"
    if publicCode == "INTERNAL_INVARIANT_FAILED" then
      Failure(publicCode, s"$code: $detail")
    else Failure(publicCode, detail)

  private def mapParameterlessBackendFailure(
      code: String,
      detail: String
  ): Failure =
    code match
      case "GENERATED_ORIGIN_INVALID" => Failure(code, detail)
      case "INTERNAL_INVARIANT_FAILED" => Failure(code, detail)
      case _ => Failure("INTERNAL_INVARIANT_FAILED", s"$code: $detail")
