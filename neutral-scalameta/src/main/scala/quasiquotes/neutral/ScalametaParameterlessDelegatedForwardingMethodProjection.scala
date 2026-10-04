package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan
import quasiquotes.definitions.ParameterlessDelegatedForwardingPlan.Plan

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedParameterlessDelegatedForwardingMethod(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-083 parameterless forwarder. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaParameterlessDelegatedForwardingMethodProjection:
  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedParameterlessDelegatedForwardingMethod] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedParameterlessDelegatedForwardingMethod] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_OUTER_TOPOLOGY_UNSUPPORTED",
        "the exact parameterless delegated forwarder has no outer modifiers."
      )
      methodName <- plainTermName(definition.name)
      group <- definition.paramClauseGroups match
        case List(value) => Right(value)
        case _ => Left(parameterClauseGroupsUnsupported)
      typeParameterName <- projectTypeParameter(group.tparamClause.values)
      clause <- group.paramClauses match
        case List(value) => Right(value)
        case _ => Left(valueClausesUnsupported)
      _ <- clause.mod match
        case Some(_: Mod.Using) => Right(())
        case _ => Left(contextualClauseUnsupported)
      parameter <- clause.values match
        case List(value)
            if value.mods match
              case List(_: Mod.Using) => value.default.isEmpty
              case _ => false =>
          Right(value)
        case _ => Left(contextualParameterUnsupported)
      contextualParameterName <- parameter.name match
        case name: Term.Name => plainTermName(name)
        case _ => Left(contextualParameterUnsupported)
      contextualType <- projectContextualType(parameter.decltpe, typeParameterName)
      (contextualConstructorName, _) = contextualType
      resultTypeName <- projectResult(definition.decltpe)
      _ <- require(
        resultTypeName == typeParameterName,
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_BINDER_MISMATCH",
        "the result Type must reference the exact method Type parameter."
      )
      body <- definition.body match
        case Term.Select(receiver: Term.Name, selected: Term.Name) =>
          for
            receiverName <- plainTermName(receiver)
            selectedName <- plainTermName(selected)
          yield receiverName -> selectedName
        case _ => Left(bodySelectionUnsupported)
      (receiverName, selectedName) = body
      _ <- require(
        receiverName == contextualParameterName,
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_RECEIVER_MISMATCH",
        "the selection receiver must reference the exact contextual parameter."
      )
      _ <- require(
        selectedName == methodName,
        "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTED_METHOD_MISMATCH",
        "the selected member must have the exact generated method source name."
      )
      plan <- ParameterlessDelegatedForwardingPlan
        .create(
          methodName,
          typeParameterName,
          contextualParameterName,
          contextualConstructorName
        )
        .left
        .map(mapPlanError)
    yield ProjectedParameterlessDelegatedForwardingMethod(plan, truthfulSpan(definition))

  private def projectTypeParameter(
      parameters: List[Type.Param]
  ): Either[NeutralProjectionError, String] =
    parameters match
      case List(parameter) if exactTypeParameter(parameter) =>
        parameter.name match
          case name: Type.Name => plainTypeName(name)
          case _ => Left(typeParameterUnsupported)
      case _ => Left(typeParameterUnsupported)

  private def exactTypeParameter(parameter: Type.Param): Boolean =
    parameter.mods.isEmpty &&
      parameter.tparamClause.values.isEmpty &&
      parameter.bounds.lo.isEmpty &&
      parameter.bounds.hi.isEmpty &&
      parameter.bounds.context.isEmpty &&
      parameter.bounds.view.isEmpty

  private def projectContextualType(
      declared: Option[Type],
      typeParameterName: String
  ): Either[NeutralProjectionError, (String, String)] =
    declared match
      case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        for
          constructorName <- plainTypeName(constructor)
          argumentName <- plainTypeName(argument)
          _ <- require(
            argumentName == typeParameterName,
            "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_ARGUMENT_MISMATCH",
            "the evidence Type argument must reference the exact method Type parameter."
          )
        yield constructorName -> argumentName
      case _ => Left(contextualTypeUnsupported)

  private def projectResult(
      declared: Option[Type]
  ): Either[NeutralProjectionError, String] =
    declared match
      case Some(name: Type.Name) => plainTypeName(name)
      case _ =>
        Left(
          error(
            "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_RESULT_TYPE_UNSUPPORTED",
            "the explicit result must be one direct Type name."
          )
        )

  private def plainTermName(name: Term.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection
      .project(name)
      .left
      .map(_ => nameUnsupported)
      .flatMap(requirePlainName)

  private def plainTypeName(name: Type.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection
      .project(name)
      .left
      .map(_ => nameUnsupported)
      .flatMap(requirePlainName)

  private def requirePlainName(name: DefinitionName): Either[NeutralProjectionError, String] =
    Either.cond(name.source == name.decoded, name.source, nameUnsupported)

  private def mapPlanError(
      problem: ParameterlessDelegatedForwardingPlan.ModelError
  ): NeutralProjectionError =
    problem.code match
      case "CONTEXTUAL_TYPE_CONSTRUCTOR_ROLE_COLLAPSE" =>
        error(
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_LEXICAL_ROLE_UNSUPPORTED",
          problem.detail
        )
      case _ =>
        error(
          "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_PLAN_UNSUPPORTED",
          problem.code + ": " + problem.detail
        )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def parameterClauseGroupsUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_PARAMETER_CLAUSE_GROUPS_UNSUPPORTED",
      "the forwarder requires exactly one parameter-clause group."
    )

  private def typeParameterUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_TYPE_PARAMETER_UNSUPPORTED",
      "the forwarder requires exactly one unmodified, invariant, unbounded, non-higher-kinded Type parameter."
    )

  private def valueClausesUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_VALUE_CLAUSES_UNSUPPORTED",
      "the forwarder requires exactly one value clause and it must be the contextual clause."
    )

  private def contextualClauseUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_CLAUSE_UNSUPPORTED",
      "the sole value clause must carry exactly the structural using marker."
    )

  private def contextualParameterUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_PARAMETER_UNSUPPORTED",
      "the using clause requires exactly one parameter with only Mod.Using and no default."
    )

  private def contextualTypeUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_CONTEXTUAL_TYPE_UNSUPPORTED",
      "the contextual parameter Type must be one direct source-named unary application."
    )

  private def bodySelectionUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_BODY_SELECTION_UNSUPPORTED",
      "the body must be one direct stable Term.Select with no Apply or wrapper."
    )

  private def nameUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_PARAMETERLESS_DELEGATED_FORWARDING_NAME_UNSUPPORTED",
      "every parameterless delegated-forwarding role must use an exact plain fresh-representable source name."
    )

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
