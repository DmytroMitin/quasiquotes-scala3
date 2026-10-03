package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan
import quasiquotes.definitions.TypeMemberInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedTypeMemberInstanceFactory(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-082 abstract-Type-member factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaTypeMemberInstanceFactoryProjection:
  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedTypeMemberInstanceFactory] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedTypeMemberInstanceFactory] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED",
        "the exact abstract-Type-member factory has no outer modifiers."
      )
      factoryName <- plainTermName(definition.name)
      group <- definition.paramClauseGroups match
        case List(value) => Right(value)
        case _ => Left(valueClausesUnsupported)
      _ <- require(group.paramClauses.isEmpty, valueClausesUnsupported)
      parameters <- projectTypeParameters(group.tparamClause.values)
      (firstTypeParameterName, secondTypeParameterName) = parameters
      result <- projectResult(
        definition.decltpe,
        firstTypeParameterName,
        secondTypeParameterName
      )
      (targetConstructorName, resultTarget, resultMemberName) = result
      anonymous <- definition.body match
        case value: Term.NewAnonymous => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_REQUIRED",
              "the factory body must be one Term.NewAnonymous implementation."
            )
          )
      anonymousMemberName <- projectTemplate(
        anonymous.templ,
        resultTarget,
        targetConstructorName,
        firstTypeParameterName,
        secondTypeParameterName
      )
      _ <- require(
        anonymousMemberName == resultMemberName,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_NAME_MISMATCH",
        "the anonymous concrete alias name must equal the result-refinement alias name."
      )
      plan <- TypeMemberInstanceFactoryPlan
        .create(
          factoryName,
          firstTypeParameterName,
          secondTypeParameterName,
          targetConstructorName,
          resultMemberName
        )
        .left
        .map(mapPlanError)
    yield ProjectedTypeMemberInstanceFactory(plan, truthfulSpan(definition))

  private def projectTypeParameters(
      parameters: List[Type.Param]
  ): Either[NeutralProjectionError, (String, String)] =
    parameters match
      case List(first, second) if exactTypeParameter(first) && exactTypeParameter(second) =>
        for
          firstName <- first.name match
            case name: Type.Name => plainTypeName(name)
            case _ => Left(typeParametersUnsupported)
          secondName <- second.name match
            case name: Type.Name => plainTypeName(name)
            case _ => Left(typeParametersUnsupported)
        yield firstName -> secondName
      case _ => Left(typeParametersUnsupported)

  private def exactTypeParameter(parameter: Type.Param): Boolean =
    parameter.mods.isEmpty &&
      parameter.tparamClause.values.isEmpty &&
      parameter.bounds.lo.isEmpty &&
      parameter.bounds.hi.isEmpty &&
      parameter.bounds.context.isEmpty &&
      parameter.bounds.view.isEmpty

  private def projectResult(
      declared: Option[Type],
      firstTypeParameterName: String,
      secondTypeParameterName: String
  ): Either[NeutralProjectionError, (String, Type.Apply, String)] =
    declared match
      case Some(Type.Refine(Some(base), stats)) =>
        for
          target <- projectTarget(
            base,
            firstTypeParameterName,
            "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
            "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH"
          )
          (constructorName, applied) = target
          member <- exactlyOne(
            stats,
            resultAliasUnsupported
          )
          memberName <- member match
            case alias: Defn.Type =>
              projectConcreteAlias(
                alias,
                secondTypeParameterName,
                resultAliasUnsupported,
                "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_RHS_MISMATCH"
              )
            case _ => Left(resultAliasUnsupported)
        yield (constructorName, applied, memberName)
      case _ => Left(resultRefinementUnsupported)

  private def projectTemplate(
      template: Template,
      resultTarget: Type.Apply,
      targetConstructorName: String,
      firstTypeParameterName: String,
      secondTypeParameterName: String
  ): Either[NeutralProjectionError, String] =
    for
      _ <- require(
        template.early.isEmpty &&
          template.derives.isEmpty &&
          template.self.isEmpty &&
          template.inits.size == 1,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED",
        "the anonymous template requires one parent and empty early/self/derives topology."
      )
      parent = template.inits.head
      _ <- require(
        parent.name.value.isEmpty && parent.argClauses.isEmpty,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_UNSUPPORTED",
        "the anonymous parent must have no constructor argument clauses."
      )
      parentTarget <- projectTarget(
        parent.tpe,
        firstTypeParameterName,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH",
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH"
      )
      (parentConstructorName, parentApplied) = parentTarget
      _ <- require(
        parentConstructorName == targetConstructorName && parentApplied.structure == resultTarget.structure,
        "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH",
        "the anonymous parent must match the exact result target."
      )
      member <- exactlyOne(template.stats, anonymousAliasUnsupported)
      memberName <- member match
        case alias: Defn.Type =>
          projectConcreteAlias(
            alias,
            secondTypeParameterName,
            anonymousAliasUnsupported,
            "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_RHS_MISMATCH"
          )
        case _ => Left(anonymousAliasUnsupported)
    yield memberName

  private def projectTarget(
      value: Type,
      firstTypeParameterName: String,
      unsupportedCode: String,
      mismatchCode: String
  ): Either[NeutralProjectionError, (String, Type.Apply)] =
    value match
      case target @ Type.Apply(constructor: Type.Name, List(argument: Type.Name)) =>
        for
          constructorName <- plainTypeName(constructor)
          argumentName <- plainTypeName(argument)
          _ <- require(
            argumentName == firstTypeParameterName,
            mismatchCode,
            "the target argument must reference the exact first factory Type parameter."
          )
        yield constructorName -> target
      case _ =>
        Left(
          error(
            unsupportedCode,
            "the target must be one direct source-named unary applied Type."
          )
        )

  private def projectConcreteAlias(
      alias: Defn.Type,
      secondTypeParameterName: String,
      topologyError: NeutralProjectionError,
      rhsMismatchCode: String
  ): Either[NeutralProjectionError, String] =
    if alias.mods.nonEmpty ||
        alias.tparamClause.values.nonEmpty ||
        alias.bounds.lo.nonEmpty ||
        alias.bounds.hi.nonEmpty ||
        alias.bounds.context.nonEmpty ||
        alias.bounds.view.nonEmpty
    then Left(topologyError)
    else
      for
        memberName <- plainTypeName(alias.name)
        rhsName <- alias.body match
          case name: Type.Name => plainTypeName(name)
          case _ =>
            Left(
              error(
                rhsMismatchCode,
                "the concrete alias RHS must be one direct Type name."
              )
            )
        _ <- require(
          rhsName == secondTypeParameterName,
          rhsMismatchCode,
          "the concrete alias RHS must reference the exact second factory Type parameter."
        )
      yield memberName

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
      problem: TypeMemberInstanceFactoryPlan.ModelError
  ): NeutralProjectionError =
    problem.code match
      case "TYPE_PARAMETER_NAMES_MUST_BE_DISTINCT" |
          "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE" |
          "MEMBER_SECOND_TYPE_PARAMETER_ROLE_COLLAPSE" =>
        error("NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED", problem.detail)
      case _ =>
        error(
          "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_PLAN_UNSUPPORTED",
          s"${problem.code}: ${problem.detail}"
        )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def exactlyOne[A](
      values: List[A],
      problem: NeutralProjectionError
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(problem)

  private def require(
      condition: Boolean,
      problem: NeutralProjectionError
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), problem)

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def typeParametersUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_TYPE_PARAMETERS_UNSUPPORTED",
      "the factory requires exactly two unmodified, invariant, unbounded, non-higher-kinded Type parameters."
    )

  private def valueClausesUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_VALUE_CLAUSES_UNSUPPORTED",
      "the factory must have exactly one Type-parameter clause group and no ordinary value clauses."
    )

  private def resultRefinementUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_REFINEMENT_UNSUPPORTED",
      "the explicit result must be one refinement with a present base and one concrete alias."
    )

  private def resultAliasUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_RESULT_ALIAS_UNSUPPORTED",
      "the result refinement must contain exactly one unmodified, non-parameterized concrete Defn.Type alias."
    )

  private def anonymousAliasUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_ANONYMOUS_ALIAS_UNSUPPORTED",
      "the anonymous body must contain exactly one unmodified, non-parameterized concrete Defn.Type alias."
    )

  private def nameUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_TYPE_MEMBER_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      "every abstract-Type-member factory role must use one exact plain fresh-representable source name."
    )

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
