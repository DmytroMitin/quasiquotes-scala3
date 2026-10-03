package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ValueInstanceFactoryPlan
import quasiquotes.definitions.ValueInstanceFactoryPlan.Plan

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedValueInstanceFactory(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-081 one-value factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaValueInstanceFactoryProjection:
  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedValueInstanceFactory] =
    Option(definition)
      .toRight(
        error(
          "NEUTRAL_VALUE_INSTANCE_FACTORY_MISSING",
          "the Scalameta Defn.Def must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedValueInstanceFactory] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED",
        "the exact one-value factory has no outer modifiers."
      )
      factoryName <- plainTermName(definition.name)
      group <- exactlyOne(
        definition.paramClauseGroups,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
        "the exact one-value factory requires one parameter-clause group."
      )
      typeParameter <- projectTypeParameter(group.tparamClause.values)
      typeParameterName <- typeParameter.name match
        case name: Type.Name => plainTypeName(name)
        case _ => Left(typeParameterUnsupported)
      carrier <- projectCarrier(group.paramClauses)
      carrierName <- plainTermParameterName(carrier.name)
      _ <- requireDirectTypeReference(
        carrier.decltpe,
        typeParameterName,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_TYPE_MISMATCH",
        "the strict carrier Type"
      )
      target <- projectTarget(definition.decltpe, typeParameterName)
      (targetConstructorName, _) = target
      anonymous <- definition.body match
        case value: Term.NewAnonymous => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_VALUE_INSTANCE_FACTORY_ANONYMOUS_REQUIRED",
              "the factory body must be one Term.NewAnonymous implementation."
            )
          )
      member <- projectTemplate(
        anonymous.templ,
        targetConstructorName,
        typeParameterName
      )
      valueOverride <- member match
        case value: Defn.Val => Right(value)
        case _ => Left(memberUnsupported)
      _ <- require(
        valueOverride.mods match
          case List(_: Mod.Override) => true
          case _ => false,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED",
        "the anonymous member must have exactly the override modifier."
      )
      memberName <- valueOverride.pats match
        case List(Pat.Var(name: Term.Name)) => plainTermName(name)
        case _ =>
          Left(
            error(
              "NEUTRAL_VALUE_INSTANCE_FACTORY_VALUE_OVERRIDE_UNSUPPORTED",
              "the override must declare exactly one direct Pat.Var member."
            )
          )
      _ <- requireDirectTypeReference(
        valueOverride.decltpe,
        typeParameterName,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_TYPE_MISMATCH",
        "the override member Type"
      )
      rhsName <- valueOverride.rhs match
        case name: Term.Name => plainTermName(name)
        case _ =>
          Left(
            error(
              "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH",
              "the override initializer must be one direct Term name."
            )
          )
      _ <- require(
        rhsName == carrierName,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_RHS_ROLE_MISMATCH",
        "the override initializer must reference the exact outer strict carrier."
      )
      plan <- ValueInstanceFactoryPlan
        .create(
          factoryName,
          typeParameterName,
          carrierName,
          targetConstructorName,
          memberName
        )
        .left
        .map(mapPlanError)
    yield ProjectedValueInstanceFactory(plan, truthfulSpan(definition))

  private def projectTypeParameter(
      parameters: List[Type.Param]
  ): Either[NeutralProjectionError, Type.Param] =
    parameters match
      case List(parameter)
          if parameter.mods.isEmpty &&
            parameter.tparamClause.values.isEmpty &&
            parameter.bounds.lo.isEmpty &&
            parameter.bounds.hi.isEmpty &&
            parameter.bounds.context.isEmpty &&
            parameter.bounds.view.isEmpty =>
        Right(parameter)
      case _ => Left(typeParameterUnsupported)

  private def projectCarrier(
      clauses: List[Term.ParamClause]
  ): Either[NeutralProjectionError, Term.Param] =
    clauses match
      case List(clause) if clause.mod.isEmpty =>
        clause.values match
          case List(parameter)
              if parameter.mods.isEmpty &&
                parameter.default.isEmpty &&
                !hasUnsupportedParameterType(parameter) =>
            Right(parameter)
          case _ => Left(carrierUnsupported)
      case _ => Left(carrierUnsupported)

  private def projectTarget(
      declared: Option[Type],
      typeParameterName: String
  ): Either[NeutralProjectionError, (String, Type.Apply)] =
    declared match
      case Some(target @ Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        for
          constructorName <- plainTypeName(constructor)
          argumentName <- plainTypeName(argument)
          _ <- require(
            argumentName == typeParameterName,
            "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_MISMATCH",
            "the result target argument must reference the exact factory Type parameter."
          )
        yield constructorName -> target
      case _ =>
        Left(
          error(
            "NEUTRAL_VALUE_INSTANCE_FACTORY_RESULT_TARGET_UNSUPPORTED",
            "the result must be one direct source-named unary applied Type."
          )
        )

  private def projectTemplate(
      template: Template,
      targetConstructorName: String,
      typeParameterName: String
  ): Either[NeutralProjectionError, Stat] =
    for
      _ <- require(
        template.early.isEmpty &&
          template.derives.isEmpty &&
          template.self.isEmpty &&
          template.inits.size == 1,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_TEMPLATE_UNSUPPORTED",
        "the anonymous template requires one parent and empty early/self/derives topology."
      )
      parent = template.inits.head
      _ <- require(
        parent.name.value.isEmpty && parent.argClauses.isEmpty,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_UNSUPPORTED",
        "the anonymous parent must have no constructor argument clauses."
      )
      _ <- parent.tpe match
        case Type.Apply(constructor: Type.Name, List(argument: Type.Name)) =>
          for
            constructorName <- plainTypeName(constructor)
            argumentName <- plainTypeName(argument)
            _ <- require(
              constructorName == targetConstructorName &&
                argumentName == typeParameterName,
              "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH",
              "the anonymous parent must match the result target and exact Type parameter."
            )
          yield ()
        case _ =>
          Left(
            error(
              "NEUTRAL_VALUE_INSTANCE_FACTORY_PARENT_TARGET_MISMATCH",
              "the anonymous parent must be the same direct unary applied target as the result."
            )
          )
      member <- exactlyOne(
        template.stats,
        "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED",
        "the anonymous body must contain exactly one override-val member."
      )
    yield member

  private def requireDirectTypeReference(
      value: Option[Type],
      expected: String,
      code: String,
      role: String
  ): Either[NeutralProjectionError, Unit] =
    value match
      case Some(name: Type.Name) =>
        plainTypeName(name).flatMap { actual =>
          require(actual == expected, code, s"$role must reference the exact factory Type parameter.")
        }
      case _ => Left(error(code, s"$role must be one direct Type-parameter name."))

  private def plainTermParameterName(name: Name): Either[NeutralProjectionError, String] =
    name match
      case term: Term.Name => plainTermName(term)
      case _ => Left(nameUnsupported)

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

  private def hasUnsupportedParameterType(parameter: Term.Param): Boolean =
    parameter.decltpe.exists {
      case _: Type.ByName | _: Type.Repeated => true
      case _ => false
    }

  private def mapPlanError(
      problem: ValueInstanceFactoryPlan.ModelError
  ): NeutralProjectionError =
    problem.code match
      case "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE" | "CARRIER_MEMBER_ROLE_COLLAPSE" =>
        error("NEUTRAL_VALUE_INSTANCE_FACTORY_LEXICAL_ROLE_UNSUPPORTED", problem.detail)
      case _ =>
        error(
          "NEUTRAL_VALUE_INSTANCE_FACTORY_PLAN_UNSUPPORTED",
          s"${problem.code}: ${problem.detail}"
        )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def exactlyOne[A](
      values: List[A],
      code: String,
      detail: String
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(error(code, detail))

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def typeParameterUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_VALUE_INSTANCE_FACTORY_TYPE_PARAMETER_UNSUPPORTED",
      "the factory requires one unmodified, invariant, unbounded, non-higher-kinded Type parameter."
    )

  private def carrierUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_VALUE_INSTANCE_FACTORY_CARRIER_UNSUPPORTED",
      "the factory requires one ordinary, strict, unmodified, explicitly typed, non-defaulted carrier."
    )

  private def memberUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_VALUE_INSTANCE_FACTORY_MEMBER_UNSUPPORTED",
      "the anonymous body must contain exactly one immutable Defn.Val member."
    )

  private def nameUnsupported: NeutralProjectionError =
    error(
      "NEUTRAL_VALUE_INSTANCE_FACTORY_NAME_UNSUPPORTED",
      "every one-value factory role must use one exact plain fresh-representable source name."
    )

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
