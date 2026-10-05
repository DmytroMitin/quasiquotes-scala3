package quasiquotes.neutral

import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan
import quasiquotes.definitions.CurriedMethodInstanceFactoryPlan.Plan
import quasiquotes.definitions.DefinitionName

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedCurriedMethodInstanceFactory(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-087 curried-method factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaCurriedMethodInstanceFactoryProjection:
  def project(definition: Defn.Def): Either[NeutralProjectionError, ProjectedCurriedMethodInstanceFactory] =
    Option(definition).toRight(error("NEUTRAL_CURRIED_METHOD_FACTORY_MISSING", "the Scalameta Defn.Def must be present.")).flatMap(projectPresent)

  private def projectPresent(definition: Defn.Def): Either[NeutralProjectionError, ProjectedCurriedMethodInstanceFactory] =
    for
      _ <- require(definition.mods.isEmpty, "NEUTRAL_CURRIED_METHOD_FACTORY_OUTER_TOPOLOGY_UNSUPPORTED", "the exact factory has no outer modifiers.")
      factory <- plainTermName(definition.name)
      group <- exactlyOne(definition.paramClauseGroups, "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED", "the exact factory requires one parameter-clause group.")
      typeParameter <- projectTypeParameter(group.tparamClause.values)
      typeName <- typeParameter.name match
        case name: Type.Name => plainTypeName(name)
        case _ => Left(typeParameterUnsupported)
      carrier <- projectCarrier(group.paramClauses)
      carrierName <- plainParameterName(carrier.name)
      _ <- projectCarrierType(carrier.decltpe, typeName)
      target <- projectTarget(definition.decltpe, typeName)
      (targetName, _) = target
      anonymous <- definition.body match
        case value: Term.NewAnonymous => Right(value)
        case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_ANONYMOUS_REQUIRED", "the body must be one anonymous implementation."))
      member <- projectTemplate(anonymous.templ, targetName, typeName)
      method <- member match
        case value: Defn.Def => Right(value)
        case _ => Left(memberUnsupported)
      _ <- require(
        method.mods match
          case List(_: Mod.Override) => true
          case _ => false,
        "NEUTRAL_CURRIED_METHOD_FACTORY_OVERRIDE_UNSUPPORTED",
        "the generated method must have exactly the override modifier."
      )
      memberName <- plainTermName(method.name)
      clauses <- projectMemberClauses(method.paramClauseGroups)
      (first, second) = clauses
      firstName <- plainParameterName(first.name)
      secondName <- plainParameterName(second.name)
      _ <- requireDirectType(first.decltpe, typeName)
      _ <- requireDirectType(second.decltpe, typeName)
      _ <- requireDirectType(method.decltpe, typeName)
      bodyRoles <- projectBody(method.body)
      (callee, firstArgument, secondArgument) = bodyRoles
      _ <- require(
        callee == carrierName && firstArgument == firstName && secondArgument == secondName,
        "NEUTRAL_CURRIED_METHOD_FACTORY_BODY_ROLE_MISMATCH",
        "the nested application must reference the exact carrier, first parameter, and second parameter in order."
      )
      plan <- CurriedMethodInstanceFactoryPlan
        .create(factory, typeName, carrierName, targetName, memberName, firstName, secondName)
        .left
        .map(mapPlanError)
    yield ProjectedCurriedMethodInstanceFactory(plan, truthfulSpan(definition))

  private def projectTypeParameter(parameters: List[Type.Param]): Either[NeutralProjectionError, Type.Param] =
    parameters match
      case List(parameter)
          if parameter.mods.isEmpty && parameter.tparamClause.values.isEmpty &&
            parameter.bounds.lo.isEmpty && parameter.bounds.hi.isEmpty &&
            parameter.bounds.context.isEmpty && parameter.bounds.view.isEmpty => Right(parameter)
      case _ => Left(typeParameterUnsupported)

  private def projectCarrier(clauses: List[Term.ParamClause]): Either[NeutralProjectionError, Term.Param] =
    clauses match
      case List(clause) if clause.mod.isEmpty =>
        clause.values match
          case List(parameter) if ordinaryParameter(parameter) => Right(parameter)
          case _ => Left(carrierUnsupported)
      case _ => Left(carrierUnsupported)

  private def projectCarrierType(value: Option[Type], expected: String): Either[NeutralProjectionError, Unit] =
    value match
      case Some(Type.Function(List(first: Type.Name), Type.Function(List(second: Type.Name), result: Type.Name))) =>
        for
          firstName <- plainTypeName(first)
          secondName <- plainTypeName(second)
          resultName <- plainTypeName(result)
          _ <- require(
            firstName == expected && secondName == expected && resultName == expected,
            "NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_TYPE_MISMATCH",
            "the carrier Type must be exactly A => (A => A) over the factory Type parameter."
          )
        yield ()
      case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_TYPE_MISMATCH", "the carrier Type must be one explicitly nested unary function Type."))

  private def projectTarget(declared: Option[Type], typeName: String): Either[NeutralProjectionError, (String, Type.Apply)] =
    declared match
      case Some(target @ Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        for
          constructorName <- plainTypeName(constructor)
          argumentName <- plainTypeName(argument)
          _ <- require(argumentName == typeName, "NEUTRAL_CURRIED_METHOD_FACTORY_RESULT_TARGET_MISMATCH", "the result target argument must be the exact Type parameter.")
        yield constructorName -> target
      case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_RESULT_TARGET_UNSUPPORTED", "the result must be one direct source-named unary applied Type."))

  private def projectTemplate(template: Template, target: String, typeName: String): Either[NeutralProjectionError, Stat] =
    for
      _ <- require(
        template.early.isEmpty && template.derives.isEmpty && template.self.isEmpty && template.inits.size == 1,
        "NEUTRAL_CURRIED_METHOD_FACTORY_TEMPLATE_UNSUPPORTED",
        "the anonymous template requires one parent and empty early/self/derives topology."
      )
      parent = template.inits.head
      _ <- require(parent.name.value.isEmpty && parent.argClauses.isEmpty, "NEUTRAL_CURRIED_METHOD_FACTORY_PARENT_UNSUPPORTED", "the parent must have no constructor arguments.")
      _ <- parent.tpe match
        case Type.Apply(constructor: Type.Name, List(argument: Type.Name)) =>
          for
            constructorName <- plainTypeName(constructor)
            argumentName <- plainTypeName(argument)
            _ <- require(constructorName == target && argumentName == typeName, "NEUTRAL_CURRIED_METHOD_FACTORY_PARENT_TARGET_MISMATCH", "the anonymous parent must match the result target.")
          yield ()
        case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_PARENT_TARGET_MISMATCH", "the anonymous parent must be the same direct unary target."))
      member <- exactlyOne(template.stats, "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_UNSUPPORTED", "the anonymous body must contain exactly one method member.")
    yield member

  private def projectMemberClauses(groups: List[Member.ParamClauseGroup]): Either[NeutralProjectionError, (Term.Param, Term.Param)] =
    groups match
      case List(group) if group.tparamClause.values.isEmpty =>
        group.paramClauses match
          case List(firstClause, secondClause) if firstClause.mod.isEmpty && secondClause.mod.isEmpty =>
            (firstClause.values, secondClause.values) match
              case (List(first), List(second)) if ordinaryParameter(first) && ordinaryParameter(second) => Right(first -> second)
              case (List(_), List(_)) => Left(memberParameterUnsupported)
              case _ => Left(memberClausesUnsupported)
          case _ => Left(memberClausesUnsupported)
      case _ => Left(memberClausesUnsupported)

  private def projectBody(body: Term): Either[NeutralProjectionError, (String, String, String)] =
    body match
      case Term.Apply(Term.Apply(callee: Term.Name, List(first: Term.Name)), List(second: Term.Name)) =>
        for
          calleeName <- plainTermName(callee)
          firstName <- plainTermName(first)
          secondName <- plainTermName(second)
        yield (calleeName, firstName, secondName)
      case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_BODY_UNSUPPORTED", "the body must be exactly Apply(Apply(carrier, first), second)."))

  private def requireDirectType(value: Option[Type], expected: String): Either[NeutralProjectionError, Unit] =
    value match
      case Some(name: Type.Name) => plainTypeName(name).flatMap(actual => require(actual == expected, "NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH", "each nested parameter and result Type must reference the exact Type parameter."))
      case _ => Left(error("NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_TYPE_MISMATCH", "each nested parameter and result Type must be one direct Type name."))

  private def ordinaryParameter(parameter: Term.Param): Boolean =
    parameter.mods.isEmpty && parameter.default.isEmpty && !parameter.decltpe.exists {
      case _: Type.ByName | _: Type.Repeated => true
      case _ => false
    }

  private def plainParameterName(name: Name): Either[NeutralProjectionError, String] = name match
    case term: Term.Name => plainTermName(term)
    case _ => Left(nameUnsupported)

  private def plainTermName(name: Term.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection.project(name).left.map(_ => nameUnsupported).flatMap(requirePlainName)

  private def plainTypeName(name: Type.Name): Either[NeutralProjectionError, String] =
    ScalametaDefinitionNameProjection.project(name).left.map(_ => nameUnsupported).flatMap(requirePlainName)

  private def requirePlainName(name: DefinitionName): Either[NeutralProjectionError, String] =
    Either.cond(name.source == name.decoded, name.source, nameUnsupported)

  private def mapPlanError(problem: CurriedMethodInstanceFactoryPlan.ModelError): NeutralProjectionError =
    problem.code match
      case "TARGET_TYPE_CONSTRUCTOR_ROLE_COLLAPSE" | "CARRIER_MEMBER_ROLE_COLLAPSE" |
          "CARRIER_FIRST_PARAMETER_ROLE_COLLAPSE" | "CARRIER_SECOND_PARAMETER_ROLE_COLLAPSE" |
          "NESTED_PARAMETER_NAMES_MUST_BE_DISTINCT" =>
        error("NEUTRAL_CURRIED_METHOD_FACTORY_LEXICAL_ROLE_UNSUPPORTED", problem.detail)
      case _ => error("NEUTRAL_CURRIED_METHOD_FACTORY_PLAN_UNSUPPORTED", s"${problem.code}: ${problem.detail}")

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] = tree.pos match
    case Position.None => None
    case position => Some(NeutralSourceSpan(position.start, position.end))

  private def exactlyOne[A](values: List[A], code: String, detail: String): Either[NeutralProjectionError, A] = values match
    case value :: Nil => Right(value)
    case _ => Left(error(code, detail))

  private def require(condition: Boolean, code: String, detail: String): Either[NeutralProjectionError, Unit] = Either.cond(condition, (), error(code, detail))
  private def typeParameterUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_TYPE_PARAMETER_UNSUPPORTED", "the factory requires one invariant, unmodified, unbounded, non-higher-kinded Type parameter.")
  private def carrierUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_CARRIER_UNSUPPORTED", "the factory requires one ordinary strict carrier.")
  private def memberUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_UNSUPPORTED", "the anonymous body must contain exactly one Defn.Def member.")
  private def memberClausesUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_CLAUSES_UNSUPPORTED", "the override requires exactly two ordinary one-parameter clauses and no Type parameters.")
  private def memberParameterUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_MEMBER_PARAMETER_UNSUPPORTED", "each nested parameter must be ordinary, strict, unmodified, typed, and non-defaulted.")
  private def nameUnsupported = error("NEUTRAL_CURRIED_METHOD_FACTORY_NAME_UNSUPPORTED", "every role must use one exact plain fresh-representable source name.")
  private def error(code: String, detail: String) = NeutralProjectionError(code, detail)
