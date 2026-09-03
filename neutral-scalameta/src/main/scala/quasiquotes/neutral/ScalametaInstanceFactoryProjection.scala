package quasiquotes.neutral

import quasiquotes.definitions.InstanceFactoryPlan
import quasiquotes.definitions.InstanceFactoryPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedInstanceFactory(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the bounded AUXify-041 factory. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaInstanceFactoryProjection:
  private val TypeBinder = BinderId(0)
  private val EmptyValueBinder = BinderId(1)
  private val CombineFunctionBinder = BinderId(2)
  private val FirstNestedBinder = BinderId(3)
  private val SecondNestedBinder = BinderId(4)

  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedInstanceFactory] =
    Option(definition)
      .toRight(error("DEFINITION_MISSING", "the Scalameta Defn.Def must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedInstanceFactory] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "OUTER_DEFINITION_TOPOLOGY_UNSUPPORTED",
        "the bounded factory has no outer modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
        "the bounded factory requires exactly one parameter-clause group."
      )
      typeParameter <- projectTypeParameter(group.tparamClause.values)
      outerParameters <- projectOuterParameters(group.paramClauses)
      (emptyValueParameter, combineFunctionParameter) = outerParameters
      _ <- require(
        emptyValueParameter.name.value != combineFunctionParameter.name.value,
        "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
        "the two declarations in one outer lexical scope require distinct source names."
      )
      typeReference = TypeParameterReference(TypeBinder, typeParameter.name.value)
      emptyValue <- projectEmptyValue(emptyValueParameter, typeReference)
      combineFunction <- projectCombineFunction(
        combineFunctionParameter,
        typeReference
      )
      target <- projectTarget(definition.decltpe, typeReference)
      anonymous <- definition.body match
        case value: Term.NewAnonymous => Right(value)
        case _ =>
          Left(
            error(
              "ANONYMOUS_IMPLEMENTATION_REQUIRED",
              "the factory body must be one Term.NewAnonymous implementation."
            )
          )
      members <- projectTemplate(anonymous.templ, target, typeReference)
      (emptyMember, combineMember) = members
      outerScope = Map(
        emptyValueParameter.name.value -> EmptyValueBinder,
        combineFunctionParameter.name.value -> CombineFunctionBinder
      )
      emptyOverride <- projectEmptyOverride(emptyMember, typeReference, outerScope)
      combineOverride <- projectCombineOverride(
        combineMember,
        typeReference,
        outerScope
      )
      plan <- InstanceFactoryPlan
        .create(
          definition.name.value,
          TypeParameter(TypeBinder, typeParameter.name.value),
          emptyValue,
          combineFunction,
          target,
          emptyOverride,
          combineOverride
        )
        .left
        .map(problem => error(problem.code, problem.detail))
    yield ProjectedInstanceFactory(plan, truthfulSpan(definition))

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
      case _ =>
        Left(
          error(
            "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the bounded factory requires one unmodified, unbounded, non-higher-kinded Type parameter."
          )
        )

  private def projectOuterParameters(
      clauses: List[Term.ParamClause]
  ): Either[NeutralProjectionError, (Term.Param, Term.Param)] =
    clauses match
      case List(clause) if clause.mod.isEmpty =>
        clause.values match
          case List(first, second) => Right(first -> second)
          case _ =>
            Left(
              error(
                "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
                "the ordinary outer clause must contain exactly two parameters in role order."
              )
            )
      case _ =>
        Left(
          error(
            "OUTER_PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "the bounded factory requires exactly one ordinary value-parameter clause."
          )
        )

  private def projectEmptyValue(
      parameter: Term.Param,
      typeReference: TypeParameterReference
  ): Either[NeutralProjectionError, ByNameCarrier] =
    for
      _ <- require(
        parameter.mods.isEmpty &&
          parameter.default.isEmpty &&
          !isRepeatedParameter(parameter),
        "EMPTY_VALUE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "the first outer parameter must be unmodified and non-defaulted."
      )
      _ <- parameter.decltpe match
        case Some(Type.ByName(name: Type.Name)) =>
          require(
            name.value == typeReference.displayName,
            "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
            "the by-name value Type must reference the exact factory Type binder."
          )
        case _ =>
          Left(
            error(
              "EMPTY_VALUE_TYPE_ROLE_MISMATCH",
              "the first outer parameter Type must be one Type.ByName over the factory Type binder."
            )
          )
    yield ByNameCarrier(
      EmptyValueBinder,
      parameter.name.value,
      ParameterMode.ByName,
      ValueType(typeReference)
    )

  private def projectCombineFunction(
      parameter: Term.Param,
      typeReference: TypeParameterReference
  ): Either[NeutralProjectionError, BinaryFunctionCarrier] =
    for
      _ <- require(
        parameter.mods.isEmpty &&
          parameter.default.isEmpty &&
          !hasUnsupportedParameterMode(parameter),
        "COMBINE_FUNCTION_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "the second outer parameter must be unmodified and non-defaulted."
      )
      _ <- parameter.decltpe match
        case Some(function: Type.Function) =>
          require(
            function.params match
              case List(first: Type.Name, second: Type.Name) =>
                first.value == typeReference.displayName &&
                  second.value == typeReference.displayName &&
                  (function.res match
                    case result: Type.Name => result.value == typeReference.displayName
                    case _ => false)
              case _ => false,
            "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
            "the binary function Type must contain two argument and one result references to the factory Type binder."
          )
        case _ =>
          Left(
            error(
              "COMBINE_FUNCTION_TYPE_ROLE_MISMATCH",
              "the second outer parameter must declare one structural Type.Function."
            )
          )
    yield BinaryFunctionCarrier(
      CombineFunctionBinder,
      parameter.name.value,
      ParameterMode.ByValue,
      BinaryFunctionType(typeReference, typeReference, typeReference)
    )

  private def projectTarget(
      declared: Option[Type],
      typeReference: TypeParameterReference
  ): Either[NeutralProjectionError, Applied] =
    declared match
      case Some(Type.Apply(constructor: Type.Name, List(argument: Type.Name))) =>
        require(
          argument.value == typeReference.displayName,
          "TARGET_TYPE_ROLE_MISMATCH",
          "the unary target argument must reference the exact factory Type binder."
        ).map(_ => Applied(SourceName(constructor.value), Vector(typeReference)))
      case _ =>
        Left(
          error(
            "TARGET_TYPE_TOPOLOGY_UNSUPPORTED",
            "the result must be one direct source-named unary applied Type."
          )
        )

  private def projectTemplate(
      template: Template,
      target: Applied,
      typeReference: TypeParameterReference
  ): Either[NeutralProjectionError, (Defn.Def, Defn.Def)] =
    for
      _ <- require(
        template.early.isEmpty &&
          template.derives.isEmpty &&
          template.self.isEmpty &&
          template.inits.size == 1,
        "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED",
        "the anonymous implementation requires one parent and empty early/self/derives topology."
      )
      parent = template.inits.head
      _ <- require(
        parent.name.value.isEmpty && parent.argClauses.isEmpty,
        "ANONYMOUS_TEMPLATE_TOPOLOGY_UNSUPPORTED",
        "the anonymous parent must use the source form's empty constructor-argument topology."
      )
      _ <- parent.tpe match
        case Type.Apply(constructor: Type.Name, List(argument: Type.Name)) =>
          val expectedConstructor = target.constructor match
            case SourceName(value) => value
            case _ => ""
          require(
            constructor.value == expectedConstructor &&
              argument.value == typeReference.displayName,
            "PARENT_TARGET_ROLE_MISMATCH",
            "the anonymous parent must match the outer result constructor and Type binder."
          )
        case _ =>
          Left(
            error(
              "PARENT_TARGET_ROLE_MISMATCH",
              "the anonymous parent must be the same direct unary applied target as the result Type."
            )
          )
      members <- template.stats match
        case List(first: Defn.Def, second: Defn.Def) => Right(first -> second)
        case _ =>
          Left(
            error(
              "OVERRIDE_MEMBER_TOPOLOGY_UNSUPPORTED",
              "the anonymous body must contain exactly two ordered Defn.Def members."
            )
          )
    yield members

  private def projectEmptyOverride(
      member: Defn.Def,
      typeReference: TypeParameterReference,
      outerScope: Map[String, BinderId]
  ): Either[NeutralProjectionError, EmptyOverride] =
    for
      _ <- require(
        onlyOverride(member.mods) && member.paramClauseGroups.isEmpty,
        "EMPTY_OVERRIDE_TOPOLOGY_UNSUPPORTED",
        "the first member must be an override with no Type or value parameters."
      )
      _ <- requireDirectTypeReference(
        member.decltpe,
        typeReference,
        "EMPTY_OVERRIDE_TYPE_ROLE_MISMATCH",
        "the empty override result Type"
      )
      bodyName <- member.body match
        case name: Term.Name => Right(name.value)
        case _ =>
          Left(
            error(
              "EMPTY_BODY_ROLE_MISMATCH",
              "the empty body must be one direct Term name."
            )
          )
      _ <- require(
        outerScope.get(bodyName).contains(EmptyValueBinder),
        "EMPTY_BODY_ROLE_MISMATCH",
        "the empty body name must resolve to the exact outer by-name carrier binder."
      )
    yield EmptyOverride(member.name.value, TermReference(EmptyValueBinder))

  private def projectCombineOverride(
      member: Defn.Def,
      typeReference: TypeParameterReference,
      outerScope: Map[String, BinderId]
  ): Either[NeutralProjectionError, CombineOverride] =
    for
      _ <- require(
        onlyOverride(member.mods),
        "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
        "the second member must have exactly the override modifier."
      )
      parameters <- member.paramClauseGroups match
        case List(group) if group.tparamClause.values.isEmpty =>
          group.paramClauses match
            case List(clause) if clause.mod.isEmpty =>
              clause.values match
                case List(first, second)
                    if validNestedParameterShell(first) && validNestedParameterShell(second) &&
                      first.name.value != second.name.value =>
                  Right(first -> second)
                case _ =>
                  Left(
                    error(
                      "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
                      "the combine override requires two distinct unmodified, non-defaulted parameters."
                    )
                  )
            case _ =>
              Left(
                error(
                  "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
                  "the combine override requires one ordinary value clause."
                )
              )
        case _ =>
          Left(
            error(
              "COMBINE_OVERRIDE_TOPOLOGY_UNSUPPORTED",
              "the combine override has no Type parameters and one value-clause group."
            )
          )
      (first, second) = parameters
      _ <- requireDirectTypeReference(
        first.decltpe,
        typeReference,
        "COMBINE_PARAMETER_TYPE_ROLE_MISMATCH",
        "the first combine parameter Type"
      )
      _ <- requireDirectTypeReference(
        second.decltpe,
        typeReference,
        "COMBINE_PARAMETER_TYPE_ROLE_MISMATCH",
        "the second combine parameter Type"
      )
      _ <- requireDirectTypeReference(
        member.decltpe,
        typeReference,
        "COMBINE_RESULT_TYPE_ROLE_MISMATCH",
        "the combine result Type"
      )
      application <- member.body match
        case value: Term.Apply => Right(value)
        case _ =>
          Left(
            error(
              "COMBINE_BODY_TOPOLOGY_UNSUPPORTED",
              "the combine body must be one direct ordinary Term.Apply."
            )
          )
      calleeName <- application.fun match
        case name: Term.Name => Right(name.value)
        case _ =>
          Left(
            error(
              "COMBINE_BODY_TOPOLOGY_UNSUPPORTED",
              "the combine callee must be one direct Term name."
            )
          )
      nestedScope = outerScope ++ Map(
        first.name.value -> FirstNestedBinder,
        second.name.value -> SecondNestedBinder
      )
      _ <- require(
        nestedScope.get(calleeName).contains(CombineFunctionBinder),
        "COMBINE_CALLEE_ROLE_MISMATCH",
        "the combine callee must resolve to the exact outer binary-function carrier binder."
      )
      argumentNames <- application.args.foldLeft(
        Right(Vector.empty): Either[NeutralProjectionError, Vector[String]]
      ) { (projected, argument) =>
        for
          values <- projected
          name <- argument match
            case direct: Term.Name => Right(direct.value)
            case _ =>
              Left(
                error(
                  "COMBINE_ARGUMENT_ROLE_MISMATCH",
                  "each combine argument must be one direct Term name."
                )
              )
        yield values :+ name
      }
      _ <- require(
        argumentNames.flatMap(nestedScope.get) == Vector(
          FirstNestedBinder,
          SecondNestedBinder
        ) && argumentNames.size == 2,
        "COMBINE_ARGUMENT_ROLE_MISMATCH",
        "the combine arguments must resolve to the first and second nested binders once, in order."
      )
    yield CombineOverride(
      member.name.value,
      NestedParameter(FirstNestedBinder, first.name.value, typeReference),
      NestedParameter(SecondNestedBinder, second.name.value, typeReference),
      typeReference,
      CombineBody(
        TermReference(CombineFunctionBinder),
        Vector(TermReference(FirstNestedBinder), TermReference(SecondNestedBinder))
      )
    )

  private def validNestedParameterShell(parameter: Term.Param): Boolean =
    parameter.mods.isEmpty &&
      parameter.default.isEmpty &&
      !hasUnsupportedParameterMode(parameter)

  private def hasUnsupportedParameterMode(parameter: Term.Param): Boolean =
    parameter.decltpe.exists {
      case _: Type.ByName | _: Type.Repeated => true
      case _ => false
    }

  private def isRepeatedParameter(parameter: Term.Param): Boolean =
    parameter.decltpe.exists(_.isInstanceOf[Type.Repeated])

  private def requireDirectTypeReference(
      value: Option[Type],
      expected: TypeParameterReference,
      code: String,
      role: String
  ): Either[NeutralProjectionError, Unit] =
    value match
      case Some(name: Type.Name) =>
        require(
          name.value == expected.displayName,
          code,
          s"$role must reference the exact factory Type binder."
        )
      case _ =>
        Left(error(code, s"$role must be one direct Type.Name reference."))

  private def onlyOverride(modifiers: List[Mod]): Boolean =
    modifiers match
      case List(_: Mod.Override) => true
      case _ => false

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
      case List(value) => Right(value)
      case _ => Left(error(code, detail))

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
