package quasiquotes.neutral

import quasiquotes.definitions.DelegatedForwardingMethodPlan
import quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.meta.*

private[quasiquotes] final case class ProjectedDelegatedForwardingMethod(
    plan: Plan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the AUXify-043 forwarder. */
private[quasiquotes] object ScalametaDelegatedForwardingMethodProjection:
  private val TypeBinder = BinderId(0)
  private val OrdinaryBinder = BinderId(1)
  private val ContextualBinder = BinderId(2)

  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedDelegatedForwardingMethod] =
    Option(definition)
      .toRight(
        error(
          "DEFINITION_TOPOLOGY_UNSUPPORTED",
          "the Scalameta Defn.Def must be present."
        )
      )
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedDelegatedForwardingMethod] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "DEFINITION_TOPOLOGY_UNSUPPORTED",
        "the exact 043 method has no definition modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
        "the exact 043 method has one parameter-clause group."
      )
      typeParameter <- group.tparamClause.values match
        case value :: Nil
            if value.mods.isEmpty &&
              value.tparamClause.values.isEmpty &&
              value.bounds.lo.isEmpty &&
              value.bounds.hi.isEmpty &&
              value.bounds.context.isEmpty &&
              value.bounds.view.isEmpty =>
          Right(value)
        case _ =>
          Left(
            error(
              "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
              "the exact 043 method requires one unmodified, unbounded, unnested Type parameter."
            )
          )
      clauses <- group.paramClauses match
        case ordinary :: contextual :: Nil => Right(ordinary -> contextual)
        case _ =>
          Left(
            error(
              "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
              "the exact 043 method requires one ordinary clause followed by one final using clause."
            )
          )
      ordinary <- projectOrdinary(clauses._1)
      contextual <- projectContextual(clauses._2)
      ordinaryType <- ordinary.decltpe match
        case Some(name: Type.Name) if name.value == typeParameter.name.value =>
          Right(TypeParameterReference(TypeBinder, name.value))
        case _ =>
          Left(
            error(
              "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH",
              "the ordinary parameter Type must reference the declared method Type binder."
            )
          )
      contextualType <- projectContextualType(
        contextual,
        typeParameter.name.value
      )
      resultType <- definition.decltpe match
        case Some(name: Type.Name) => Right(SourceName(name.value))
        case _ =>
          Left(
            error(
              "RESULT_TYPE_UNSUPPORTED",
              "the exact 043 result Type must be one direct source name."
            )
          )
      application <- definition.body match
        case value: Term.Apply => Right(value)
        case _ =>
          Left(
            error(
              "BODY_APPLICATION_UNSUPPORTED",
              "the exact 043 body must be one application."
            )
          )
      selection <- application.fun match
        case value: Term.Select => Right(value)
        case _ =>
          Left(
            error(
              "BODY_SELECTION_UNSUPPORTED",
              "the applied function must be one direct selection."
            )
          )
      _ <- selection.qual match
        case name: Term.Name if name.value == contextual.name.value => Right(())
        case _ =>
          Left(
            error(
              "BODY_RECEIVER_BINDER_MISMATCH",
              "the selected receiver must be the exact contextual parameter."
            )
          )
      _ <- require(
        selection.name.value == definition.name.value,
        "BODY_SELECTED_METHOD_MISMATCH",
        "the selected member must equal the generated method name."
      )
      argumentName <- application.args match
        case List(name: Term.Name) => Right(name.value)
        case _ =>
          Left(
            error(
              "BODY_ARGUMENT_TOPOLOGY_UNSUPPORTED",
              "the selected method must receive exactly one direct Term-name argument."
            )
          )
      _ <- require(
        argumentName == ordinary.name.value,
        "BODY_ARGUMENT_BINDER_MISMATCH",
        "the application argument must be the exact ordinary parameter."
      )
      plan <- DelegatedForwardingMethodPlan
        .create(
          definition.name.value,
          TypeParameter(TypeBinder, typeParameter.name.value),
          OrdinaryParameter(OrdinaryBinder, ordinary.name.value, ordinaryType),
          ContextualParameter(
            ContextualBinder,
            contextual.name.value,
            contextualType
          ),
          resultType,
          ForwardingBody(
            ContextualReference(ContextualBinder),
            selection.name.value,
            OrdinaryReference(OrdinaryBinder)
          )
        )
        .left
        .map(problem => error(problem.code, problem.detail))
    yield ProjectedDelegatedForwardingMethod(plan, truthfulSpan(definition))

  private def projectOrdinary(
      clause: Term.ParamClause
  ): Either[NeutralProjectionError, Term.Param] =
    if clause.mod.nonEmpty then
      Left(
        error(
          "ORDINARY_CLAUSE_UNSUPPORTED",
          "the first value clause must be ordinary."
        )
      )
    else
      clause.values match
        case parameter :: Nil
            if parameter.mods.isEmpty &&
              parameter.default.isEmpty &&
              !hasUnsupportedParameterType(parameter) =>
          Right(parameter)
        case _ =>
          Left(
            error(
              "ORDINARY_PARAMETER_UNSUPPORTED",
              "the first clause must contain one unmodified, non-defaulted parameter."
            )
          )

  private def hasUnsupportedParameterType(parameter: Term.Param): Boolean =
    parameter.decltpe.exists {
      case _: Type.ByName | _: Type.Repeated => true
      case _ => false
    }

  private def projectContextual(
      clause: Term.ParamClause
  ): Either[NeutralProjectionError, Term.Param] =
    if !clause.mod.exists(_.isInstanceOf[Mod.Using]) then
      Left(
        error(
          "CONTEXTUAL_CLAUSE_UNSUPPORTED",
          "the second and final value clause must be a using clause."
        )
      )
    else
      clause.values match
        case parameter :: Nil
            if parameter.mods.forall(_.isInstanceOf[Mod.Using]) &&
              parameter.default.isEmpty =>
          Right(parameter)
        case _ =>
          Left(
            error(
              "CONTEXTUAL_PARAMETER_UNSUPPORTED",
              "the final using clause must contain one non-defaulted contextual parameter."
            )
          )

  private def projectContextualType(
      parameter: Term.Param,
      typeParameterName: String
  ): Either[NeutralProjectionError, Applied] =
    parameter.decltpe match
      case Some(applied: Type.Apply) =>
        applied.tpe match
          case constructor: Type.Name =>
            applied.argClause.values match
              case List(reference: Type.Name)
                  if reference.value == typeParameterName =>
                Right(
                  Applied(
                    SourceName(constructor.value),
                    Vector(TypeParameterReference(TypeBinder, reference.value))
                  )
                )
              case List(_: Type.Name) =>
                Left(
                  error(
                    "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
                    "the contextual Type argument must reference the declared method Type binder."
                  )
                )
              case _ =>
                Left(
                  error(
                    "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
                    "the contextual Type must apply one source-named constructor to one direct Type reference."
                  )
                )
          case _ =>
            Left(
              error(
                "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
                "the contextual Type constructor must be one direct source name."
              )
            )
      case _ =>
        Left(
          error(
            "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
            "the contextual parameter must declare one unary applied Type."
          )
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

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
