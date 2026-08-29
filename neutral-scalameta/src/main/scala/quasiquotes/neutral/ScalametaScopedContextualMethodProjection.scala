package quasiquotes.neutral

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.meta.*

private[quasiquotes] final case class ProjectedScopedContextualMethod(
    plan: ScopedContextualMethodPlan,
    sourceSpan: Option[NeutralSourceSpan]
)

private[quasiquotes] sealed trait ProjectedContextualMethodRoute

private[quasiquotes] object ProjectedContextualMethodRoute:
  final case class Legacy(value: ProjectedContextualMethod)
      extends ProjectedContextualMethodRoute

  final case class Scoped037(value: ProjectedScopedContextualMethod)
      extends ProjectedContextualMethodRoute

/**
 * Truthful internal dispatch between the public legacy projection result and
 * the identity-bearing exact-037 scoped plan. Once a two-Type-parameter shape
 * selects the scoped route, rejection never falls back to the legacy model.
 */
private[quasiquotes] object ScalametaContextualMethodDispatch:
  import ProjectedContextualMethodRoute.*

  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedContextualMethodRoute] =
    Option(definition) match
      case Some(value)
          if value.paramClauseGroups match
            case group :: Nil => group.tparamClause.values.size == 2
            case _ => false
          =>
        ScalametaScopedContextualMethodProjection
          .project(value)
          .map(Scoped037.apply)
      case _ =>
        ScalametaContextualMethodProjection
          .project(definition)
          .map(Legacy.apply)

/** Exact Scalameta projection for the bounded AUXify-037 method shape. */
private[quasiquotes] object ScalametaScopedContextualMethodProjection:
  private val FirstTypeBinder = BinderId(0)
  private val SecondTypeBinder = BinderId(1)
  private val ContextualTermBinder = BinderId(2)

  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedScopedContextualMethod] =
    Option(definition)
      .toRight(error("NEUTRAL_DEFINITION_MISSING", "the Scalameta definition must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedScopedContextualMethod] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_SCOPED037_DEFINITION_MODIFIERS_UNSUPPORTED",
        "the exact bounded contextual method has no definition modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "NEUTRAL_SCOPED037_PARAMETER_GROUPS_UNSUPPORTED",
        "expected exactly one parameter-clause group."
      )
      typeParameters <- requireTwoTypeParameters(group.tparamClause.values)
      first <- projectTypeParameter(typeParameters.head, FirstTypeBinder, 1)
      second <- projectTypeParameter(typeParameters(1), SecondTypeBinder, 2)
      _ <- require(
        second.upperBound == first.upperBound,
        "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_MISMATCH",
        "the two exact 037 type parameters must share one source-named upper bound."
      )
      parameterClause <- exactlyOne(
        group.paramClauses,
        "NEUTRAL_SCOPED037_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "expected exactly one value-parameter clause."
      )
      _ <- require(
        parameterClause.mod.exists(_.isInstanceOf[Mod.Using]),
        "NEUTRAL_SCOPED037_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "the value-parameter clause must be a `using` clause."
      )
      parameter <- exactlyOne(
        parameterClause.values,
        "NEUTRAL_SCOPED037_CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "expected exactly one contextual parameter."
      )
      _ <- require(
        parameter.mods.forall(_.isInstanceOf[Mod.Using]) && parameter.default.isEmpty,
        "NEUTRAL_SCOPED037_CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "the contextual parameter has no modifier beyond `using` and no default."
      )
      contextualTree <- parameter.decltpe.toRight(
        error(
          "NEUTRAL_SCOPED037_CONTEXTUAL_TYPE_MISSING",
          "the contextual parameter must declare an applied type."
        )
      )
      contextualType <- projectApplied(
        contextualTree,
        Vector(first, second),
        "contextual parameter"
      )
      resultTree <- definition.decltpe.toRight(
        error(
          "NEUTRAL_SCOPED037_RESULT_TYPE_MISSING",
          "the method must declare a refined result type."
        )
      )
      resultType <- projectRefinement(
        resultTree,
        contextualType,
        Vector(first, second),
        parameter.name.value
      )
      bodyBinder <- definition.body match
        case name: Term.Name if name.value == parameter.name.value =>
          Right(ContextualTermBinder)
        case _: Term.Name =>
          Left(
            error(
              "NEUTRAL_SCOPED037_BODY_BINDER_MISMATCH",
              "the method body must reference the exact contextual parameter."
            )
          )
        case _ =>
          Left(
            error(
              "NEUTRAL_SCOPED037_BODY_UNSUPPORTED",
              "the method body must be one stable identifier."
            )
          )
      plan <- ScopedContextualMethodPlan
        .create(
          definition.name.value,
          Vector(first, second),
          ContextualTermBinder,
          parameter.name.value,
          contextualType,
          resultType,
          bodyBinder
        )
        .left
        .map(problem =>
          error("NEUTRAL_SCOPED037_PLAN_REJECTED", problem.message)
        )
    yield ProjectedScopedContextualMethod(plan, truthfulSpan(definition))

  private def requireTwoTypeParameters(
      values: List[Type.Param]
  ): Either[NeutralProjectionError, List[Type.Param]] =
    require(
      values.size == 2,
      "NEUTRAL_SCOPED037_TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
      "expected exactly two bounded type parameters."
    ).map(_ => values)

  private def projectTypeParameter(
      parameter: Type.Param,
      binderId: BinderId,
      position: Int
  ): Either[NeutralProjectionError, ScopedTypeParameter] =
    for
      _ <- require(
        parameter.mods.isEmpty &&
          parameter.tparamClause.values.isEmpty &&
          parameter.bounds.lo.isEmpty &&
          parameter.bounds.context.isEmpty &&
          parameter.bounds.view.isEmpty,
        "NEUTRAL_SCOPED037_TYPE_PARAMETER_UNSUPPORTED",
        s"type parameter $position must be unmodified, unnested, and have only one upper bound."
      )
      upper <- parameter.bounds.hi match
        case Some(name: Type.Name) => Right(SourceName(name.value))
        case _ =>
          Left(
            error(
              "NEUTRAL_SCOPED037_TYPE_PARAMETER_UPPER_BOUND_UNSUPPORTED",
              s"type parameter $position must have one source-named upper bound."
            )
          )
    yield ScopedTypeParameter(binderId, parameter.name.value, upper)

  private def projectApplied(
      tree: Type,
      declarations: Vector[ScopedTypeParameter],
      role: String
  ): Either[NeutralProjectionError, Applied] =
    tree match
      case applied: Type.Apply =>
        for
          constructor <- applied.tpe match
            case name: Type.Name => Right(SourceName(name.value))
            case _ =>
              Left(
                error(
                  "NEUTRAL_SCOPED037_TYPE_CONSTRUCTOR_UNSUPPORTED",
                  s"the $role type constructor must be one source name."
                )
              )
          arguments <- applied.argClause.values.zipWithIndex.foldLeft(
            Right(Vector.empty): Either[NeutralProjectionError, Vector[ScopedType]]
          ) { case (accumulated, (argument, index)) =>
            for
              values <- accumulated
              declaration <- declarations.lift(index).toRight(
                error(
                  "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH",
                  s"the $role must apply the two declared type parameters once, in order."
                )
              )
              reference <- argument match
                case name: Type.Name if name.value == declaration.displayName =>
                  Right(TypeParameterReference(declaration.binderId, name.value))
                case _: Type.Name =>
                  Left(
                    error(
                      "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH",
                      s"the $role must apply the two declared type parameters once, in order."
                    )
                  )
                case _ =>
                  Left(
                    error(
                      "NEUTRAL_SCOPED037_TYPE_ARGUMENT_UNSUPPORTED",
                      s"the $role admits only direct type-parameter references."
                    )
                  )
            yield values :+ reference
          }
          _ <- require(
            arguments.size == declarations.size,
            "NEUTRAL_SCOPED037_TYPE_ARGUMENT_ORDER_MISMATCH",
            s"the $role must apply the two declared type parameters once, in order."
          )
        yield Applied(constructor, arguments)
      case _ =>
        Left(
          error(
            "NEUTRAL_SCOPED037_APPLIED_TYPE_UNSUPPORTED",
            s"the $role must be one applied type."
          )
        )

  private def projectRefinement(
      tree: Type,
      contextualType: Applied,
      declarations: Vector[ScopedTypeParameter],
      contextualName: String
  ): Either[NeutralProjectionError, Refinement] =
    tree match
      case refinement: Type.Refine =>
        for
          baseTree <- refinement.tpe.toRight(
            error(
              "NEUTRAL_SCOPED037_REFINEMENT_BASE_MISSING",
              "the result refinement must have an applied base type."
            )
          )
          base <- projectApplied(baseTree, declarations, "result refinement base")
          _ <- require(
            base == contextualType,
            "NEUTRAL_SCOPED037_REFINEMENT_BASE_MISMATCH",
            "the result refinement base must equal the contextual parameter type."
          )
          member <- exactlyOne(
            refinement.stats,
            "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
            "the result refinement must contain exactly one type alias."
          )
          alias <- member match
            case value: Defn.Type => Right(value)
            case _ =>
              Left(
                error(
                  "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_UNSUPPORTED",
                  "the result refinement member must be one type alias."
                )
              )
          _ <- require(
            alias.mods.isEmpty && alias.tparamClause.values.isEmpty,
            "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_UNSUPPORTED",
            "the result refinement type alias must be unmodified and non-generic."
          )
          selected <- alias.body match
            case value: Type.Select => Right(value)
            case _ =>
              Left(
                error(
                  "NEUTRAL_SCOPED037_REFINEMENT_RHS_UNSUPPORTED",
                  "the refinement alias RHS must be one direct stable selected type."
                )
              )
          _ <- selected.qual match
            case name: Term.Name if name.value == contextualName => Right(())
            case _: Term.Name =>
              Left(
                error(
                  "NEUTRAL_SCOPED037_SELECTED_PREFIX_UNBOUND",
                  "the selected type prefix must be the exact contextual parameter."
                )
              )
            case _ =>
              Left(
                error(
                  "NEUTRAL_SCOPED037_REFINEMENT_RHS_UNSUPPORTED",
                  "the refinement alias RHS must use one direct stable term prefix."
                )
              )
          _ <- require(
            alias.name.value == selected.name.value,
            "NEUTRAL_SCOPED037_REFINEMENT_MEMBER_NAME_MISMATCH",
            "the refinement alias name must equal the selected member name."
          )
          selectedType = DirectStableSelected(
            ContextualTermBinder,
            selected.name.value
          )
        yield Refinement(base, Vector(ScopedTypeAlias(alias.name.value, selectedType)))
      case _ =>
        Left(
          error(
            "NEUTRAL_SCOPED037_RESULT_TYPE_UNSUPPORTED",
            "the result type must be one applied type with one refinement alias."
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
