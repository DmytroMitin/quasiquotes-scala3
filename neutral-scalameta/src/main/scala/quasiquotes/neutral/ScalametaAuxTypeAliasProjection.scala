package quasiquotes.neutral

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*

private[quasiquotes] final case class ProjectedAuxTypeAlias(
    plan: AuxTypeAliasPlan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact structural Scalameta projector for the bounded AUXify-039 family. */
@nowarn("cat=deprecation")
private[quasiquotes] object ScalametaAuxTypeAliasProjection:
  private val FirstBinder = BinderId(0)
  private val SecondBinder = BinderId(1)
  private val OutputBinder = BinderId(2)

  def project(
      definition: Defn.Type,
      expected: AuxTypeAliasExpectation
  ): Either[NeutralProjectionError, ProjectedAuxTypeAlias] =
    for
      _ <- AuxTypeAliasPlan
        .validateExpectation(expected)
        .left
        .map(problem =>
          error("NEUTRAL_AUX_EXPECTATION_INVALID", problem.message)
        )
      present <- Option(definition).toRight(
        error(
          "NEUTRAL_AUX_ALIAS_MISSING",
          "the Scalameta Defn.Type must be present."
        )
      )
      projected <- projectPresent(present, expected)
    yield projected

  private def projectPresent(
      definition: Defn.Type,
      expected: AuxTypeAliasExpectation
  ): Either[NeutralProjectionError, ProjectedAuxTypeAlias] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_AUX_ALIAS_MODIFIERS_UNSUPPORTED",
        "the bounded alias has no modifiers."
      )
      _ <- require(
        definition.bounds.lo.isEmpty &&
          definition.bounds.hi.isEmpty &&
          definition.bounds.context.isEmpty &&
          definition.bounds.view.isEmpty,
        "NEUTRAL_AUX_ALIAS_BOUNDS_UNSUPPORTED",
        "the outer alias has no auxiliary bounds."
      )
      _ <- require(
        definition.name.value == expected.aliasName,
        "NEUTRAL_AUX_ALIAS_NAME_MISMATCH",
        "the alias name must equal the explicit source expectation."
      )
      sourceParameters <- requireThreeParameters(
        definition.tparamClause.values
      )
      first <- projectParameter(
        sourceParameters(0),
        FirstBinder,
        expected.firstParameter,
        1
      )
      second <- projectParameter(
        sourceParameters(1),
        SecondBinder,
        expected.secondParameter,
        2
      )
      output <- projectParameter(
        sourceParameters(2),
        OutputBinder,
        expected.outputParameter,
        3
      )
      declarations = Vector(first, second, output)
      refinement <- projectRefinement(
        definition.body,
        declarations,
        expected
      )
      plan <- AuxTypeAliasPlan
        .create(
          definition.name.value,
          declarations,
          refinement,
          expected
        )
        .left
        .map(problem =>
          error("NEUTRAL_AUX_PLAN_REJECTED", problem.message)
        )
    yield ProjectedAuxTypeAlias(plan, truthfulSpan(definition))

  private def requireThreeParameters(
      parameters: List[Type.Param]
  ): Either[NeutralProjectionError, List[Type.Param]] =
    require(
      parameters.size == 3,
      "NEUTRAL_AUX_TYPE_PARAMETER_ARITY_UNSUPPORTED",
      "the bounded alias requires exactly three Type parameters."
    ).map(_ => parameters)

  private def projectParameter(
      parameter: Type.Param,
      binderId: BinderId,
      expected: AuxTypeParameterExpectation,
      position: Int
  ): Either[NeutralProjectionError, ScopedTypeParameter] =
    for
      _ <- require(
        parameter.mods.isEmpty,
        "NEUTRAL_AUX_TYPE_PARAMETER_MODIFIERS_UNSUPPORTED",
        s"Type parameter $position has no modifiers or variance."
      )
      _ <- require(
        parameter.tparamClause.values.isEmpty,
        "NEUTRAL_AUX_TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        s"Type parameter $position has no nested Type-parameter clause."
      )
      _ <- require(
        parameter.bounds.lo.isEmpty,
        "NEUTRAL_AUX_TYPE_PARAMETER_LOWER_BOUND_UNSUPPORTED",
        s"Type parameter $position has no lower bound."
      )
      _ <- require(
        parameter.bounds.context.isEmpty && parameter.bounds.view.isEmpty,
        "NEUTRAL_AUX_TYPE_PARAMETER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED",
        s"Type parameter $position has no context or view bounds."
      )
      upperTree <- parameter.bounds.hi.toRight(
        error(
          "NEUTRAL_AUX_TYPE_PARAMETER_UPPER_BOUND_MISSING",
          s"Type parameter $position must have one source-named upper bound."
        )
      )
      upperName <- upperTree match
        case name: Type.Name => Right(name.value)
        case _ =>
          Left(
            error(
              "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED",
              s"Type parameter $position upper bound must be one direct Type.Name."
            )
          )
      _ <- require(
        parameter.name.value == expected.displayName,
        "NEUTRAL_AUX_TYPE_PARAMETER_NAME_MISMATCH",
        s"Type parameter $position name must equal its explicit source expectation."
      )
      _ <- require(
        upperName == expected.upperBoundName,
        "NEUTRAL_AUX_TYPE_PARAMETER_BOUND_MISMATCH",
        s"Type parameter $position upper bound must equal its explicit source expectation."
      )
    yield ScopedTypeParameter(
      binderId,
      parameter.name.value,
      SourceName(upperName)
    )

  private def projectRefinement(
      body: Type,
      declarations: Vector[ScopedTypeParameter],
      expected: AuxTypeAliasExpectation
  ): Either[NeutralProjectionError, Refinement] =
    body match
      case refinement: Type.Refine =>
        for
          baseTree <- refinement.tpe.toRight(
            error(
              "NEUTRAL_AUX_REFINEMENT_BASE_MISSING",
              "the bounded alias refinement must have an applied base."
            )
          )
          base <- projectApplied(baseTree, declarations, expected.targetName)
          statistic <- exactlyOne(
            refinement.stats,
            "NEUTRAL_AUX_REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
            "the bounded alias refinement must contain exactly one member."
          )
          member <- projectMember(
            statistic,
            declarations(2),
            expected.refinementMemberName
          )
        yield Refinement(base, Vector(member))
      case _ =>
        Left(
          error(
            "NEUTRAL_AUX_RHS_REFINEMENT_REQUIRED",
            "the bounded alias RHS must be one refinement."
          )
        )

  private def projectApplied(
      tree: Type,
      declarations: Vector[ScopedTypeParameter],
      expectedTarget: String
  ): Either[NeutralProjectionError, Applied] =
    tree match
      case applied: Type.Apply =>
        for
          constructor <- applied.tpe match
            case name: Type.Name => Right(SourceName(name.value))
            case _ =>
              Left(
                error(
                  "NEUTRAL_AUX_TARGET_CONSTRUCTOR_UNSUPPORTED",
                  "the target constructor must be one direct Type.Name."
                )
              )
          _ <- require(
            constructor.value == expectedTarget,
            "NEUTRAL_AUX_TARGET_NAME_MISMATCH",
            "the target constructor must equal the explicit source expectation."
          )
          arguments <- requireTwoArguments(applied.argClause.values)
          references <- arguments.zip(declarations.take(2)).zipWithIndex.foldLeft(
            Right(Vector.empty): Either[
              NeutralProjectionError,
              Vector[ScopedType]
            ]
          ) { case (projected, ((argument, declaration), index)) =>
            for
              values <- projected
              name <- argument match
                case value: Type.Name => Right(value.value)
                case _ =>
                  Left(
                    error(
                      "NEUTRAL_AUX_TARGET_ARGUMENT_UNSUPPORTED",
                      s"target argument ${index + 1} must be one direct Type.Name."
                    )
                  )
              _ <- require(
                name == declaration.displayName,
                "NEUTRAL_AUX_TARGET_BINDER_REFERENCE_MISMATCH",
                s"target argument ${index + 1} must reference declaration binder ${index + 1}."
              )
            yield values :+ TypeParameterReference(
              declaration.binderId,
              name
            )
          }
        yield Applied(constructor, references)
      case _ =>
        Left(
          error(
            "NEUTRAL_AUX_TARGET_APPLIED_TYPE_REQUIRED",
            "the refinement base must be one applied Type."
          )
        )

  private def requireTwoArguments(
      arguments: List[Type]
  ): Either[NeutralProjectionError, List[Type]] =
    require(
      arguments.size == 2,
      "NEUTRAL_AUX_TARGET_ARITY_UNSUPPORTED",
      "the target constructor must receive exactly two Type arguments."
    ).map(_ => arguments)

  private def projectMember(
      statistic: Stat,
      outputDeclaration: ScopedTypeParameter,
      expectedMemberName: String
  ): Either[NeutralProjectionError, ScopedTypeAlias] =
    for
      member <- statistic match
        case value: Defn.Type => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_AUX_REFINEMENT_MEMBER_UNSUPPORTED",
              "the refinement member must be one Type alias."
            )
          )
      _ <- require(
        member.mods.isEmpty && member.tparamClause.values.isEmpty,
        "NEUTRAL_AUX_REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
        "the refinement Type alias must be unmodified and non-generic."
      )
      _ <- require(
        member.bounds.lo.isEmpty &&
          member.bounds.hi.isEmpty &&
          member.bounds.context.isEmpty &&
          member.bounds.view.isEmpty,
        "NEUTRAL_AUX_REFINEMENT_MEMBER_BOUNDS_UNSUPPORTED",
        "the refinement Type alias has no auxiliary bounds."
      )
      _ <- require(
        member.name.value == expectedMemberName,
        "NEUTRAL_AUX_REFINEMENT_MEMBER_NAME_MISMATCH",
        "the refinement member name must equal the explicit source expectation."
      )
      outputName <- member.body match
        case name: Type.Name => Right(name.value)
        case _ =>
          Left(
            error(
              "NEUTRAL_AUX_REFINEMENT_RHS_UNSUPPORTED",
              "the refinement RHS must be one direct Type.Name."
            )
          )
      _ <- require(
        outputName == outputDeclaration.displayName,
        "NEUTRAL_AUX_OUTPUT_BINDER_REFERENCE_MISMATCH",
        "the refinement RHS must reference the exact third Type binder."
      )
      reference = TypeParameterReference(
        outputDeclaration.binderId,
        outputName
      )
    yield ScopedTypeAlias(member.name.value, reference)

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
