package quasiquotes.neutral

import quasiquotes.publicapi.{
  CompletedTerm,
  CompletedType,
  DefinitionConstruction,
  DefinitionResultView
}

import scala.meta.*

/**
 * Structural projection for exactly one generic method with one `using`
 * parameter clause, applied/named types, and an identifier body.
 */
object ScalametaContextualMethodProjection:
  def project(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedContextualMethod] =
    Option(definition)
      .toRight(error("NEUTRAL_DEFINITION_MISSING", "the Scalameta definition must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      definition: Defn.Def
  ): Either[NeutralProjectionError, ProjectedContextualMethod] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_DEFINITION_MODIFIERS_UNSUPPORTED",
        "the admitted contextual method has no definition modifiers."
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "NEUTRAL_PARAMETER_GROUPS_UNSUPPORTED",
        "expected exactly one parameter-clause group."
      )
      typeParameter <- exactlyOne(
        group.tparamClause.values,
        "NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
        "expected exactly one type parameter."
      )
      _ <- require(
        typeParameter.mods.isEmpty &&
          typeParameter.tparamClause.values.isEmpty &&
          typeParameter.bounds.lo.isEmpty &&
          typeParameter.bounds.hi.isEmpty &&
          typeParameter.bounds.context.isEmpty &&
          typeParameter.bounds.view.isEmpty,
        "NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
        "the admitted type parameter is unmodified, unnested, and unbounded."
      )
      parameterClause <- exactlyOne(
        group.paramClauses,
        "NEUTRAL_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "expected exactly one value-parameter clause."
      )
      _ <- require(
        parameterClause.mod.exists(_.isInstanceOf[Mod.Using]),
        "NEUTRAL_CONTEXTUAL_CLAUSE_UNSUPPORTED",
        "the value-parameter clause must be a `using` clause."
      )
      parameter <- exactlyOne(
        parameterClause.values,
        "NEUTRAL_CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "expected exactly one contextual parameter."
      )
      _ <- require(
        parameter.mods.forall(_.isInstanceOf[Mod.Using]) && parameter.default.isEmpty,
        "NEUTRAL_CONTEXTUAL_PARAMETER_UNSUPPORTED",
        "the admitted contextual parameter has no modifier beyond `using` and no default."
      )
      parameterType <- parameter.decltpe.toRight(
        error(
          "NEUTRAL_CONTEXTUAL_TYPE_MISSING",
          "the contextual parameter must declare a type."
        )
      )
      resultType <- definition.decltpe.toRight(
        error("NEUTRAL_RESULT_TYPE_MISSING", "the method must declare a result type.")
      )
      completedParameterType <- projectType(parameterType, typeParameter.name.value)
      completedResultType <- projectType(resultType, typeParameter.name.value)
      bodyName <- definition.body match
        case value: Term.Name => Right(value.value)
        case _ =>
          Left(
            error(
              "NEUTRAL_BODY_UNSUPPORTED",
              "the admitted method body must be one stable identifier."
            )
          )
      completedBody <- CompletedTerm
        .reference(bodyName)
        .left
        .map(failure =>
          error(
            "NEUTRAL_BODY_VALIDATION_FAILED",
            s"${failure.code}: ${failure.message}"
          )
        )
      result <- DefinitionConstruction
        .contextualMethod(
          definition.name.value,
          typeParameter.name.value,
          parameter.name.value,
          completedParameterType,
          completedResultType,
          completedBody
        )
        .left
        .map(failure =>
          error(
            "NEUTRAL_VALIDATED_IR_REJECTED",
            s"${failure.code}: ${failure.message}"
          )
        )
    yield ProjectedContextualMethod(result, truthfulSpan(definition))

  private def projectType(
      tree: Type,
      declaredTypeParameter: String
  ): Either[NeutralProjectionError, CompletedType] =
    tree match
      case name: Type.Name if name.value == declaredTypeParameter =>
        CompletedType
          .typeParameter(name.value)
          .left
          .map(failure => invalidType(failure.code, failure.message))
      case name: Type.Name =>
        CompletedType
          .named(name.value)
          .left
          .map(failure => invalidType(failure.code, failure.message))
      case applied: Type.Apply =>
        for
          constructor <- projectType(applied.tpe, declaredTypeParameter)
          arguments <- traverse(applied.argClause.values)(
            projectType(_, declaredTypeParameter)
          )
          result <- CompletedType
            .applied(constructor, arguments.toVector)
            .left
            .map(failure => invalidType(failure.code, failure.message))
        yield result
      case _ =>
        Left(
          error(
            "NEUTRAL_TYPE_UNSUPPORTED",
            "only names, the declared type parameter, and nonempty type applications are admitted."
          )
        )

  private def traverse[A, B](
      values: List[A]
  )(
      projectValue: A => Either[NeutralProjectionError, B]
  ): Either[NeutralProjectionError, List[B]] =
    values.foldLeft[Either[NeutralProjectionError, List[B]]](Right(Nil)) {
      (result, value) =>
        for
          completed <- result
          next <- projectValue(value)
        yield completed :+ next
    }

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

  private def invalidType(
      underlyingCode: String,
      detail: String
  ): NeutralProjectionError =
    error("NEUTRAL_TYPE_VALIDATION_FAILED", s"$underlyingCode: $detail")

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
