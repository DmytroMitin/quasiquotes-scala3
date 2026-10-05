package quasiquotes.neutral

import quasiquotes.definitions.*

import scala.meta.*

private[quasiquotes] final case class ProjectedSelfAbstractTypeMember(
    plan: SelfAbstractTypeMemberPlan,
    sourceSpan: Option[NeutralSourceSpan]
)

/** Exact Scalameta 4.17.3 projector for the closed AUXify-046/086 family. */
private[quasiquotes] object ScalametaSelfAbstractTypeMemberProjection:
  def project(
      declaration: Decl.Type,
      expectedMemberName: String,
      expectedSelfAliasName: String,
      expectedUpperBaseName: String
  ): Either[NeutralProjectionError, ProjectedSelfAbstractTypeMember] =
    val expectation = SelfAbstractTypeMemberExpectation(
      expectedMemberName,
      expectedSelfAliasName,
      expectedUpperBaseName
    )

    for
      _ <- validateExpectation(expectation)
      present <- validateHeader(declaration, expectedMemberName)
      lower <- parseLower(present.bounds.lo, expectedSelfAliasName)
      _ <- lower.toRight(
        error(
          "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING",
          "the singleton lower bound must be present."
        )
      )
      upper <- present.bounds.hi.toRight(
        error(
          "NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING",
          "the refined upper bound must be present."
        )
      )
      _ <- upper match
        case _: Type.Refine => Right(())
        case _ =>
          Left(
            error(
              "NEUTRAL_SELF_MEMBER_UPPER_REFINEMENT_MISSING",
              "the upper bound must be exactly one named-base refinement."
            )
          )
      projected <- projectValidatedMatrix(present, expectation)
    yield projected

  def projectMatrix(
      declaration: Decl.Type,
      expectedMemberName: String,
      expectedSelfAliasName: String,
      expectedUpperBaseName: String
  ): Either[NeutralProjectionError, ProjectedSelfAbstractTypeMember] =
    val expectation = SelfAbstractTypeMemberExpectation(
      expectedMemberName,
      expectedSelfAliasName,
      expectedUpperBaseName
    )

    for
      _ <- validateExpectation(expectation)
      present <- validateHeader(declaration, expectedMemberName)
      projected <- projectValidatedMatrix(present, expectation)
    yield projected

  private def projectValidatedMatrix(
      declaration: Decl.Type,
      expectation: SelfAbstractTypeMemberExpectation
  ): Either[NeutralProjectionError, ProjectedSelfAbstractTypeMember] =
    for
      lowerAlias <- parseLower(
        declaration.bounds.lo,
        expectation.selfAliasName
      )
      upper <- parseUpper(
        declaration.bounds.hi,
        expectation.memberName,
        expectation.selfAliasName,
        expectation.upperBaseName
      )
      observed = ObservedSelfAbstractTypeMemberMatrix(
        declaration.name.value,
        lowerAlias,
        upper._1,
        upper._2
      )
      plan <- SelfAbstractTypeMemberPlan
        .createMatrix(observed, expectation)
        .left
        .map(problem =>
          error(
            "NEUTRAL_SELF_MEMBER_PLAN_REJECTED",
            problem.message
          )
        )
    yield ProjectedSelfAbstractTypeMember(plan, truthfulSpan(declaration))

  private def validateExpectation(
      expectation: SelfAbstractTypeMemberExpectation
  ): Either[NeutralProjectionError, Unit] =
    SelfAbstractTypeMemberPlan
      .validateExpectation(expectation)
      .left
      .map(classifyExpectationFailure)

  private def validateHeader(
      declaration: Decl.Type,
      expectedMemberName: String
  ): Either[NeutralProjectionError, Decl.Type] =
    for
      present <- Option(declaration).toRight(
        error(
          "NEUTRAL_SELF_MEMBER_DECLARATION_MISSING",
          "the Scalameta Decl.Type must be present."
        )
      )
      _ <- require(
        present.mods.isEmpty,
        "NEUTRAL_SELF_MEMBER_MODIFIERS_UNSUPPORTED",
        "the exact self abstract-Type member has no modifiers."
      )
      _ <- require(
        present.tparamClause.values.isEmpty,
        "NEUTRAL_SELF_MEMBER_TYPE_PARAMETERS_UNSUPPORTED",
        "the exact self abstract-Type member has no type parameters."
      )
      _ <- require(
        present.bounds.context.isEmpty && present.bounds.view.isEmpty,
        "NEUTRAL_SELF_MEMBER_CONTEXT_VIEW_BOUNDS_UNSUPPORTED",
        "context and view bounds are not admitted."
      )
      _ <- require(
        present.name.value == expectedMemberName,
        "NEUTRAL_SELF_MEMBER_OUTER_NAME_MISMATCH",
        "the outer member name must equal the explicit member expectation."
      )
    yield present

  private def parseLower(
      lower: Option[Type],
      expectedSelfAliasName: String
  ): Either[NeutralProjectionError, Option[String]] =
    lower match
      case None => Right(None)
      case Some(Type.Singleton(name: Term.Name)) =>
        require(
          name.value == expectedSelfAliasName,
          "NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH",
          "the singleton lower-bound alias must equal the prepared self-alias expectation."
        ).map(_ => Some(name.value))
      case Some(_) =>
        Left(
          error(
            "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON",
            "the lower bound must be exactly Type.Singleton(Term.Name(alias))."
          )
        )

  private def parseUpper(
      upper: Option[Type],
      expectedMemberName: String,
      expectedSelfAliasName: String,
      expectedUpperBaseName: String
  ): Either[
    NeutralProjectionError,
    (String, Option[ObservedSelfAbstractTypeMemberRefinement])
  ] =
    upper match
      case None =>
        Left(
          error(
            "NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING",
            "the upper bound must be present."
          )
        )
      case Some(base: Type.Name) =>
        require(
          base.value == expectedUpperBaseName,
          "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH",
          "the direct upper base must equal the explicit upper-base expectation."
        ).map(_ => (base.value, None))
      case Some(refined: Type.Refine) =>
        parseRefinement(
          refined,
          expectedMemberName,
          expectedSelfAliasName,
          expectedUpperBaseName
        ).map(observed => (expectedUpperBaseName, Some(observed)))
      case Some(_) =>
        Left(
          error(
            "NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED",
            "the upper bound must be one direct Type.Name or one exact named-base refinement."
          )
        )

  private def parseRefinement(
      refined: Type.Refine,
      expectedMemberName: String,
      expectedSelfAliasName: String,
      expectedUpperBaseName: String
  ): Either[NeutralProjectionError, ObservedSelfAbstractTypeMemberRefinement] =
    for
      baseName <- refined.tpe match
        case Some(name: Type.Name) => Right(name.value)
        case _ =>
          Left(
            error(
              "NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED",
              "the upper refinement base must be one direct Type.Name."
            )
          )
      _ <- require(
        baseName == expectedUpperBaseName,
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH",
        "the upper refinement base must equal the explicit upper-base expectation."
      )
      statistic <- exactlyOne(
        refined.stats,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED",
        "the upper refinement must contain exactly one type alias."
      )
      alias <- statistic match
        case value: Defn.Type => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED",
              "the upper refinement member must be one Defn.Type alias."
            )
          )
      _ <- require(
        alias.mods.isEmpty && alias.tparamClause.values.isEmpty,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED",
        "the refinement alias must be unmodified and non-generic."
      )
      _ <- require(
        alias.bounds.lo.isEmpty &&
          alias.bounds.hi.isEmpty &&
          alias.bounds.context.isEmpty &&
          alias.bounds.view.isEmpty,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_ALIAS_BOUNDS_UNSUPPORTED",
        "the refinement alias has no auxiliary bounds."
      )
      _ <- require(
        alias.name.value == expectedMemberName,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH",
        "the refinement alias name must equal the outer member name."
      )
      selected <- alias.body match
        case value: Type.Select => Right(value)
        case _ =>
          Left(
            error(
              "NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED",
              "the refinement alias RHS must be one direct selected Type."
            )
          )
      selectedPrefix <- selected.qual match
        case name: Term.Name => Right(name.value)
        case _ =>
          Left(
            error(
              "NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED",
              "the selected Type prefix must be one direct Term.Name."
            )
          )
      _ <- require(
        selectedPrefix == expectedSelfAliasName,
        "NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH",
        "the selected Type prefix must equal the prepared self-alias expectation."
      )
      _ <- require(
        selected.name.value == alias.name.value &&
          selected.name.value == expectedMemberName,
        "NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH",
        "the selected Type member must equal the refinement alias and outer expectation."
      )
    yield ObservedSelfAbstractTypeMemberRefinement(
      alias.name.value,
      selectedPrefix,
      selected.name.value
    )

  private def classifyExpectationFailure(
      problem: SelfAbstractTypeMemberPlanError
  ): NeutralProjectionError =
    val code = problem.code match
      case "EXPECTED_MEMBER_NAME_INVALID" =>
        "NEUTRAL_SELF_MEMBER_EXPECTED_MEMBER_INVALID"
      case "EXPECTED_SELF_ALIAS_NAME_INVALID" =>
        "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
      case "EXPECTED_UPPER_BASE_NAME_INVALID" =>
        "NEUTRAL_SELF_MEMBER_EXPECTED_UPPER_BASE_INVALID"
      case _ => "NEUTRAL_SELF_MEMBER_EXPECTATION_INVALID"
    error(code, problem.detail)

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
