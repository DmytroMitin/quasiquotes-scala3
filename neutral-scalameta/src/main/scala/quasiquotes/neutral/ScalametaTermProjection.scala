package quasiquotes.neutral

import _root_.quasiquotes.parser.{
  BinderId,
  BlockStatement,
  LocalDefDiagnosticMessages,
  P2LocalValAdmission,
  P2LocalValDiagnosticMessages,
  SourceOwnedLocalDefAdmission,
  TermShape,
  TypeShape
}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

/** Compiler-free projection for the bounded ordinary source-Term family. */
object ScalametaTermProjection:
  private enum BinderKind:
    case Lambda1, P2LocalVal, LocalDefMethod, LocalDefParameter

  private final case class ActiveBinder(
      name: String,
      id: BinderId,
      kind: BinderKind
  )

  private final class ProjectionState:
    val p2Admission = new P2LocalValAdmission.Tracker
    private var nextBinderId = 0

    def allocateBinder(): BinderId =
      val result = BinderId(nextBinderId)
      nextBinderId += 1
      result

  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val PlainSourceName = "[A-Za-z_][A-Za-z0-9_]*".r
  private val Scala3Keywords = Set(
    "abstract",
    "as",
    "case",
    "catch",
    "class",
    "def",
    "derives",
    "do",
    "else",
    "end",
    "enum",
    "export",
    "extends",
    "extension",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "given",
    "if",
    "implicit",
    "import",
    "infix",
    "inline",
    "lazy",
    "macro",
    "match",
    "new",
    "null",
    "object",
    "opaque",
    "open",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "then",
    "this",
    "throw",
    "trait",
    "transparent",
    "true",
    "try",
    "type",
    "using",
    "val",
    "var",
    "while",
    "with",
    "yield"
  )

  def project(
      term: Term
  ): Either[NeutralProjectionError, ProjectedTermShape] =
    Option(term)
      .toRight(error("NEUTRAL_TERM_MISSING", "the Scalameta term must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      term: Term
  ): Either[NeutralProjectionError, ProjectedTermShape] =
    projectShape(term, Nil, new ProjectionState)
      .flatMap(shape =>
        SourceOwnedLocalDefAdmission
          .validate(shape)
          .left
          .map(localDefAdmissionError)
          .map(_ => ProjectedTermShape(shape, truthfulSpan(term)))
      )

  private def projectShape(
      term: Term,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    term match
      case name: Term.Name =>
        scope.find(_.name == name.value) match
          case Some(binder) =>
            Right(TermShape.BoundReference(binder.id, name.value))
          case None =>
            validateSourceName(
              name.value,
              "NEUTRAL_IDENTIFIER_NAME_UNSUPPORTED",
              "direct identifiers"
            ).map(TermShape.Identifier(_, isPlaceholder = false))
      case _: Term.ContextFunction =>
        Left(
          error(
            "NEUTRAL_LAMBDA_CONTEXT_FUNCTION_UNSUPPORTED",
            "Lambda1 projection supports ordinary => functions only."
          )
        )
      case function: Term.Function =>
        projectLambda1(function, scope, state)
      case block: Term.Block =>
        projectBlock(block, scope, state)
      case Lit.Int(value) =>
        Right(TermShape.Literal(value.toString))
      case Lit.String(value) =>
        Right(TermShape.Literal("\"" + value + "\""))
      case Lit.Boolean(value) =>
        Right(TermShape.Literal(value.toString))
      case select: Term.Select =>
        for
          qualifier <- projectShape(select.qual, scope, state)
          selectedName <- validateSourceName(
            select.name.value,
            "NEUTRAL_SELECTION_NAME_UNSUPPORTED",
            "selected names"
          )
        yield TermShape.Select(qualifier, selectedName)
      case application: Term.Apply =>
        projectApply(application, scope, state)
      case unary: Term.ApplyUnary =>
        for
          _ <- require(
            SupportedUnaryOperators(unary.op.value),
            "NEUTRAL_UNARY_OPERATOR_UNSUPPORTED",
            "unary terms support exactly +, -, !, and ~."
          )
          operand <- projectShape(unary.arg, scope, state)
        yield TermShape.Unary(unary.op.value, operand)
      case infix: Term.ApplyInfix =>
        for
          _ <- require(
            infix.targClause.values.isEmpty,
            "NEUTRAL_INFIX_TYPE_ARGUMENTS_UNSUPPORTED",
            "binary infix terms must not have type arguments."
          )
          right <- infix.argClause match
            case Term.ArgClause(value :: Nil, None) => Right(value)
            case _ =>
              Left(
                error(
                  "NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED",
                  "binary infix terms require exactly one ordinary RHS argument."
                )
              )
          leftShape <- projectShape(infix.lhs, scope, state)
          rightShape <- projectShape(right, scope, state)
        yield TermShape.Infix(leftShape, infix.op.value, rightShape)
      case tuple: Term.Tuple =>
        for
          _ <- require(
            tuple.args.size >= 2 && tuple.args.size <= 22,
            "NEUTRAL_TUPLE_ARITY_UNSUPPORTED",
            s"tuple terms require arity 2 through 22, found ${tuple.args.size}."
          )
          elements <- traverse(tuple.args)(projectShape(_, scope, state))
        yield TermShape.Tuple(elements)
      case conditional: Term.If =>
        for
          _ <- require(
            !isSyntheticNoElse(conditional.elsep),
            "NEUTRAL_IF_ELSE_UNSUPPORTED",
            "if terms require an explicit else branch."
          )
          condition <- projectShape(conditional.cond, scope, state)
          thenBranch <- projectShape(conditional.thenp, scope, state)
          elseBranch <- projectShape(conditional.elsep, scope, state)
        yield TermShape.If(condition, thenBranch, elseBranch)
      case other =>
        Left(
          error(
            "NEUTRAL_TERM_UNSUPPORTED",
            s"unsupported Scalameta term node: ${other.productPrefix}."
          )
        )

  private def projectBlock(
      block: Term.Block,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    block.stats match
      case Nil =>
        Left(
          error(
            "NEUTRAL_BLOCK_EMPTY_UNSUPPORTED",
            "P1 block projection requires at least one Term statement."
          )
        )
      case (definition: Defn.Val) :: (result: Term) :: Nil =>
        projectP2LocalVal(definition, result, scope, state)
      case (definition: Defn.Def) :: (result: Term) :: Nil =>
        projectSourceOwnedLocalDef(definition, result, scope, state)
      case (_: Defn.Var) :: (_: Term) :: Nil =>
        Left(
          error(
            "NEUTRAL_P2_MUTABLE_UNSUPPORTED",
            P2LocalValDiagnosticMessages.Mutable
          )
        )
      case stats if stats.exists(_.isInstanceOf[Defn.Val]) =>
        Left(
          error(
            "NEUTRAL_P2_EXACTLY_ONE_LOCAL_VAL_UNSUPPORTED",
            P2LocalValDiagnosticMessages.ExactlyOne
          )
        )
      case stats if stats.exists(_.isInstanceOf[Defn.Var]) =>
        Left(
          error(
            "NEUTRAL_P2_MUTABLE_UNSUPPORTED",
            P2LocalValDiagnosticMessages.Mutable
          )
        )
      case stats if stats.exists(_.isInstanceOf[Defn.Def]) =>
        Left(
          error(
            "NEUTRAL_LOCAL_DEF_EXACTLY_ONE_UNSUPPORTED",
            LocalDefDiagnosticMessages.ExactlyOne
          )
        )
      case stats =>
        collectTermStatements(stats).flatMap {
          case result :: Nil =>
            projectShape(result, scope, state)
          case terms =>
            for
              prefix <- traverse(terms.init)(projectShape(_, scope, state))
              result <- projectShape(terms.last, scope, state)
            yield TermShape.Block(prefix, result)
        }

  private def projectSourceOwnedLocalDef(
      definition: Defn.Def,
      result: Term,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    for
      _ <- require(
        definition.mods.isEmpty,
        "NEUTRAL_LOCAL_DEF_MODIFIERS_UNSUPPORTED",
        LocalDefDiagnosticMessages.Modifiers
      )
      methodName <- validateSourceName(
        definition.name.value,
        "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED",
        "source-owned local-def method names"
      )
      group <- exactlyOne(
        definition.paramClauseGroups,
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
        LocalDefDiagnosticMessages.ParameterClause
      )
      _ <- require(
        group.tparamClause.values.isEmpty,
        "NEUTRAL_LOCAL_DEF_TYPE_PARAMETERS_UNSUPPORTED",
        LocalDefDiagnosticMessages.TypeParameters
      )
      parameterClause <- exactlyOne(
        group.paramClauses,
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
        LocalDefDiagnosticMessages.ParameterClause
      )
      _ <- require(
        parameterClause.mod.isEmpty,
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
        LocalDefDiagnosticMessages.ParameterClause
      )
      parameter <- exactlyOne(
        parameterClause.values,
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
        LocalDefDiagnosticMessages.ParameterClause
      )
      _ <- require(
        parameter.mods.isEmpty && parameter.default.isEmpty,
        "NEUTRAL_LOCAL_DEF_PARAMETER_CLAUSE_UNSUPPORTED",
        LocalDefDiagnosticMessages.ParameterClause
      )
      parameterName <- validateSourceName(
        parameter.name.value,
        "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED",
        "source-owned local-def parameter names"
      )
      parameterType <- parameter.decltpe
        .toRight(
          error(
            "NEUTRAL_LOCAL_DEF_PARAMETER_TYPE_REQUIRED",
            LocalDefDiagnosticMessages.ExplicitTypes
          )
        )
        .flatMap(
          projectLocalDefType(
            _,
            "NEUTRAL_LOCAL_DEF_PARAMETER_TYPE_UNSUPPORTED"
          )
        )
      resultType <- definition.decltpe
        .toRight(
          error(
            "NEUTRAL_LOCAL_DEF_RESULT_TYPE_REQUIRED",
            LocalDefDiagnosticMessages.ExplicitTypes
          )
        )
        .flatMap(
          projectLocalDefType(
            _,
            "NEUTRAL_LOCAL_DEF_RESULT_TYPE_UNSUPPORTED"
          )
        )
      _ <- require(
        parameterType == resultType,
        "NEUTRAL_LOCAL_DEF_INCOMPATIBLE_TYPES_UNSUPPORTED",
        LocalDefDiagnosticMessages.IncompatibleResultType
      )
      methodBinder = ActiveBinder(
        methodName,
        state.allocateBinder(),
        BinderKind.LocalDefMethod
      )
      parameterBinder = ActiveBinder(
        parameterName,
        state.allocateBinder(),
        BinderKind.LocalDefParameter
      )
      body <- projectLocalDefBody(
        definition.body,
        methodBinder,
        parameterBinder,
        scope,
        state
      )
      resultShape <- projectLocalDefResult(
        result,
        methodBinder,
        scope,
        state
      )
    yield TermShape.Block(
      List(
        BlockStatement.LocalDef(
          methodBinder.id,
          methodName,
          parameterBinder.id,
          parameterName,
          parameterType,
          resultType,
          body
        )
      ),
      resultShape
    )

  private def projectLocalDefType(
      sourceType: Type,
      unsupportedCode: String
  ): Either[NeutralProjectionError, TypeShape] =
    val normalized = sourceType match
      case name: Type.Name =>
        name.value match
          case "Int" | "String" | "Boolean" => Some(name.value)
          case _ => None
      case selected: Type.Select =>
        (termPath(selected.qual), selected.name.value) match
          case (Some(List("scala")), "Int") => Some("Int")
          case (Some(List("scala")), "String") => Some("String")
          case (Some(List("scala")), "Boolean") => Some("Boolean")
          case _ => None
      case _ => None

    normalized
      .map(name => TypeShape.Identifier(name))
      .toRight(error(unsupportedCode, LocalDefDiagnosticMessages.UnsupportedTypes))

  private def projectLocalDefBody(
      body: Term,
      methodBinder: ActiveBinder,
      parameterBinder: ActiveBinder,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    body match
      case name: Term.Name if name.value == parameterBinder.name =>
        projectShape(name, parameterBinder :: scope, state).flatMap {
          case reference @ TermShape.BoundReference(id, _)
              if id == parameterBinder.id => Right(reference)
          case _ =>
            Left(
              error(
                "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED",
                LocalDefDiagnosticMessages.Body
              )
            )
        }
      case name: Term.Name if name.value == methodBinder.name =>
        Left(
          error(
            "NEUTRAL_LOCAL_DEF_RECURSION_UNSUPPORTED",
            LocalDefDiagnosticMessages.Body
          )
        )
      case _ =>
        Left(
          error(
            "NEUTRAL_LOCAL_DEF_BODY_UNSUPPORTED",
            LocalDefDiagnosticMessages.Body
          )
        )

  private def projectLocalDefResult(
      result: Term,
      methodBinder: ActiveBinder,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    result match
      case name: Term.Name if name.value == methodBinder.name =>
        projectShape(name, methodBinder :: scope, state).flatMap {
          case reference @ TermShape.BoundReference(id, _)
              if id == methodBinder.id => Right(reference)
          case _ =>
            Left(
              error(
                "NEUTRAL_LOCAL_DEF_RESULT_UNSUPPORTED",
                "Source-owned local def final result must be exactly its method reference."
              )
            )
        }
      case _ =>
        Left(
          error(
            "NEUTRAL_LOCAL_DEF_RESULT_UNSUPPORTED",
            "Source-owned local def final result must be exactly its method reference."
          )
        )

  private def projectP2LocalVal(
      definition: Defn.Val,
      result: Term,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    definition.pats match
      case Pat.Var(name) :: Nil if definition.mods.exists(_.isInstanceOf[Mod.Lazy]) =>
        Left(
          error(
            "NEUTRAL_P2_LAZY_UNSUPPORTED",
            P2LocalValDiagnosticMessages.Lazy
          )
        )
      case Pat.Var(name) :: Nil =>
        for
          _ <- require(
            definition.mods.isEmpty,
            "NEUTRAL_P2_MODIFIERS_UNSUPPORTED",
            "P2 local val requires an eager immutable declaration without modifiers."
          )
          displayName <- validateSourceName(
            name.value,
            "NEUTRAL_P2_BINDER_NAME_UNSUPPORTED",
            "P2 local val binder names"
          )
          declaredType <- definition.decltpe
            .toRight(
              error(
                "NEUTRAL_P2_TYPE_REQUIRED",
                P2LocalValDiagnosticMessages.MissingExplicitType
              )
            )
            .flatMap(projectP2DeclaredType)
          _ <- state.p2Admission
            .introduceLocalVal(displayName)
            .left
            .map(admissionError)
          initializer <- mapP2ChildFailure(
            projectShape(definition.rhs, scope, state),
            "NEUTRAL_P2_INITIALIZER_UNSUPPORTED",
            P2LocalValDiagnosticMessages.UnsupportedInitializer
          )
          binder = ActiveBinder(
            displayName,
            state.allocateBinder(),
            BinderKind.P2LocalVal
          )
          resultShape <- withinLocalValResult(
            state,
            displayName
          )(
            mapP2ChildFailure(
              projectShape(result, binder :: scope, state),
              "NEUTRAL_P2_RESULT_UNSUPPORTED",
              P2LocalValDiagnosticMessages.UnsupportedResult
            )
          )
        yield TermShape.Block(
          List(
            BlockStatement.LocalVal(
              binder.id,
              displayName,
              declaredType,
              initializer
            )
          ),
          resultShape
        )
      case _ =>
        Left(
          error(
            "NEUTRAL_P2_PATTERN_UNSUPPORTED",
            P2LocalValDiagnosticMessages.Pattern
          )
        )

  private def projectP2DeclaredType(
      declaredType: Type
  ): Either[NeutralProjectionError, String] =
    ScalametaTypeNormalFormProjection
      .project(declaredType)
      .map(projected => TermShapeTraversal.renderNormalForm(projected.normalForm))
      .left
      .map(_ =>
        error(
          "NEUTRAL_P2_DECLARED_TYPE_UNSUPPORTED",
          P2LocalValDiagnosticMessages.UnsupportedType
        )
      )

  private def collectTermStatements(
      stats: List[Stat]
  ): Either[NeutralProjectionError, List[Term]] =
    stats match
      case Nil => Right(Nil)
      case (term: Term) :: tail =>
        collectTermStatements(tail).map(term :: _)
      case stat :: _ =>
        Left(
          error(
            "NEUTRAL_BLOCK_STATEMENT_UNSUPPORTED",
            s"P1 blocks admit Term expression statements only, found ${stat.productPrefix}."
          )
        )

  private def projectApply(
      application: Term.Apply,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    for
      _ <- require(
        application.argClause.mod.isEmpty,
        "NEUTRAL_APPLY_ARGUMENT_CLAUSE_UNSUPPORTED",
        "ordinary Apply terms require one non-contextual argument clause."
      )
      _ <- application.fun match
        case _: Term.Apply =>
          Left(
            error(
              "NEUTRAL_APPLY_MULTIPLE_LISTS_UNSUPPORTED",
              "nested Apply function topology would advertise multiple argument lists."
            )
          )
        case _: Term.ApplyType =>
          Left(
            error(
              "NEUTRAL_APPLY_FUNCTION_UNSUPPORTED",
              "ordinary Apply terms must not contain a Type application."
            )
          )
        case _ => Right(())
      function <- projectShape(application.fun, scope, state)
      arguments <- traverse(application.argClause.values)(projectApplyArgument(_, scope, state))
    yield TermShape.Apply(function, arguments)

  private def projectApplyArgument(
      argument: Term,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    argument match
      case _: Term.Assign | _: Term.Repeated =>
        Left(
          error(
            "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED",
            s"ordinary Apply arguments must be positional Terms, found ${argument.productPrefix}."
          )
        )
      case other => projectShape(other, scope, state)

  private def projectLambda1(
      function: Term.Function,
      scope: List[ActiveBinder],
      state: ProjectionState
  ): Either[NeutralProjectionError, TermShape] =
    scope.find(_.kind == BinderKind.Lambda1) match
      case Some(_) =>
        Left(
          error(
            "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED",
            "nested Lambda1 terms are outside the bounded neutral projection."
          )
        )
      case None =>
        function.paramClause.values match
          case parameter :: Nil =>
            for
              _ <- require(
                parameter.mods.isEmpty && function.paramClause.mod.isEmpty,
                "NEUTRAL_LAMBDA_PARAMETER_MODIFIERS_UNSUPPORTED",
                "Lambda1 projection requires one ordinary parameter without modifiers."
              )
              parameterType <- parameter.decltpe
                .toRight(
                  error(
                    "NEUTRAL_LAMBDA_PARAMETER_TYPE_REQUIRED",
                    "Lambda1 projection requires an explicit parameter type."
                  )
                )
                .flatMap(projectLambda1ParameterType)
              parameterName <- validateSourceName(
                parameter.name.value,
                "NEUTRAL_LAMBDA_PARAMETER_NAME_UNSUPPORTED",
                "lambda parameter names"
              )
              binder = ActiveBinder(
                parameterName,
                state.allocateBinder(),
                BinderKind.Lambda1
              )
              body <- withinLambda(
                state,
                parameterName
              )(projectShape(function.body, binder :: scope, state))
            yield TermShape.Lambda1(binder.id, parameterName, parameterType, body)
          case _ =>
            Left(
              error(
                "NEUTRAL_LAMBDA_PARAMETER_CLAUSE_UNSUPPORTED",
                "Lambda1 projection requires exactly one ordinary parameter."
              )
            )

  private def projectLambda1ParameterType(
      parameterType: Type
  ): Either[NeutralProjectionError, String] =
    val normalized = parameterType match
      case name: Type.Name =>
        name.value match
          case "Int" | "String" | "Boolean" => Some(name.value)
          case _ => None
      case select: Type.Select =>
        (termPath(select.qual), select.name.value) match
          case (Some(List("scala")), "Int") => Some("Int")
          case (Some(List("scala")), "String") => Some("String")
          case (Some(List("java", "lang")), "String") => Some("String")
          case (Some(List("scala")), "Boolean") => Some("Boolean")
          case _ => None
      case _ => None

    normalized.toRight(
      error(
        "NEUTRAL_LAMBDA_PARAMETER_TYPE_UNSUPPORTED",
        "Lambda1 parameter types are limited to the established Int, String, and Boolean concrete spellings."
      )
    )

  private def withinLocalValResult[A](
      state: ProjectionState,
      displayName: String
  )(
      body: => Either[NeutralProjectionError, A]
  ): Either[NeutralProjectionError, A] =
    var projected = Option.empty[Either[NeutralProjectionError, A]]
    state.p2Admission
      .withinLocalValResult(displayName) {
        projected = Some(body)
        Right(())
      }
      .left
      .map(admissionError)
      .flatMap(_ =>
        projected match
          case Some(result) => result
          case None =>
            Left(
              error(
                "NEUTRAL_P2_ADMISSION_INTERNAL",
                "P2 result admission did not evaluate the projection body."
              )
            )
      )

  private def withinLambda[A](
      state: ProjectionState,
      displayName: String
  )(
      body: => Either[NeutralProjectionError, A]
  ): Either[NeutralProjectionError, A] =
    var projected = Option.empty[Either[NeutralProjectionError, A]]
    state.p2Admission
      .withinLambda(displayName) {
        projected = Some(body)
        Right(())
      }
      .left
      .map(admissionError)
      .flatMap(_ =>
        projected match
          case Some(result) => result
          case None =>
            Left(
              error(
                "NEUTRAL_P2_ADMISSION_INTERNAL",
                "Lambda admission did not evaluate the projection body."
              )
            )
      )

  private def admissionError(
      violation: P2LocalValAdmission.Violation
  ): NeutralProjectionError =
    violation match
      case P2LocalValAdmission.Violation.SecondOrNestedLocalVal =>
        error(
          "NEUTRAL_P2_SECOND_OR_NESTED_LOCAL_VAL_UNSUPPORTED",
          violation.message
        )
      case P2LocalValAdmission.Violation.SourceBinderShadowing =>
        error(
          "NEUTRAL_P2_SOURCE_BINDER_SHADOWING_UNSUPPORTED",
          violation.message
        )

  private def localDefAdmissionError(
      violation: SourceOwnedLocalDefAdmission.Violation
  ): NeutralProjectionError =
    error(
      "NEUTRAL_LOCAL_DEF_SECOND_OR_NESTED_UNSUPPORTED",
      violation.message
    )

  private def mapP2ChildFailure[A](
      projection: Either[NeutralProjectionError, A],
      code: String,
      detail: String
  ): Either[NeutralProjectionError, A] =
    projection.left.map { problem =>
      if problem.code == "NEUTRAL_P2_SECOND_OR_NESTED_LOCAL_VAL_UNSUPPORTED" ||
          problem.code == "NEUTRAL_P2_SOURCE_BINDER_SHADOWING_UNSUPPORTED" ||
          problem.code == "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED"
      then problem
      else error(code, detail)
    }

  private def termPath(reference: Term): Option[List[String]] =
    reference match
      case name: Term.Name => Some(List(name.value))
      case select: Term.Select =>
        termPath(select.qual).map(_ :+ select.name.value)
      case _ => None

  private def isSyntheticNoElse(term: Term): Boolean =
    term match
      case _: Lit.Unit =>
        term.pos match
          case Position.None => true
          case position => position.start == position.end
      case _ => false

  private def validateSourceName(
      name: String,
      code: String,
      role: String
  ): Either[NeutralProjectionError, String] =
    Either.cond(
      Option(name).exists(value =>
        value != "_" && PlainSourceName.matches(value) && !Scala3Keywords(value)
      ),
      name,
      error(
        code,
        s"$role require a non-keyword ASCII name matching [A-Za-z_][A-Za-z0-9_]*, excluding _."
      )
    )

  private def traverse[A, B](
      values: List[A]
  )(projectValue: A => Either[NeutralProjectionError, B]): Either[NeutralProjectionError, List[B]] =
    values
      .foldLeft(Right(Nil): Either[NeutralProjectionError, List[B]]) { (result, value) =>
        for
          reversed <- result
          projected <- projectValue(value)
        yield projected :: reversed
      }
      .map(_.reverse)

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

  private def exactlyOne[A](
      values: List[A],
      code: String,
      detail: String
  ): Either[NeutralProjectionError, A] =
    values match
      case value :: Nil => Right(value)
      case _ => Left(error(code, detail))

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
