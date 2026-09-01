package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.meta.*

/** Compiler-free projection for the bounded ordinary source-Term family. */
object ScalametaTermProjection:
  private final case class ActiveBinder(name: String, id: BinderId)

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
    projectShape(term, None).map(ProjectedTermShape(_, truthfulSpan(term)))

  private def projectShape(
      term: Term,
      activeBinder: Option[ActiveBinder]
  ): Either[NeutralProjectionError, TermShape] =
    term match
      case name: Term.Name =>
        activeBinder.filter(_.name == name.value) match
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
        projectLambda1(function, activeBinder)
      case block: Term.Block =>
        projectBlock(block, activeBinder)
      case Lit.Int(value) =>
        Right(TermShape.Literal(value.toString))
      case Lit.String(value) =>
        Right(TermShape.Literal("\"" + value + "\""))
      case Lit.Boolean(value) =>
        Right(TermShape.Literal(value.toString))
      case select: Term.Select =>
        for
          qualifier <- projectShape(select.qual, activeBinder)
          selectedName <- validateSourceName(
            select.name.value,
            "NEUTRAL_SELECTION_NAME_UNSUPPORTED",
            "selected names"
          )
        yield TermShape.Select(qualifier, selectedName)
      case application: Term.Apply =>
        projectApply(application, activeBinder)
      case unary: Term.ApplyUnary =>
        for
          _ <- require(
            SupportedUnaryOperators(unary.op.value),
            "NEUTRAL_UNARY_OPERATOR_UNSUPPORTED",
            "unary terms support exactly +, -, !, and ~."
          )
          operand <- projectShape(unary.arg, activeBinder)
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
          leftShape <- projectShape(infix.lhs, activeBinder)
          rightShape <- projectShape(right, activeBinder)
        yield TermShape.Infix(leftShape, infix.op.value, rightShape)
      case tuple: Term.Tuple =>
        for
          _ <- require(
            tuple.args.size >= 2 && tuple.args.size <= 22,
            "NEUTRAL_TUPLE_ARITY_UNSUPPORTED",
            s"tuple terms require arity 2 through 22, found ${tuple.args.size}."
          )
          elements <- traverse(tuple.args)(projectShape(_, activeBinder))
        yield TermShape.Tuple(elements)
      case conditional: Term.If =>
        for
          _ <- require(
            !isSyntheticNoElse(conditional.elsep),
            "NEUTRAL_IF_ELSE_UNSUPPORTED",
            "if terms require an explicit else branch."
          )
          condition <- projectShape(conditional.cond, activeBinder)
          thenBranch <- projectShape(conditional.thenp, activeBinder)
          elseBranch <- projectShape(conditional.elsep, activeBinder)
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
      activeBinder: Option[ActiveBinder]
  ): Either[NeutralProjectionError, TermShape] =
    block.stats match
      case Nil =>
        Left(
          error(
            "NEUTRAL_BLOCK_EMPTY_UNSUPPORTED",
            "P1 block projection requires at least one Term statement."
          )
        )
      case stats =>
        collectTermStatements(stats).flatMap {
          case result :: Nil =>
            projectShape(result, activeBinder)
          case terms =>
            for
              prefix <- traverse(terms.init)(projectShape(_, activeBinder))
              result <- projectShape(terms.last, activeBinder)
            yield TermShape.Block(prefix, result)
        }

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
      activeBinder: Option[ActiveBinder]
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
      function <- projectShape(application.fun, activeBinder)
      arguments <- traverse(application.argClause.values)(projectApplyArgument(_, activeBinder))
    yield TermShape.Apply(function, arguments)

  private def projectApplyArgument(
      argument: Term,
      activeBinder: Option[ActiveBinder]
  ): Either[NeutralProjectionError, TermShape] =
    argument match
      case _: Term.Assign | _: Term.Repeated =>
        Left(
          error(
            "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED",
            s"ordinary Apply arguments must be positional Terms, found ${argument.productPrefix}."
          )
        )
      case other => projectShape(other, activeBinder)

  private def projectLambda1(
      function: Term.Function,
      activeBinder: Option[ActiveBinder]
  ): Either[NeutralProjectionError, TermShape] =
    activeBinder match
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
              binder = ActiveBinder(parameterName, BinderId(0))
              body <- projectShape(function.body, Some(binder))
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
    values.foldRight(Right(Nil): Either[NeutralProjectionError, List[B]]) { (value, rest) =>
      for
        head <- projectValue(value)
        tail <- rest
      yield head :: tail
    }

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

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
