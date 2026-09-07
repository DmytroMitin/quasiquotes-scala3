package quasiquotes.terms.dotty

import quasiquotes.parser.{BlockStatement, TermShape}

/** Source-role restrictions applied only after the shared checked semantic route. */
private[dotty] object TermGeneratedOriginAdmission:
  import TermGeneratedOriginLowering.Failure

  def validate(term: TermShape): Either[Failure, Unit] =
    def reject(path: String, detail: String): Either[Failure, Unit] =
      Left(Failure("UNSUPPORTED_SEMANTIC_VALUE", s"$path: $detail"))

    def name(value: String, role: String, path: String): Either[Failure, Unit] =
      if value != "_" && StandardSInterpolationEncoding.isPlainIdentifier(value) then Right(())
      else reject(path, s"generated-source $role name `$value` must be a bounded ASCII decoded identifier other than `_`.")

    def children(values: List[TermShape], path: String): Either[Failure, Unit] =
      values.zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) { case (checked, (child, index)) =>
        checked.flatMap(_ => loop(child, s"$path $index"))
      }

    def statement(value: BlockStatement, path: String): Either[Failure, Unit] = value match
      case term: TermShape => loop(term, path)
      case local: BlockStatement.LocalVal =>
        name(local.displayName, "local value", path).flatMap(_ => loop(local.initializer, s"$path initializer"))
      case local: BlockStatement.LocalDef =>
        for
          _ <- name(local.methodDisplayName, "local method", path)
          _ <- name(local.parameterDisplayName, "local method parameter", s"$path parameter")
          _ <- loop(local.body, s"$path body")
        yield ()

    def loop(value: TermShape, path: String): Either[Failure, Unit] = value match
      // References emit the declaration's checked spelling, not this diagnostic display name.
      case _: TermShape.BoundReference | _: TermShape.Literal => Right(())
      case TermShape.Identifier(value, _) => name(value, "identifier", path)
      case TermShape.Select(qualifier, member) =>
        for
          _ <- name(member, "selected member", path)
          _ <- qualifier match
            case TermShape.Literal(value) if value.startsWith("-") =>
              reject(s"$path qualifier", "a directly selected signed decimal literal requires explicit semantic Parenthesized grouping.")
            case _ => Right(())
          _ <- loop(qualifier, s"$path qualifier")
        yield ()
      case TermShape.New(constructor, arguments) =>
        constructor.split("\\.").toList.zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) {
          case (checked, (segment, index)) => checked.flatMap { _ =>
            if segment == "_" || StandardSInterpolationEncoding.isKeyword(segment) then
              reject(s"$path constructor segment $index", s"generated-source constructor segment `$segment` cannot be `_` or a keyword.")
            else Right(())
          }
        }.flatMap(_ => children(arguments, s"$path constructor argument"))
      case TermShape.Apply(function, arguments) =>
        loop(function, s"$path function").flatMap(_ => children(arguments, s"$path argument"))
      case TermShape.Infix(left, _, right) =>
        loop(left, s"$path left operand").flatMap(_ => loop(right, s"$path right operand"))
      case TermShape.Unary(_, operand) => loop(operand, s"$path unary operand")
      case TermShape.InterpolatedString(_, _, arguments) => children(arguments, s"$path interpolation argument")
      case TermShape.Typed(expression, _) => loop(expression, s"$path typed expression")
      case TermShape.Tuple(elements) => children(elements, s"$path tuple element")
      case TermShape.If(condition, thenBranch, elseBranch) =>
        for
          _ <- loop(condition, s"$path condition")
          _ <- loop(thenBranch, s"$path then branch")
          _ <- loop(elseBranch, s"$path else branch")
        yield ()
      case TermShape.Lambda1(_, parameter, _, body) =>
        name(parameter, "lambda parameter", path).flatMap(_ => loop(body, s"$path lambda body"))
      case TermShape.Block(statements, result) =>
        statements.zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) { case (checked, (value, index)) =>
          checked.flatMap(_ => statement(value, s"$path statement $index"))
        }.flatMap(_ => loop(result, s"$path result"))
      case TermShape.Parenthesized(expression) => loop(expression, s"$path parenthesized expression")
      case _: TermShape.Unsupported | null =>
        Left(Failure("INTERNAL_INVARIANT_FAILED", s"$path: the checked semantic graph contained an impossible source-admission node."))

    loop(term, "term")
