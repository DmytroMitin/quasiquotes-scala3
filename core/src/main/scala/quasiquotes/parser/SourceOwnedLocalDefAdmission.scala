package quasiquotes.parser

private[quasiquotes] object SourceOwnedLocalDefAdmission:
  final case class Violation(message: String)

  def validate(shape: TermShape): Either[Violation, Unit] =
    shape match
      case TermShape.Unsupported("Block", detail)
          if detail == LocalDefDiagnosticMessages.ExactlyOne =>
        Left(Violation(detail))
      case TermShape.Block((definition: BlockStatement.LocalDef) :: Nil, result) =>
        if containsDefinition(definition.body) || containsDefinition(result) then
          Left(Violation(LocalDefDiagnosticMessages.ExactlyOne))
        else Right(())
      case other if containsLocalDef(other) =>
        Left(Violation(LocalDefDiagnosticMessages.ExactlyOne))
      case _ => Right(())

  private def containsLocalDef(shape: TermShape): Boolean =
    shape match
      case TermShape.Block(statements, result) =>
        statements.exists {
          case _: BlockStatement.LocalDef => true
          case BlockStatement.LocalVal(_, _, _, initializer) => containsLocalDef(initializer)
          case term: TermShape => containsLocalDef(term)
        } || containsLocalDef(result)
      case TermShape.Select(qualifier, _) => containsLocalDef(qualifier)
      case TermShape.Apply(function, arguments) =>
        containsLocalDef(function) || arguments.exists(containsLocalDef)
      case TermShape.New(_, arguments) => arguments.exists(containsLocalDef)
      case TermShape.Infix(left, _, right) => containsLocalDef(left) || containsLocalDef(right)
      case TermShape.Unary(_, operand) => containsLocalDef(operand)
      case TermShape.InterpolatedString(_, _, arguments) => arguments.exists(containsLocalDef)
      case TermShape.Typed(expression, _) => containsLocalDef(expression)
      case TermShape.Tuple(elements) => elements.exists(containsLocalDef)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        containsLocalDef(condition) || containsLocalDef(thenBranch) || containsLocalDef(elseBranch)
      case TermShape.Parenthesized(expression) => containsLocalDef(expression)
      case TermShape.Lambda1(_, _, _, body) => containsLocalDef(body)
      case TermShape.Identifier(_, _) | TermShape.BoundReference(_, _) |
          TermShape.Literal(_) | TermShape.Unsupported(_, _) => false

  private def containsDefinition(shape: TermShape): Boolean =
    shape match
      case TermShape.Block(statements, result) =>
        statements.exists {
          case _: BlockStatement.LocalDef | _: BlockStatement.LocalVal => true
          case term: TermShape => containsDefinition(term)
        } || containsDefinition(result)
      case TermShape.Select(qualifier, _) => containsDefinition(qualifier)
      case TermShape.Apply(function, arguments) =>
        containsDefinition(function) || arguments.exists(containsDefinition)
      case TermShape.New(_, arguments) => arguments.exists(containsDefinition)
      case TermShape.Infix(left, _, right) => containsDefinition(left) || containsDefinition(right)
      case TermShape.Unary(_, operand) => containsDefinition(operand)
      case TermShape.InterpolatedString(_, _, arguments) => arguments.exists(containsDefinition)
      case TermShape.Typed(expression, _) => containsDefinition(expression)
      case TermShape.Tuple(elements) => elements.exists(containsDefinition)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        containsDefinition(condition) || containsDefinition(thenBranch) || containsDefinition(elseBranch)
      case TermShape.Parenthesized(expression) => containsDefinition(expression)
      case TermShape.Lambda1(_, _, _, body) => containsDefinition(body)
      case TermShape.Identifier(_, _) | TermShape.BoundReference(_, _) |
          TermShape.Literal(_) | TermShape.Unsupported(_, _) => false
