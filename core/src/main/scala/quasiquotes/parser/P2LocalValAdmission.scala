package quasiquotes.parser

private[quasiquotes] object P2LocalValAdmission:
  enum Violation derives CanEqual:
    case SecondOrNestedLocalVal
    case SourceBinderShadowing

    def message: String =
      this match
        case SecondOrNestedLocalVal => P2LocalValDiagnosticMessages.SecondOrNested
        case SourceBinderShadowing => P2LocalValDiagnosticMessages.SourceBinderShadowing

  final class Tracker:
    private enum BinderKind:
      case Lambda1, P2LocalVal

    private var p2Seen = false
    private var activeBinders = List.empty[(String, BinderKind)]

    def introduceLocalVal(displayName: String): Either[Violation, Unit] =
      if p2Seen then Left(Violation.SecondOrNestedLocalVal)
      else if activeBinders.exists(_._1 == displayName) then
        Left(Violation.SourceBinderShadowing)
      else
        p2Seen = true
        Right(())

    def withinLocalValResult[A](
        displayName: String
    )(
        body: => Either[Violation, A]
    ): Either[Violation, A] =
      within(displayName, BinderKind.P2LocalVal)(body)

    def withinLambda[A](
        displayName: String
    )(
        body: => Either[Violation, A]
    ): Either[Violation, A] =
      if activeBinders.exists {
          case (`displayName`, BinderKind.P2LocalVal) => true
          case _ => false
        }
      then Left(Violation.SourceBinderShadowing)
      else within(displayName, BinderKind.Lambda1)(body)

    private def within[A](
        displayName: String,
        kind: BinderKind
    )(
        body: => Either[Violation, A]
    ): Either[Violation, A] =
      val previous = activeBinders
      activeBinders = (displayName -> kind) :: activeBinders
      try body
      finally activeBinders = previous

  def validate(shape: TermShape): Either[Violation, Unit] =
    val tracker = new Tracker

    def sequence(shapes: List[TermShape]): Either[Violation, Unit] =
      shapes.foldLeft[Either[Violation, Unit]](Right(())) { (result, shape) =>
        result.flatMap(_ => loop(shape))
      }

    def loop(shape: TermShape): Either[Violation, Unit] =
      shape match
        case TermShape.Lambda1(_, displayName, _, body) =>
          tracker.withinLambda(displayName)(loop(body))
        case TermShape.Select(qualifier, _) => loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function).flatMap(_ => sequence(arguments))
        case TermShape.New(_, arguments) => sequence(arguments)
        case TermShape.Infix(left, _, right) => loop(left).flatMap(_ => loop(right))
        case TermShape.Unary(_, operand) => loop(operand)
        case TermShape.InterpolatedString(_, _, arguments) => sequence(arguments)
        case TermShape.Typed(expression, _) => loop(expression)
        case TermShape.Tuple(elements) => sequence(elements)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition).flatMap(_ => loop(thenBranch)).flatMap(_ => loop(elseBranch))
        case TermShape.Block((local: BlockStatement.LocalVal) :: Nil, result) =>
          tracker
            .introduceLocalVal(local.displayName)
            .flatMap(_ => loop(local.initializer))
            .flatMap(_ => tracker.withinLocalValResult(local.displayName)(loop(result)))
        case TermShape.Block(statements, result) =>
          val expressions = statements.collect { case term: TermShape => term }
          sequence(expressions).flatMap(_ => loop(result))
        case TermShape.Parenthesized(expression) => loop(expression)
        case TermShape.Identifier(_, _) |
            TermShape.BoundReference(_, _) |
            TermShape.Literal(_) |
            TermShape.Unsupported(_, _) => Right(())

    loop(shape)
