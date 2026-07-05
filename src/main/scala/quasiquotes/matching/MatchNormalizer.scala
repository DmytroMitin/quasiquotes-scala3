package quasiquotes.matching

import scala.quoted.Quotes

object MatchNormalizer:
  private val SymbolicOperators = Set("+", "-", "*", "/", "%")

  def normalizePattern(pattern: TermPattern): TermPattern =
    pattern match
      case TermPattern.Parenthesized(inner) =>
        normalizePattern(inner)
      case TermPattern.Select(qualifier, name) =>
        TermPattern.Select(normalizePattern(qualifier), name)
      case TermPattern.Apply(function, arguments) =>
        TermPattern.Apply(normalizePattern(function), arguments.map(normalizePattern))
      case TermPattern.Infix(left, operator, right) =>
        TermPattern.Infix(normalizePattern(left), operator, normalizePattern(right))
      case TermPattern.Typed(expression, typeName) =>
        TermPattern.Typed(normalizePattern(expression), typeName)
      case TermPattern.Tuple(elements) =>
        TermPattern.Tuple(elements.map(normalizePattern))
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        TermPattern.If(normalizePattern(condition), normalizePattern(thenBranch), normalizePattern(elseBranch))
      case other =>
        other

  def normalizeTarget[T](view: TargetTermView[T]): TargetTermView[T] =
    view match
      case TargetTermView.Select(qualifier, name, original) =>
        TargetTermView.Select(normalizeTarget(qualifier), name, original)
      case TargetTermView.Apply(TargetTermView.Select(left, operator, _), right :: Nil, original) if SymbolicOperators.contains(operator) =>
        // Task 3.5: normalize the reflect-level method-call encoding of infix syntax.
        TargetTermView.Infix(normalizeTarget(left), operator, normalizeTarget(right), original)
      case TargetTermView.Apply(function, arguments, original) =>
        TargetTermView.Apply(normalizeTarget(function), arguments.map(normalizeTarget), original)
      case TargetTermView.Typed(expression, typeName, original) =>
        TargetTermView.Typed(normalizeTarget(expression), typeName, original)
      case TargetTermView.Tuple(elements, original) =>
        TargetTermView.Tuple(elements.map(normalizeTarget), original)
      case TargetTermView.If(condition, thenBranch, elseBranch, original) =>
        TargetTermView.If(normalizeTarget(condition), normalizeTarget(thenBranch), normalizeTarget(elseBranch), original)
      case other =>
        other

  def normalizedView(using q: Quotes)(
      term: q.reflect.Term
  ): Either[MatchFailure, TargetTermView[q.reflect.Term]] =
    TargetTermView.fromTerm(term).map(normalizeTarget)

  def normalizedTreeStructure(using q: Quotes)(term: q.reflect.Term): String =
    import q.reflect.*
    normalizedView(term) match
      case Right(view) => view.render
      case Left(_) => term.show(using Printer.TreeStructure)
