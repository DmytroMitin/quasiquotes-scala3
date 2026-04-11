package quasiquotes.matching

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
      case other =>
        other
