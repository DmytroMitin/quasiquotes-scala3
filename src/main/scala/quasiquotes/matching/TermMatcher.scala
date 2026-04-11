package quasiquotes.matching

import scala.quoted.Quotes

object TermMatcher:
  def matchTermRaw(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    matchViews(pattern, target, normalized = false)

  def matchTerm(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    matchViews(pattern, target, normalized = true)

  private def matchViews(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term,
      normalized: Boolean
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    import q.reflect.*
    val preparedPattern =
      if normalized then MatchNormalizer.normalizePattern(pattern)
      else pattern

    def loop(
        pattern: TermPattern,
        target: TargetTermView[Term],
        bindings: Map[String, Term]
    ): Either[MatchFailure, Map[String, Term]] =
      pattern match
        case TermPattern.Hole(name) =>
          bindings.get(name) match
            case None => Right(bindings.updated(name, target.original))
            case Some(previous) =>
              val current = target.original
              normalizedEquality(previous, current).flatMap { equal =>
                if equal then Right(bindings)
                else
                  Left(
                    MatchFailure.RepeatedHoleMismatch(
                      name = name,
                      previous = MatchNormalizer.normalizedTreeStructure(previous),
                      current = MatchNormalizer.normalizedTreeStructure(current)
                    )
                  )
              }
        case TermPattern.Identifier(name) =>
          target match
            case TargetTermView.Identifier(targetName, _) if targetName == name => Right(bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Literal(value) =>
          target match
            case TargetTermView.Literal(targetValue, _) if targetValue == value => Right(bindings)
            case _ => Left(shapeMismatch(pattern, target))
        case TermPattern.Select(qualifier, name) =>
          target match
            case TargetTermView.Select(targetQualifier, targetName, _) if targetName == name =>
              loop(qualifier, targetQualifier, bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Apply(function, arguments) =>
          target match
            case TargetTermView.Apply(targetFunction, targetArguments, _) if targetArguments.length == arguments.length =>
              for
                functionBindings <- loop(function, targetFunction, bindings)
                argumentBindings <- arguments.zip(targetArguments).foldLeft(Right(functionBindings): Either[MatchFailure, Map[String, Term]]) {
                  case (acc, (patternArgument, targetArgument)) =>
                    acc.flatMap(loop(patternArgument, targetArgument, _))
                }
              yield argumentBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Infix(left, operator, right) =>
          target match
            case TargetTermView.Infix(targetLeft, targetOperator, targetRight, _) if targetOperator == operator =>
              for
                leftBindings <- loop(left, targetLeft, bindings)
                rightBindings <- loop(right, targetRight, leftBindings)
              yield rightBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Parenthesized(inner) =>
          if normalized then loop(inner, target, bindings)
          else Left(shapeMismatch(pattern, target))

    for
      rawView <- TargetTermView.fromTerm(target)
      preparedTarget = if normalized then MatchNormalizer.normalizeTarget(rawView) else rawView
      bindings <- loop(preparedPattern, preparedTarget, Map.empty)
    yield MatchResult[q.reflect.Term](bindings)

  private def normalizedEquality(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): Either[MatchFailure, Boolean] =
    for
      normalizedLeft <- MatchNormalizer.normalizedView(left)
      normalizedRight <- MatchNormalizer.normalizedView(right)
    yield normalizedLeft.render == normalizedRight.render

  private def shapeMismatch(pattern: TermPattern, target: TargetTermView[?]): MatchFailure =
    MatchFailure.ShapeMismatch(pattern.render, target.render)
