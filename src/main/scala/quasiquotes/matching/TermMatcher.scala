package quasiquotes.matching

import scala.quoted.Quotes

object TermMatcher:
  def matchTerm(using q: Quotes)(
      pattern: TermPattern,
      target: q.reflect.Term
  ): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    import q.reflect.*

    def loop(
        pattern: TermPattern,
        target: Term,
        bindings: Map[String, Term]
    ): Either[MatchFailure, Map[String, Term]] =
      val normalizedTarget = normalize(target)
      pattern match
        case TermPattern.Hole(name) =>
          bindings.get(name) match
            case None => Right(bindings.updated(name, normalizedTarget))
            case Some(previous) =>
              if structurallyEqual(previous, normalizedTarget) then Right(bindings)
              else
                Left(
                  MatchFailure.RepeatedHoleMismatch(
                    name = name,
                    previous = previous.show(using Printer.TreeStructure),
                    current = normalizedTarget.show(using Printer.TreeStructure)
                  )
                )
        case TermPattern.Identifier(name) =>
          normalizedTarget match
            case Ident(targetName) if targetName == name => Right(bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Literal(value) =>
          literalText(normalizedTarget) match
            case Some(targetValue) if targetValue == value => Right(bindings)
            case _ => Left(shapeMismatch(pattern, normalizedTarget))
        case TermPattern.Select(qualifier, name) =>
          normalizedTarget match
            case Select(targetQualifier, targetName) if targetName == name =>
              loop(qualifier, targetQualifier, bindings)
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Apply(function, arguments) =>
          normalizedTarget match
            case Apply(targetFunction, targetArguments) if targetArguments.length == arguments.length =>
              for
                functionBindings <- loop(function, targetFunction, bindings)
                argumentBindings <- arguments.zip(targetArguments).foldLeft(Right(functionBindings): Either[MatchFailure, Map[String, Term]]) {
                  case (acc, (patternArgument, targetArgument)) =>
                    acc.flatMap(loop(patternArgument, targetArgument, _))
                }
              yield argumentBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Infix(left, operator, right) =>
          normalizedTarget match
            // Reflect trees encode infix syntax as a one-argument Apply(Select(lhs, op), rhs).
            case Apply(Select(targetLeft, targetOperator), targetRight :: Nil) if targetOperator == operator =>
              for
                leftBindings <- loop(left, targetLeft, bindings)
                rightBindings <- loop(right, targetRight, leftBindings)
              yield rightBindings
            case other => Left(shapeMismatch(pattern, other))
        case TermPattern.Parenthesized(inner) =>
          loop(inner, normalizedTarget, bindings)

    loop(pattern, target, Map.empty).map(MatchResult[q.reflect.Term].apply)

  private def normalize(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, _, inner) => normalize(inner)
      case Typed(inner, _) => normalize(inner)
      case Block(Nil, inner: Term) => normalize(inner)
      // Macro arguments often arrive as proxy identifiers whose ValDef rhs is the real tree.
      case ident: Ident if ident.symbol.exists =>
        ident.symbol.tree match
          case ValDef(_, _, Some(rhs)) => normalize(rhs)
          case _ => term
      case _ => term

  private def literalText(using q: Quotes)(term: q.reflect.Term): Option[String] =
    import q.reflect.*
    normalize(term) match
      case Literal(IntConstant(value)) => Some(value.toString)
      case Literal(StringConstant(value)) => Some("\"" + value + "\"")
      case _ => None

  private def structurallyEqual(using q: Quotes)(left: q.reflect.Term, right: q.reflect.Term): Boolean =
    import q.reflect.*
    normalize(left).show(using Printer.TreeStructure) == normalize(right).show(using Printer.TreeStructure)

  private def shapeMismatch(using q: Quotes)(pattern: TermPattern, target: q.reflect.Term): MatchFailure =
    import q.reflect.*
    MatchFailure.ShapeMismatch(pattern.render, normalize(target).show(using Printer.TreeStructure))
