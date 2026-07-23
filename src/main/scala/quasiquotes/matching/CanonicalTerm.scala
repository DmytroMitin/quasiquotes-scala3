package quasiquotes.matching

import scala.quoted.Quotes

sealed trait CanonicalTerm derives CanEqual:
  final def render: String = CanonicalTerm.render(this)

object CanonicalTerm:
  final case class Ident(name: String) extends CanonicalTerm
  final case class Literal(value: String) extends CanonicalTerm
  final case class Select(qualifier: CanonicalTerm, name: String) extends CanonicalTerm
  final case class Apply(function: CanonicalTerm, arguments: List[CanonicalTerm]) extends CanonicalTerm
  final case class Infix(left: CanonicalTerm, operator: String, right: CanonicalTerm) extends CanonicalTerm
  final case class Unary(operator: String, operand: CanonicalTerm) extends CanonicalTerm
  final case class Typed(expression: CanonicalTerm, typeName: String) extends CanonicalTerm
  final case class Tuple(elements: List[CanonicalTerm]) extends CanonicalTerm
  final case class If(condition: CanonicalTerm, thenBranch: CanonicalTerm, elseBranch: CanonicalTerm) extends CanonicalTerm

  def render(term: CanonicalTerm): String =
    term match
      case Ident(name) => s"CIdent($name)"
      case Literal(value) => s"CLiteral($value)"
      case Select(qualifier, name) => s"CSelect(${render(qualifier)}, $name)"
      case Apply(function, arguments) =>
        s"CApply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right) =>
        s"CInfix(${render(left)}, $operator, ${render(right)})"
      case Unary(operator, operand) =>
        s"CUnary($operator, ${render(operand)})"
      case Typed(expression, typeName) =>
        s"CTyped(${render(expression)}, Type($typeName))"
      case Tuple(elements) =>
        s"CTuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch) =>
        s"CIf(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"

object TermCanonicalizer:
  def canonicalize(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, CanonicalTerm] =
    MatchNormalizer.normalizedView(term).flatMap(canonicalizeView)

  def canonicalEqual(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): Either[MatchFailure, Boolean] =
    for
      canonicalLeft <- canonicalize(left)
      canonicalRight <- canonicalize(right)
    yield canonicalLeft == canonicalRight

  private def canonicalizeView(view: TargetTermView[?]): Either[MatchFailure, CanonicalTerm] =
    view match
      case TargetTermView.Identifier(name, _) =>
        Right(CanonicalTerm.Ident(name))
      case TargetTermView.Literal(value, _) =>
        Right(CanonicalTerm.Literal(value))
      case TargetTermView.Select(qualifier, name, _) =>
        canonicalizeView(qualifier).map(CanonicalTerm.Select(_, name))
      case TargetTermView.Apply(function, arguments, _) =>
        for
          canonicalFunction <- canonicalizeView(function)
          canonicalArguments <- sequence(arguments.map(canonicalizeView))
        yield CanonicalTerm.Apply(canonicalFunction, canonicalArguments)
      case TargetTermView.Infix(left, operator, right, _) =>
        for
          canonicalLeft <- canonicalizeView(left)
          canonicalRight <- canonicalizeView(right)
        yield CanonicalTerm.Infix(canonicalLeft, operator, canonicalRight)
      case TargetTermView.Unary(operator, operand, _) =>
        canonicalizeView(operand).map(CanonicalTerm.Unary(operator, _))
      case TargetTermView.Typed(expression, typeName, _) =>
        canonicalizeView(expression).map(CanonicalTerm.Typed(_, typeName))
      case TargetTermView.Tuple(elements, _) =>
        sequence(elements.map(canonicalizeView)).map(CanonicalTerm.Tuple.apply)
      case TargetTermView.If(condition, thenBranch, elseBranch, _) =>
        for
          canonicalCondition <- canonicalizeView(condition)
          canonicalThenBranch <- canonicalizeView(thenBranch)
          canonicalElseBranch <- canonicalizeView(elseBranch)
        yield CanonicalTerm.If(canonicalCondition, canonicalThenBranch, canonicalElseBranch)

  private def sequence[A](values: List[Either[MatchFailure, A]]): Either[MatchFailure, List[A]] =
    values.foldRight(Right(Nil): Either[MatchFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
