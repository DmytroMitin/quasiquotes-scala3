package quasiquotes.matching

import scala.quoted.Quotes

sealed trait TargetTermView[+T] derives CanEqual:
  def original: T
  final def render: String = TargetTermView.render(this)

object TargetTermView:
  final case class Identifier[T](name: String, original: T) extends TargetTermView[T]
  final case class Literal[T](value: String, original: T) extends TargetTermView[T]
  final case class Select[T](qualifier: TargetTermView[T], name: String, original: T) extends TargetTermView[T]
  final case class Apply[T](function: TargetTermView[T], arguments: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class Infix[T](left: TargetTermView[T], operator: String, right: TargetTermView[T], original: T) extends TargetTermView[T]

  def fromTerm(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, TargetTermView[q.reflect.Term]] =
    import q.reflect.*

    // This extraction step removes compiler-introduced wrappers but does not perform
    // the higher-level normalization experiment introduced in Task 3.5.
    def extract(term: Term): Either[MatchFailure, TargetTermView[Term]] =
      unwrapWrappers(term) match
        case Ident(name) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Identifier(name, current))
        case q.reflect.Literal(IntConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal(value.toString, current))
        case q.reflect.Literal(StringConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal("\"" + value + "\"", current))
        case q.reflect.Select(qualifier, name) =>
          val current = unwrapWrappers(term)
          extract(qualifier).map(TargetTermView.Select(_, name, current))
        case q.reflect.Apply(function, arguments) =>
          val current = unwrapWrappers(term)
          for
            extractedFunction <- extract(function)
            extractedArguments <- sequence(arguments.map(extract))
          yield TargetTermView.Apply(extractedFunction, extractedArguments, current)
        case other =>
          Left(MatchFailure.UnsupportedTargetShape(other.show(using Printer.TreeStructure)))

    extract(term)

  def render(view: TargetTermView[?]): String =
    view match
      case Identifier(name, _) => s"Ident($name)"
      case Literal(value, _) => s"Literal($value)"
      case Select(qualifier, name, _) => s"Select(${render(qualifier)}, $name)"
      case Apply(function, arguments, _) =>
        s"Apply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right, _) =>
        s"Infix(${render(left)}, $operator, ${render(right)})"

  private def unwrapWrappers(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, _, inner) => unwrapWrappers(inner)
      case Typed(inner, _) => unwrapWrappers(inner)
      case Block(Nil, inner: Term) => unwrapWrappers(inner)
      case ident: Ident if ident.symbol.exists =>
        ident.symbol.tree match
          case ValDef(_, _, Some(rhs)) => unwrapWrappers(rhs)
          case _ => term
      case _ => term

  private def sequence[A](values: List[Either[MatchFailure, A]]): Either[MatchFailure, List[A]] =
    values.foldRight(Right(Nil): Either[MatchFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
