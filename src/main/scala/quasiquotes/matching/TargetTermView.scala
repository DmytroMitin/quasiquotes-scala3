package quasiquotes.matching

import scala.quoted.Quotes
import scala.util.matching.Regex

sealed trait TargetTermView[+T] derives CanEqual:
  def original: T
  final def render: String = TargetTermView.render(this)

object TargetTermView:
  private val UnaryOperatorByMethod = Map(
    "unary_+" -> "+",
    "unary_-" -> "-",
    "unary_!" -> "!",
    "unary_~" -> "~"
  )

  final case class Identifier[T](name: String, original: T) extends TargetTermView[T]
  final case class Literal[T](value: String, original: T) extends TargetTermView[T]
  final case class Select[T](qualifier: TargetTermView[T], name: String, original: T) extends TargetTermView[T]
  final case class Apply[T](function: TargetTermView[T], arguments: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class Infix[T](left: TargetTermView[T], operator: String, right: TargetTermView[T], original: T) extends TargetTermView[T]
  final case class Unary[T](operator: String, operand: TargetTermView[T], original: T) extends TargetTermView[T]
  final case class Typed[T](expression: TargetTermView[T], typeName: String, original: T) extends TargetTermView[T]
  final case class Tuple[T](elements: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class If[T](condition: TargetTermView[T], thenBranch: TargetTermView[T], elseBranch: TargetTermView[T], original: T) extends TargetTermView[T]

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
        case q.reflect.Literal(BooleanConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal(value.toString, current))
        case q.reflect.Select(operand, name) if UnaryOperatorByMethod.contains(name) =>
          val current = unwrapWrappers(term)
          extract(operand).map(TargetTermView.Unary(UnaryOperatorByMethod(name), _, current))
        case q.reflect.Select(qualifier, name) =>
          val current = unwrapWrappers(term)
          extract(qualifier).map(TargetTermView.Select(_, name, current))
        case q.reflect.Apply(function, arguments) if tupleArity(function).contains(arguments.length) =>
          val current = unwrapWrappers(term)
          sequence(arguments.map(extract)).map(TargetTermView.Tuple(_, current))
        case q.reflect.Apply(function, arguments) =>
          val current = unwrapWrappers(term)
          for
            extractedFunction <- extract(function)
            extractedArguments <- sequence(arguments.map(extract))
          yield TargetTermView.Apply(extractedFunction, extractedArguments, current)
        case q.reflect.Typed(expression, typeTree) =>
          val current = unwrapWrappers(term)
          extract(expression).map(TargetTermView.Typed(_, renderType(typeTree), current))
        case q.reflect.If(condition, thenBranch, elseBranch) =>
          val current = unwrapWrappers(term)
          for
            extractedCondition <- extract(condition)
            extractedThenBranch <- extract(thenBranch)
            extractedElseBranch <- extract(elseBranch)
          yield TargetTermView.If(extractedCondition, extractedThenBranch, extractedElseBranch, current)
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
      case Unary(operator, operand, _) =>
        s"Unary($operator, ${render(operand)})"
      case Typed(expression, typeName, _) =>
        s"Typed(${render(expression)}, Type($typeName))"
      case Tuple(elements, _) =>
        s"Tuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch, _) =>
        s"If(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"

  private def unwrapWrappers(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, _, inner) => unwrapWrappers(inner)
      case Block(Nil, inner: Term) => unwrapWrappers(inner)
      case ident: Ident if ident.symbol.exists =>
        ident.symbol.tree match
          case ValDef(_, _, Some(rhs)) => unwrapWrappers(rhs)
          case _ => term
      case _ => term

  private def renderType(using q: Quotes)(typeTree: q.reflect.TypeTree): String =
    import q.reflect.*
    normalizeTypeName(typeTree.tpe.show)

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other

  private val TupleSymbol: Regex = """.*Tuple([2-9]|1[0-9]|2[0-2])(\.apply|\.<init>)?$""".r

  private def tupleArity(using q: Quotes)(term: q.reflect.Term): Option[Int] =
    import q.reflect.*

    def fromSymbol(term: Term): Option[Int] =
      if term.symbol.exists then
        term.symbol.fullName match
          case TupleSymbol(arity, _) => Some(arity.toInt)
          case _ => None
      else None

    fromSymbol(term).orElse {
      term match
        case q.reflect.TypeApply(function, _) => tupleArity(function)
        case q.reflect.Select(qualifier, _) => tupleArity(qualifier)
        case _ => None
    }

  private def sequence[A](values: List[Either[MatchFailure, A]]): Either[MatchFailure, List[A]] =
    values.foldRight(Right(Nil): Either[MatchFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
