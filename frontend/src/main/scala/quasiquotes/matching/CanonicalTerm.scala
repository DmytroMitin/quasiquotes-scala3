package quasiquotes.matching

import scala.quoted.Quotes
import quasiquotes.parser.BinderId

sealed trait CanonicalBlockStatement derives CanEqual

sealed trait CanonicalTerm extends CanonicalBlockStatement derives CanEqual:
  final def render: String = CanonicalTerm.render(this)

object CanonicalBlockStatement:
  private[quasiquotes] final case class LocalVal(
      declaredType: String,
      initializer: CanonicalTerm
  ) extends CanonicalBlockStatement

object CanonicalTerm:
  final case class Ident(name: String) extends CanonicalTerm
  private[quasiquotes] final case class Bound(distanceFromInnermost: Int) extends CanonicalTerm
  private[quasiquotes] final case class Lambda1(parameterType: String, body: CanonicalTerm) extends CanonicalTerm
  final case class Literal(value: String) extends CanonicalTerm
  final case class Select(qualifier: CanonicalTerm, name: String) extends CanonicalTerm
  final case class Apply(function: CanonicalTerm, arguments: List[CanonicalTerm]) extends CanonicalTerm
  final case class New(constructor: String, arguments: List[CanonicalTerm]) extends CanonicalTerm
  final case class Infix(left: CanonicalTerm, operator: String, right: CanonicalTerm) extends CanonicalTerm
  final case class Unary(operator: String, operand: CanonicalTerm) extends CanonicalTerm
  final case class InterpolatedString(
      prefix: String,
      parts: List[String],
      arguments: List[CanonicalTerm]
  ) extends CanonicalTerm
  final case class Typed(expression: CanonicalTerm, typeName: String) extends CanonicalTerm
  final case class Tuple(elements: List[CanonicalTerm]) extends CanonicalTerm
  final case class If(condition: CanonicalTerm, thenBranch: CanonicalTerm, elseBranch: CanonicalTerm) extends CanonicalTerm
  final case class Block(statements: List[CanonicalBlockStatement], result: CanonicalTerm) extends CanonicalTerm

  def render(term: CanonicalTerm): String =
    term match
      case Ident(name) => s"CIdent($name)"
      case Bound(distance) => s"CBound($distance)"
      case Lambda1(parameterType, body) =>
        s"CLambda1(Type($parameterType), ${render(body)})"
      case Literal(value) => s"CLiteral($value)"
      case Select(qualifier, name) => s"CSelect(${render(qualifier)}, $name)"
      case Apply(function, arguments) =>
        s"CApply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case New(constructor, arguments) =>
        s"CNew($constructor, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right) =>
        s"CInfix(${render(left)}, $operator, ${render(right)})"
      case Unary(operator, operand) =>
        s"CUnary($operator, ${render(operand)})"
      case InterpolatedString(prefix, parts, arguments) =>
        s"CInterpolatedString($prefix, [${parts.map(quote).mkString(", ")}], [${arguments.map(render).mkString(", ")}])"
      case Typed(expression, typeName) =>
        s"CTyped(${render(expression)}, Type($typeName))"
      case Tuple(elements) =>
        s"CTuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch) =>
        s"CIf(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"
      case Block(statements, result) =>
        s"CBlock([${statements.map(renderStatement).mkString(", ")}], ${render(result)})"

  private def renderStatement(statement: CanonicalBlockStatement): String =
    statement match
      case CanonicalBlockStatement.LocalVal(declaredType, initializer) =>
        s"CLocalVal(Type($declaredType), ${render(initializer)})"
      case term: CanonicalTerm => render(term)

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

object TermCanonicalizer:
  def canonicalize(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, CanonicalTerm] =
    MatchNormalizer.normalizedView(term).flatMap(canonicalizeView(_))

  def canonicalEqual(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): Either[MatchFailure, Boolean] =
    for
      canonicalLeft <- canonicalize(left)
      canonicalRight <- canonicalize(right)
    yield canonicalLeft == canonicalRight

  private def canonicalizeView(
      view: TargetTermView[?],
      scope: List[BinderId] = Nil
  ): Either[MatchFailure, CanonicalTerm] =
    view match
      case TargetTermView.Identifier(name, _) =>
        Right(CanonicalTerm.Ident(name))
      case TargetTermView.BoundReference(binderId, _, _) =>
        val distance = scope.indexOf(binderId)
        if distance >= 0 then Right(CanonicalTerm.Bound(distance))
        else Left(MatchFailure.UnsupportedTargetShape("lambda binder scope mismatch"))
      case TargetTermView.Lambda1(binderId, _, parameterType, _, body, _) =>
        canonicalizeView(body, binderId :: scope)
          .map(CanonicalTerm.Lambda1(parameterType, _))
      case TargetTermView.Literal(value, _) =>
        Right(CanonicalTerm.Literal(value))
      case TargetTermView.Select(qualifier, name, _) =>
        canonicalizeView(qualifier, scope).map(CanonicalTerm.Select(_, name))
      case TargetTermView.Apply(function, arguments, _) =>
        for
          canonicalFunction <- canonicalizeView(function, scope)
          canonicalArguments <- sequence(arguments.map(canonicalizeView(_, scope)))
        yield CanonicalTerm.Apply(canonicalFunction, canonicalArguments)
      case TargetTermView.New(constructor, arguments, _) =>
        sequence(arguments.map(canonicalizeView(_, scope))).map(CanonicalTerm.New(constructor, _))
      case TargetTermView.Infix(left, operator, right, _) =>
        for
          canonicalLeft <- canonicalizeView(left, scope)
          canonicalRight <- canonicalizeView(right, scope)
        yield CanonicalTerm.Infix(canonicalLeft, operator, canonicalRight)
      case TargetTermView.Unary(operator, operand, _) =>
        canonicalizeView(operand, scope).map(CanonicalTerm.Unary(operator, _))
      case TargetTermView.InterpolatedString(prefix, parts, arguments, _) =>
        sequence(arguments.map(canonicalizeView(_, scope)))
          .map(CanonicalTerm.InterpolatedString(prefix, parts, _))
      case TargetTermView.Typed(expression, typeName, _) =>
        canonicalizeView(expression, scope).map(CanonicalTerm.Typed(_, typeName))
      case TargetTermView.Tuple(elements, _) =>
        sequence(elements.map(canonicalizeView(_, scope))).map(CanonicalTerm.Tuple.apply)
      case TargetTermView.If(condition, thenBranch, elseBranch, _) =>
        for
          canonicalCondition <- canonicalizeView(condition, scope)
          canonicalThenBranch <- canonicalizeView(thenBranch, scope)
          canonicalElseBranch <- canonicalizeView(elseBranch, scope)
        yield CanonicalTerm.If(canonicalCondition, canonicalThenBranch, canonicalElseBranch)
      case TargetTermView.Block(statements, result, _) =>
        statements match
          case List(TargetBlockStatementView.LocalVal(binderId, _, declaredType, _, initializer, _)) =>
            for
              canonicalInitializer <- canonicalizeView(initializer, scope)
              canonicalResult <- canonicalizeView(result, binderId :: scope)
            yield CanonicalTerm.Block(
              List(CanonicalBlockStatement.LocalVal(declaredType, canonicalInitializer)),
              canonicalResult
            )
          case expressionStatements if expressionStatements.forall(_.isInstanceOf[TargetTermView[?]]) =>
            for
              canonicalStatements <- sequence(expressionStatements.map(statement => canonicalizeView(statement.asInstanceOf[TargetTermView[?]], scope)))
              canonicalResult <- canonicalizeView(result, scope)
            yield CanonicalTerm.Block(canonicalStatements, canonicalResult)
          case _ => Left(MatchFailure.UnsupportedTargetShape("unsupported canonical block statement sequence"))

  private def sequence[A](values: List[Either[MatchFailure, A]]): Either[MatchFailure, List[A]] =
    values.foldRight(Right(Nil): Either[MatchFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
