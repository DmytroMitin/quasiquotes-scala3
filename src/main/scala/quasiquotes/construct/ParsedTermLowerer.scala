package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.{Expr, Quotes}
import dotty.tools.dotc.ast.untpd

object ParsedTermLowerer:
  def lower(using q: Quotes)(tree: untpd.Tree, holes: Vector[q.reflect.Term]): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    def lowerTerm(tree: untpd.Tree): Either[QuasiquoteError, Term] =
      tree match
        case untpd.Ident(name) =>
          val text = name.toString
          if quasiquotes.parser.Placeholder.isPlaceholder(text) then resolvePlaceholder(text, holes)
          else IdentifierResolver.resolve(text)
        case untpd.Literal(constant) =>
          constant.value match
            case value: String => Right(Literal(StringConstant(value)))
            case value: Int => Right(Literal(IntConstant(value)))
            case value: Boolean => Right(Literal(BooleanConstant(value)))
            case value => Left(QuasiquoteError.UnsupportedLiteral(String.valueOf(value)))
        case untpd.Number(digits, _) =>
          scala.util.Try(digits.toInt).toEither.left.map(_ => QuasiquoteError.UnsupportedLiteral(digits)).map(value => Literal(IntConstant(value)))
        case untpd.Select(qualifier, name) =>
          for
            loweredQualifier <- lowerTerm(qualifier)
            loweredSelect <- selectMember(loweredQualifier, name.toString)
          yield loweredSelect
        case untpd.Apply(function, arguments) =>
          for
            loweredFunction <- lowerTerm(function)
            loweredArguments <- sequence(arguments.map(lowerTerm))
            applied <- applyFunction(loweredFunction, loweredArguments)
          yield applied
        case untpd.InfixOp(left, op, right) =>
          for
            loweredLeft <- lowerTerm(left)
            loweredRight <- lowerTerm(right)
            applied <- applyInfix(loweredLeft, op.name.toString, loweredRight)
          yield applied
        case untpd.Typed(expression, typeTree) =>
          for
            loweredExpression <- lowerTerm(expression)
            loweredType <- lowerType(typeTree)
          yield Typed(loweredExpression, loweredType)
        case untpd.Tuple(elements) =>
          for
            loweredElements <- sequence(elements.map(lowerTerm))
            loweredTuple <- makeTuple(loweredElements)
          yield loweredTuple
        case untpd.If(condition, thenBranch, elseBranch) =>
          for
            loweredCondition <- lowerTerm(condition)
            loweredThenBranch <- lowerTerm(thenBranch)
            loweredElseBranch <- lowerTerm(elseBranch)
          yield If(loweredCondition, loweredThenBranch, loweredElseBranch)
        case untpd.Parens(inner) =>
          lowerTerm(inner)
        case untpd.TypedSplice(tree) =>
          lowerTerm(tree)
        case other =>
          Left(QuasiquoteError.UnsupportedTree(other.getClass.getSimpleName, other.toString))

    lowerTerm(tree)

  private def lowerType(using q: Quotes)(tree: untpd.Tree): Either[QuasiquoteError, q.reflect.TypeTree] =
    import q.reflect.*
    renderType(tree) match
      case "Int" | "scala.Int" => Right(TypeTree.of[Int])
      case "String" | "scala.String" => Right(TypeTree.of[String])
      case "Boolean" | "scala.Boolean" => Right(TypeTree.of[Boolean])
      case other => Left(QuasiquoteError.UnsupportedTree("TypeTree", s"Unsupported type ascription: $other"))

  private def resolvePlaceholder(
      using q: Quotes
  )(
      name: String,
      holes: Vector[q.reflect.Term]
  ): Either[QuasiquoteError, q.reflect.Term] =
    val indexText = name.stripPrefix("__hole")
    scala.util.Try(indexText.toInt).toEither.left.map(_ => QuasiquoteError.InvalidPlaceholderName(name)).flatMap { index =>
      holes.lift(index).toRight(QuasiquoteError.MissingPlaceholder(index))
    }

  private def selectMember(
      using q: Quotes
  )(
      qualifier: q.reflect.Term,
      name: String
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    try Right(normalizeTerm(Select.unique(qualifier, name)))
    catch
      case NonFatal(error) =>
        Left(
          QuasiquoteError.UnsupportedSelection(
            qualifierType = qualifier.tpe.show,
            name = name,
            detail = error.getMessage.nn
          )
        )

  private def applyFunction(
      using q: Quotes
  )(
      function: q.reflect.Term,
      arguments: List[q.reflect.Term]
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    try Right(function.appliedToArgs(arguments))
    catch
      case NonFatal(_) =>
        try Right(Select.unique(function, "apply").appliedToArgs(arguments))
        catch
          case NonFatal(error) =>
            Left(QuasiquoteError.UnsupportedApplication(error.getMessage.nn))

  private def applyInfix(
      using q: Quotes
  )(
      qualifier: q.reflect.Term,
      name: String,
      argument: q.reflect.Term
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    try Right(Select.overloaded(qualifier, name, Nil, argument :: Nil))
    catch
      case NonFatal(error) =>
        Left(
          QuasiquoteError.UnsupportedApplication(
            s"Could not lower infix operator $name on ${qualifier.tpe.show}: ${error.getMessage.nn}"
          )
        )

  private def makeTuple(using q: Quotes)(elements: List[q.reflect.Term]): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    if elements.size < 2 || elements.size > 22 then
      Left(QuasiquoteError.UnsupportedTree("Tuple", s"Unsupported tuple arity: ${elements.size}"))
    else
      try
        Right(Expr.ofTupleFromSeq(elements.map(_.asExpr)).asTerm)
      catch
        case NonFatal(error) =>
          Left(QuasiquoteError.UnsupportedTree("Tuple", error.getMessage.nn))

  private def normalizeTerm(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term.tpe.widen match
      case mt: MethodType if mt.paramNames.isEmpty => term.appliedToNone
      case _ => term

  private def renderType(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => name.toString
      case untpd.Select(qualifier, name) => s"${renderType(qualifier)}.${name.toString}"
      case other => other.toString

  private def sequence[A](values: List[Either[QuasiquoteError, A]]): Either[QuasiquoteError, List[A]] =
    values.foldRight(Right(Nil): Either[QuasiquoteError, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
