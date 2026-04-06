package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.Quotes
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
        case untpd.Parens(inner) =>
          lowerTerm(inner)
        case untpd.TypedSplice(tree) =>
          lowerTerm(tree)
        case other =>
          Left(QuasiquoteError.UnsupportedTree(other.getClass.getSimpleName, other.toString))

    lowerTerm(tree)

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

  private def normalizeTerm(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term.tpe.widen match
      case mt: MethodType if mt.paramNames.isEmpty => term.appliedToNone
      case _ => term

  private def sequence[A](values: List[Either[QuasiquoteError, A]]): Either[QuasiquoteError, List[A]] =
    values.foldRight(Right(Nil): Either[QuasiquoteError, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
