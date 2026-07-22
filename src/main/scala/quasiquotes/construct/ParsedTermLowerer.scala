package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.{Expr, Quotes}
import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.DottySourceSpanAdapter

object ParsedTermLowerer:
  def lower(using q: Quotes)(
      tree: untpd.Tree,
      bindings: Vector[PlaceholderBinding[q.reflect.Term]],
      literalCategorizedNames: Set[String] = Set.empty
  ): Either[QuasiquoteError, q.reflect.Term] =
    lowerLocated(tree, bindings, literalCategorizedNames).left.map(_.error)

  private[construct] def lowerLocated(using q: Quotes)(
      tree: untpd.Tree,
      bindings: Vector[PlaceholderBinding[q.reflect.Term]],
      literalCategorizedNames: Set[String] = Set.empty
  ): Either[QuasiquoteLoweringFailure, q.reflect.Term] =
    import q.reflect.*

    val placeholderIndex = new CategorizedPlaceholderIndex(bindings, literalCategorizedNames)

    def lowerTerm(tree: untpd.Tree): Either[QuasiquoteLoweringFailure, Term] =
      tree match
        case untpd.Ident(name) =>
          val text = name.toString
          placeholderIndex
            .resolve(text, PlaceholderCategory.TermSplice, PlaceholderPosition.Term)
            .left.map(located(_, tree))
            .flatMap {
              case Some(PlaceholderBinding(_, QuasiquoteHole.Term(term))) => Right(term)
              case Some(_) => Left(located(QuasiquoteError.UnknownPlaceholder(text), tree))
              case None => IdentifierResolver.resolve(text).left.map(located(_, tree))
            }
        case untpd.Literal(constant) =>
          constant.value match
            case value: String => Right(Literal(StringConstant(value)))
            case value: Int => Right(Literal(IntConstant(value)))
            case value: Boolean => Right(Literal(BooleanConstant(value)))
            case value => Left(located(QuasiquoteError.UnsupportedLiteral(String.valueOf(value)), tree))
        case untpd.Number(digits, _) =>
          scala.util.Try(digits.toInt).toEither
            .left.map(_ => located(QuasiquoteError.UnsupportedLiteral(digits), tree))
            .map(value => Literal(IntConstant(value)))
        case untpd.Select(qualifier, name) =>
          for
            loweredQualifier <- lowerTerm(qualifier)
            loweredSelect <- selectMember(loweredQualifier, name.toString).left.map(located(_, tree))
          yield loweredSelect
        case untpd.Apply(function, arguments) =>
          for
            loweredFunction <- lowerTerm(function)
            loweredArguments <- sequenceLocated(arguments.map(lowerTerm))
            applied <- applyFunction(loweredFunction, loweredArguments).left.map(located(_, tree))
          yield applied
        case untpd.InfixOp(left, op, right) =>
          for
            loweredLeft <- lowerTerm(left)
            loweredRight <- lowerTerm(right)
            applied <- applyInfix(loweredLeft, op.name.toString, loweredRight).left.map(located(_, tree))
          yield applied
        case untpd.Typed(expression, typeTree) =>
          for
            loweredExpression <- lowerTerm(expression)
            loweredType <- lowerType(typeTree, placeholderIndex)
          yield Typed(loweredExpression, loweredType)
        case untpd.Tuple(elements) =>
          for
            loweredElements <- sequenceLocated(elements.map(lowerTerm))
            loweredTuple <- makeTuple(loweredElements).left.map(located(_, tree))
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
          unsupportedTermPlaceholderFailure(other, placeholderIndex) match
            case Some(failure) => Left(failure)
            case None => Left(located(QuasiquoteError.UnsupportedTree(other.getClass.getSimpleName, other.toString), other))

    lowerTerm(tree)

  private def lowerType(using q: Quotes)(
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[q.reflect.Term]
  ): Either[QuasiquoteLoweringFailure, q.reflect.TypeTree] =
    import q.reflect.*
    tree match
      case untpd.Ident(name) =>
        val text = name.toString
        placeholderIndex
          .resolve(
            text,
            PlaceholderCategory.ConstructedTypeSplice,
            PlaceholderPosition.ExpressionAscriptionType
          )
          .left.map(located(_, tree))
          .flatMap {
            case Some(PlaceholderBinding(_, QuasiquoteHole.ConstructedTypeSplice(constructedType))) =>
              constructedType.toTypeRepr
                .left.map(error => QuasiquoteError.TypeSpliceLoweringFailure(error.message))
                .left.map(located(_, tree))
                .map(Inferred.apply)
            case Some(_) => Left(located(QuasiquoteError.UnknownPlaceholder(text), tree))
            case None => lowerLiteralType(tree).left.map(located(_, tree))
          }
      case _ =>
        unsupportedTypePlaceholderFailure(tree, placeholderIndex) match
          case Some(failure) => Left(failure)
          case None => lowerLiteralType(tree).left.map(located(_, tree))

  private def lowerLiteralType(using q: Quotes)(tree: untpd.Tree): Either[QuasiquoteError, q.reflect.TypeTree] =
    import q.reflect.*
    renderType(tree) match
      case "Int" | "scala.Int" => Right(TypeTree.of[Int])
      case "String" | "scala.String" => Right(TypeTree.of[String])
      case "Boolean" | "scala.Boolean" => Right(TypeTree.of[Boolean])
      case other => Left(QuasiquoteError.UnsupportedTree("TypeTree", s"Unsupported type ascription: $other"))

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

  private def unsupportedTermPlaceholderFailure[T](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T]
  ): Option[QuasiquoteLoweringFailure] =
    placeholderIndex.firstUnknownOccurrence(tree).map { occurrence =>
      QuasiquoteLoweringFailure(QuasiquoteError.UnknownPlaceholder(occurrence.name), occurrence.generatedSpan)
    }.orElse {
      placeholderIndex.findOccurrences(tree).collectFirst {
        case PlaceholderOccurrence(binding @ PlaceholderBinding(_, _: QuasiquoteHole.ConstructedTypeSplice), span) =>
          val position = tree match
            case _: untpd.TypeApply => PlaceholderPosition.UnsupportedType("method type arguments")
            case _ => PlaceholderPosition.UnsupportedTerm("unsupported term syntax")
          QuasiquoteLoweringFailure(
            QuasiquoteError.UnsupportedPlaceholderPosition(
              binding.name,
              placeholderIndex.categoryOf(binding.hole),
              position
            ),
            span
          )
      }
    }

  private def unsupportedTypePlaceholderFailure[T](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T]
  ): Option[QuasiquoteLoweringFailure] =
    placeholderIndex.firstUnknownOccurrence(tree).map { occurrence =>
      QuasiquoteLoweringFailure(QuasiquoteError.UnknownPlaceholder(occurrence.name), occurrence.generatedSpan)
    }.orElse {
      placeholderIndex.findOccurrences(tree).headOption.map { occurrence =>
        val error = placeholderIndex.categoryOf(occurrence.binding.hole) match
          case PlaceholderCategory.ConstructedTypeSplice =>
            QuasiquoteError.UnsupportedPlaceholderPosition(
              occurrence.binding.name,
              PlaceholderCategory.ConstructedTypeSplice,
              PlaceholderPosition.UnsupportedType("nested type syntax")
            )
          case PlaceholderCategory.TermSplice =>
            QuasiquoteError.PlaceholderCategoryMismatch(
              occurrence.binding.name,
              PlaceholderCategory.TermSplice,
              PlaceholderPosition.ExpressionAscriptionType
            )
        QuasiquoteLoweringFailure(error, occurrence.generatedSpan)
      }
    }

  private def located(error: QuasiquoteError, tree: untpd.Tree): QuasiquoteLoweringFailure =
    QuasiquoteLoweringFailure(error, DottySourceSpanAdapter.fromTree(tree))

  private def sequenceLocated[A](
      values: List[Either[QuasiquoteLoweringFailure, A]]
  ): Either[QuasiquoteLoweringFailure, List[A]] =
    values.foldRight(Right(Nil): Either[QuasiquoteLoweringFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
