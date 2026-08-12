package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.{Expr, Quotes, Varargs}
import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.parser.InterpolatedStringSegments
import quasiquotes.parser.ConstructorNamePolicy
import quasiquotes.types.toTypeRepr

object ParsedTermLowerer:
  private val UnaryMethodByOperator = Map(
    "+" -> "unary_+",
    "-" -> "unary_-",
    "!" -> "unary_!",
    "~" -> "unary_~"
  )

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

    def lowerTerm(
        tree: untpd.Tree,
        boundTerms: List[(String, Term)] = Nil,
        lambdaDepth: Int = 0
    ): Either[QuasiquoteLoweringFailure, Term] =
      def lowerChild(child: untpd.Tree): Either[QuasiquoteLoweringFailure, Term] =
        lowerTerm(child, boundTerms, lambdaDepth)

      tree match
        case untpd.Ident(name) =>
          val text = name.toString
          placeholderIndex
            .resolve(text, PlaceholderCategory.TermSplice, PlaceholderPosition.Term)
            .left.map(located(_, tree))
            .flatMap {
              case Some(PlaceholderBinding(_, QuasiquoteHole.Term(term))) =>
                if lambdaDepth > 0 && containsOwnedDefinition(term) then
                  Left(
                    located(
                      QuasiquoteError.UnsupportedTree(
                        "Lambda1Splice",
                        "term splice containing owned definitions is not supported inside Lambda1"
                      ),
                      tree
                    )
                  )
                else Right(term)
              case Some(_) => Left(located(QuasiquoteError.UnknownPlaceholder(text), tree))
              case None =>
                boundTerms.collectFirst { case (`text`, term) => term } match
                  case Some(term) => Right(term)
                  case None => IdentifierResolver.resolve(text).left.map(located(_, tree))
            }
        case untpd.Function(parameters, body) =>
          if lambdaDepth > 0 then
            Left(
              located(
                QuasiquoteError.UnsupportedTree(
                  "Lambda1",
                  "nested lambdas are outside the bounded Lambda1 tranche"
                ),
                tree
              )
            )
          else
            parameters match
              case (parameter: untpd.ValDef) :: Nil if !parameter.tpt.isEmpty =>
                lowerType(parameter.tpt, placeholderIndex).flatMap { parameterTypeTree =>
                  val methodType = MethodType(List(parameter.name.toString))(
                    _ => List(parameterTypeTree.tpe),
                    _ => TypeRepr.of[Any]
                  )
                  var bodyFailure: Option[QuasiquoteLoweringFailure] = None
                  val lambda = Lambda(
                    owner = Symbol.spliceOwner,
                    tpe = methodType,
                    rhsFn = (_, parameters) =>
                      val parameterTerm = parameters.head.asInstanceOf[Term]
                      lowerTerm(
                        body,
                        (parameter.name.toString -> parameterTerm) :: boundTerms,
                        lambdaDepth + 1
                      ) match
                        case Right(lowered) => lowered
                        case Left(failure) =>
                          bodyFailure = Some(failure)
                          Literal(UnitConstant())
                  )
                  bodyFailure.toLeft(lambda)
                }
              case _ :: Nil =>
                Left(
                  located(
                    QuasiquoteError.UnsupportedTree(
                      "Lambda1",
                      "an explicit parameter type is required"
                    ),
                    tree
                  )
                )
              case _ =>
                Left(
                  located(
                    QuasiquoteError.UnsupportedTree(
                      "Lambda1",
                      "exactly one parameter is required"
                    ),
                    tree
                  )
                )
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
            loweredQualifier <- lowerChild(qualifier)
            loweredSelect <- selectMember(loweredQualifier, name.toString).left.map(located(_, tree))
          yield loweredSelect
        case multiple @ untpd.Apply(untpd.Apply(untpd.Select(_: untpd.New, init), _), _)
            if init.toString == "<init>" =>
          Left(located(QuasiquoteError.UnsupportedTree("ConstructorNew", "multiple constructor argument lists are not supported"), multiple))
        case constructor @ untpd.Apply(untpd.Select(untpd.New(typeTree), init), arguments)
            if init.toString == "<init>" =>
          if arguments.exists(_.isInstanceOf[untpd.NamedArg]) then
            Left(located(QuasiquoteError.UnsupportedTree("ConstructorNew", "named constructor arguments are not supported"), constructor))
          else
            for
              constructorName <- renderConstructorName(typeTree).left.map(located(_, constructor))
              _ <- ConstructorNamePolicy
                .validate(constructorName)
                .left.map(detail => located(QuasiquoteError.InvalidConstructorName(constructorName, detail), typeTree))
              loweredArguments <- sequenceLocated(arguments.map(lowerChild))
              lowered <- lowerConstructor(constructorName, loweredArguments).left.map(located(_, constructor))
            yield lowered
        case untpd.Apply(function, arguments) =>
          for
            loweredFunction <- lowerChild(function)
            loweredArguments <- sequenceLocated(arguments.map(lowerChild))
            applied <- applyFunction(loweredFunction, loweredArguments).left.map(located(_, tree))
          yield applied
        case untpd.InfixOp(left, op, right) =>
          for
            loweredLeft <- lowerChild(left)
            loweredRight <- lowerChild(right)
            applied <- applyInfix(loweredLeft, op.name.toString, loweredRight).left.map(located(_, tree))
          yield applied
        case untpd.PrefixOp(untpd.Ident(operator), operand) if UnaryMethodByOperator.contains(operator.toString) =>
          for
            loweredOperand <- lowerChild(operand)
            loweredUnary <- applyUnary(loweredOperand, operator.toString).left.map(located(_, tree))
          yield loweredUnary
        case interpolation @ untpd.InterpolatedString(prefix, segments) =>
          if prefix.toString != "s" then
            Left(located(QuasiquoteError.UnsupportedTree("InterpolatedString", s"Unsupported prefix: ${prefix.toString}"), interpolation))
          else
            InterpolatedStringSegments.decode(segments) match
              case Left(detail) =>
                Left(located(QuasiquoteError.UnsupportedTree("InterpolatedString", detail), interpolation))
              case Right(decoded) =>
                for
                  loweredArguments <- sequenceLocated(decoded.arguments.map(lowerChild))
                  lowered <- lowerSInterpolation(decoded.parts, loweredArguments).left.map(located(_, interpolation))
                yield lowered
        case untpd.Typed(expression, typeTree) =>
          for
            loweredExpression <- lowerChild(expression)
            loweredType <- lowerType(typeTree, placeholderIndex)
          yield Typed(loweredExpression, loweredType)
        case untpd.Tuple(elements) =>
          for
            loweredElements <- sequenceLocated(elements.map(lowerChild))
            loweredTuple <- makeTuple(loweredElements).left.map(located(_, tree))
          yield loweredTuple
        case untpd.If(condition, thenBranch, elseBranch) =>
          for
            loweredCondition <- lowerChild(condition)
            loweredThenBranch <- lowerChild(thenBranch)
            loweredElseBranch <- lowerChild(elseBranch)
          yield If(loweredCondition, loweredThenBranch, loweredElseBranch)
        case untpd.Parens(inner) =>
          lowerChild(inner)
        case untpd.TypedSplice(tree) =>
          lowerChild(tree)
        case other =>
          unsupportedTermPlaceholderFailure(other, placeholderIndex) match
            case Some(failure) => Left(failure)
            case None => Left(located(QuasiquoteError.UnsupportedTree(other.getClass.getSimpleName, other.toString), other))

    lowerTerm(tree)

  private def containsOwnedDefinition(using q: Quotes)(term: q.reflect.Term): Boolean =
    import q.reflect.*
    var found = false
    val traverser = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case _: ValDef | _: DefDef | _: ClassDef => found = true
          case _ if !found => super.traverseTree(tree)(owner)
          case _ => ()
    traverser.traverseTree(term)(Symbol.spliceOwner)
    found

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

  private def lowerConstructor(using q: Quotes)(
      name: String,
      arguments: List[q.reflect.Term]
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*
    val classSymbol =
      try
        Class.forName(name, false, ParsedTermLowerer.getClass.getClassLoader)
        Right(Symbol.requiredClass(name))
      catch
        case NonFatal(error) => Left(QuasiquoteError.UnresolvedConstructor(name, error.getMessage.nn))
    classSymbol.flatMap { symbol =>
      try
        val created = New(TypeTree.ref(symbol))
        Right(Select.overloaded(created, "<init>", Nil, arguments))
      catch
        case NonFatal(error) => Left(QuasiquoteError.UnsupportedConstructorApplication(name, error.getMessage.nn))
    }

  private def renderConstructorName(tree: untpd.Tree): Either[QuasiquoteError, String] =
    tree match
      case untpd.Ident(name) => Right(name.toString)
      case untpd.Select(qualifier, name) => renderConstructorName(qualifier).map(_ + "." + name.toString)
      case _: untpd.AppliedTypeTree =>
        Left(QuasiquoteError.UnsupportedTree("ConstructorNew", "constructor type arguments are not supported"))
      case other =>
        Left(QuasiquoteError.UnsupportedTree("ConstructorNew", s"unsupported constructor type syntax: ${other.getClass.getSimpleName}"))

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

  private def applyUnary(
      using q: Quotes
  )(
      operand: q.reflect.Term,
      operator: String
  ): Either[QuasiquoteError, q.reflect.Term] =
    UnaryMethodByOperator.get(operator) match
      case Some(methodName) => selectMember(operand, methodName)
      case None =>
        Left(
          QuasiquoteError.UnsupportedTree(
            "PrefixOp",
            s"Unsupported unary operator: $operator"
          )
        )

  private def lowerSInterpolation(using q: Quotes)(
      parts: List[String],
      arguments: List[q.reflect.Term]
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*
    try
      val partExpressions = parts.map(Expr(_))
      val argumentExpressions = arguments.map(_.asExpr)
      Right('{ StringContext(${Varargs(partExpressions)}*).s(${Varargs(argumentExpressions)}*) }.asTerm)
    catch
      case NonFatal(error) =>
        Left(QuasiquoteError.UnsupportedApplication(error.getMessage.nn))

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
