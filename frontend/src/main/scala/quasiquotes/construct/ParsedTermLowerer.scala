package quasiquotes.construct

import scala.util.control.NonFatal

import scala.quoted.{Expr, Quotes, Varargs}
import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.parser.InterpolatedStringSegments
import quasiquotes.parser.ConstructorNamePolicy
import quasiquotes.parser.Lambda1DiagnosticMessages
import quasiquotes.parser.P1BlockDiagnosticMessages
import quasiquotes.parser.P2LocalValDiagnosticMessages
import quasiquotes.parser.P2LocalValUntypedAdmission
import quasiquotes.parser.TypeShapeInspector
import quasiquotes.types.{TypeReprLowerer, toTypeRepr}

object ParsedTermLowerer:
  private val UnaryMethodByOperator = Map(
    "+" -> "unary_+",
    "-" -> "unary_-",
    "!" -> "unary_!",
    "~" -> "unary_~"
  )

  def lower(using q: Quotes)(
      tree: untpd.Tree,
      bindings: Vector[PlaceholderBinding[q.reflect.Term, q.reflect.TypeRepr]],
      literalCategorizedNames: Set[String] = Set.empty
  ): Either[QuasiquoteError, q.reflect.Term] =
    lowerLocated(tree, bindings, literalCategorizedNames).left.map(_.error)

  private[construct] def lowerLocated(using q: Quotes)(
      tree: untpd.Tree,
      bindings: Vector[PlaceholderBinding[q.reflect.Term, q.reflect.TypeRepr]],
      literalCategorizedNames: Set[String] = Set.empty
  ): Either[QuasiquoteLoweringFailure, q.reflect.Term] =
    import q.reflect.*

    val placeholderIndex = new CategorizedPlaceholderIndex(bindings, literalCategorizedNames)

    def lowerTerm(
        tree: untpd.Tree,
        boundTerms: List[(String, Term)] = Nil,
        lambdaDepth: Int = 0,
        binderContext: Option[(String, String)] = None,
        applicationFunction: Boolean = false
    ): Either[QuasiquoteLoweringFailure, Term] =
      def lowerChild(child: untpd.Tree): Either[QuasiquoteLoweringFailure, Term] =
        lowerTerm(child, boundTerms, lambdaDepth, binderContext)

      tree match
        case untpd.Ident(name) =>
          val text = name.toString
          placeholderIndex
            .resolve(text, PlaceholderCategory.TermSplice, PlaceholderPosition.Term)
            .left.map(located(_, tree))
            .flatMap {
              case Some(PlaceholderBinding(_, QuasiquoteHole.Term(term))) =>
                if binderContext.nonEmpty && containsOwnedDefinition(term) then
                  val (nodeKind, detail) = binderContext.get
                  Left(
                    located(
                      QuasiquoteError.UnsupportedTree(
                        nodeKind,
                        detail
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
                  Lambda1DiagnosticMessages.NestedLambda
                ),
                tree
              )
            )
          else
            parameters match
              case (parameter: untpd.ValDef) :: Nil if !parameter.tpt.isEmpty =>
                lowerType(parameter.tpt, placeholderIndex).flatMap { parameterTypeTree =>
                  val parameterType = parameterTypeTree.tpe
                  val previewSymbol = Symbol.newVal(
                    Symbol.spliceOwner,
                    parameter.name.toString,
                    parameterType,
                    Flags.EmptyFlags,
                    Symbol.noSymbol
                  )
                  val previewParameter = Ref(previewSymbol)
                  lowerTerm(
                    body,
                    (parameter.name.toString -> previewParameter) :: boundTerms,
                    lambdaDepth + 1,
                    Some("Lambda1Splice" -> Lambda1DiagnosticMessages.OwnedDefinitionSplice)
                  ).flatMap { previewBody =>
                    val methodType = MethodType(List(parameter.name.toString))(
                      _ => List(parameterType),
                      _ => previewBody.tpe.widen
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
                          lambdaDepth + 1,
                          Some("Lambda1Splice" -> Lambda1DiagnosticMessages.OwnedDefinitionSplice)
                        ) match
                          case Right(lowered) => lowered
                          case Left(failure) =>
                            bodyFailure = Some(failure)
                            Literal(UnitConstant())
                    )
                    bodyFailure.toLeft(lambda)
                  }
                }
              case _ :: Nil =>
                Left(
                  located(
                    QuasiquoteError.UnsupportedTree(
                      "Lambda1",
                      Lambda1DiagnosticMessages.ExplicitParameterType
                    ),
                    tree
                  )
                )
              case _ =>
                Left(
                  located(
                    QuasiquoteError.UnsupportedTree(
                      "Lambda1",
                      Lambda1DiagnosticMessages.ExactlyOneParameter
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
            loweredSelect <- placeholderIndex
              .resolve(
                name.toString,
                PlaceholderCategory.SelectedMemberNameSplice,
                PlaceholderPosition.SelectedMemberName
              )
              .left.map(located(_, tree))
              .flatMap {
                case Some(PlaceholderBinding(_, QuasiquoteHole.SelectedMemberNameSplice(selectedName))) =>
                  selectDynamicMember(
                    loweredQualifier,
                    selectedName.decoded,
                    normalizeSelection = !applicationFunction
                  ).left.map(located(_, tree))
                case Some(_) => Left(located(QuasiquoteError.UnknownPlaceholder(name.toString), tree))
                case None =>
                  selectMember(
                    loweredQualifier,
                    name.toString,
                    normalizeSelection = !applicationFunction
                  ).left.map(located(_, tree))
              }
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
              loweredArguments <- sequenceLocated(arguments.map(lowerChild))
              lowered <- lowerConstructorType(
                typeTree,
                loweredArguments,
                placeholderIndex
              )
            yield lowered
        case untpd.Apply(function, arguments) =>
          for
            loweredFunction <- lowerTerm(
              function,
              boundTerms,
              lambdaDepth,
              binderContext,
              applicationFunction = true
            )
            loweredArguments <- sequenceLocated(arguments.map(lowerChild))
            applied <- applyFunction(loweredFunction, loweredArguments).left.map(located(_, tree))
          yield applied
        case untpd.InfixOp(left, op, right) =>
          for
            _ <- rejectSelectedMemberNameHole(
              op,
              placeholderIndex,
              "in dynamic infix position"
            )
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
        case untpd.Block(Nil, result) =>
          lowerChild(result)
        case block @ untpd.Block(statements, result) =>
          statements match
            case (value: untpd.ValDef) :: Nil =>
              lowerLocalValBlock(
                value,
                result,
                boundTerms,
                lambdaDepth,
                binderContext,
                placeholderIndex,
                (child, terms, depth, context) =>
                  lowerTerm(child, terms, depth, context)
              )
            case values if values.exists(_.isInstanceOf[untpd.ValDef]) =>
              Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.ExactlyOne), block))
            case definitions if definitions.exists(_.isInstanceOf[untpd.DefDef]) =>
              val definition = definitions.find(_.isInstanceOf[untpd.DefDef]).get
              Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.LocalDef), definition))
            case patternDefinitions if patternDefinitions.exists(isPatternDefinition) =>
              val pattern = patternDefinitions.find(isPatternDefinition).get
              Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.Pattern), pattern))
            case expressionStatements if expressionStatements.forall(_.isTerm) =>
              for
                loweredStatements <- sequenceLocated(expressionStatements.map(lowerChild))
                loweredResult <- lowerChild(result)
              yield Block(loweredStatements, loweredResult)
            case statement :: _ =>
              Left(
                located(
                  QuasiquoteError.UnsupportedTree(
                    "Block",
                    P1BlockDiagnosticMessages.UnsupportedStatement(statement.getClass.getSimpleName)
                  ),
                  statement
                )
              )
            case Nil => lowerChild(result)
        case untpd.Parens(inner) =>
          lowerTerm(
            inner,
            boundTerms,
            lambdaDepth,
            binderContext,
            applicationFunction
          )
        case untpd.TypedSplice(tree) =>
          lowerTerm(
            tree,
            boundTerms,
            lambdaDepth,
            binderContext,
            applicationFunction
          )
        case other =>
          unsupportedTermPlaceholderFailure(other, placeholderIndex) match
            case Some(failure) => Left(failure)
            case None => Left(located(QuasiquoteError.UnsupportedTree(other.getClass.getSimpleName, other.toString), other))

    P2LocalValUntypedAdmission
      .validate(tree)
      .left
      .map(violation =>
        located(
          QuasiquoteError.UnsupportedTree("Block", violation.message),
          tree
        )
      )
      .flatMap(_ => lowerTerm(tree))

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

  private def lowerLocalValBlock(using q: Quotes)(
      value: untpd.ValDef,
      result: untpd.Tree,
      boundTerms: List[(String, q.reflect.Term)],
      lambdaDepth: Int,
      binderContext: Option[(String, String)],
      placeholderIndex: CategorizedPlaceholderIndex[q.reflect.Term, q.reflect.TypeRepr],
      lowerTerm: (
          untpd.Tree,
          List[(String, q.reflect.Term)],
          Int,
          Option[(String, String)]
      ) => Either[QuasiquoteLoweringFailure, q.reflect.Term]
  ): Either[QuasiquoteLoweringFailure, q.reflect.Term] =
    import q.reflect.*

    val displayName = value.name.toString
    if value.mods.is(dotty.tools.dotc.core.Flags.Mutable) then
      Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.Mutable), value))
    else if value.mods.is(dotty.tools.dotc.core.Flags.Lazy) then
      Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.Lazy), value))
    else if !isSimpleBinderName(displayName) then
      Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.Pattern), value))
    else if value.tpt.isEmpty then
      Left(located(QuasiquoteError.UnsupportedTree("Block", P2LocalValDiagnosticMessages.MissingExplicitType), value))
    else
      for
        declaredTypeTree <- lowerP2DeclaredType(value.tpt, placeholderIndex)
        initializer <- lowerTerm(
          value.unforcedRhs.asInstanceOf[untpd.Tree],
          boundTerms,
          lambdaDepth,
          binderContext
        )
        symbol = Symbol.newVal(
          Symbol.spliceOwner,
          displayName,
          declaredTypeTree.tpe,
          Flags.EmptyFlags,
          Symbol.noSymbol
        )
        definition = ValDef(symbol, Some(initializer))
        loweredResult <- lowerTerm(
          result,
          (displayName -> Ref(symbol)) :: boundTerms,
          lambdaDepth,
          Some("P2LocalValSplice" -> P2LocalValDiagnosticMessages.OwnedDefinitionSplice)
        )
      yield Block(List(definition), loweredResult)

  private def isSimpleBinderName(name: String): Boolean =
    name != "_" && name.matches("[A-Za-z_$][A-Za-z0-9_$]*")

  private def isPatternDefinition(tree: untpd.Tree): Boolean =
    val kind = tree.getClass.getSimpleName
    kind.contains("PatDef") || kind.contains("Pattern") || kind.contains("Thicket")

  private def lowerType(using q: Quotes)(
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[q.reflect.Term, q.reflect.TypeRepr]
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

  private def lowerP2DeclaredType(using q: Quotes)(
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[q.reflect.Term, q.reflect.TypeRepr]
  ): Either[QuasiquoteLoweringFailure, q.reflect.TypeTree] =
    import q.reflect.*
    lowerType(tree, placeholderIndex).orElse(
      TypeReprLowerer
        .lower(TypeShapeInspector.inspect(tree))
        .left
        .map(error => located(QuasiquoteError.UnsupportedTree("TypeTree", error.message), tree))
        .map(Inferred.apply)
    )

  private def selectMember(
      using q: Quotes
  )(
      qualifier: q.reflect.Term,
      name: String,
      normalizeSelection: Boolean = true
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    try
      val selected = Select.unique(qualifier, name)
      Right(if normalizeSelection then normalizeTerm(selected) else selected)
    catch
      case NonFatal(error) =>
        Left(
          QuasiquoteError.UnsupportedSelection(
            qualifierType = qualifier.tpe.show,
            name = name,
            detail = error.getMessage.nn
          )
        )

  private def selectDynamicMember(
      using q: Quotes
  )(
      qualifier: q.reflect.Term,
      name: String,
      normalizeSelection: Boolean = true
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    val selected =
      try Right(Select.unique(qualifier, name))
      catch
        case NonFatal(error) if isNonUniqueSelectionFailure(error) =>
          Left(QuasiquoteError.NonUniqueSelectedMember(name))
        case NonFatal(_) =>
          Left(QuasiquoteError.MissingOrInaccessibleSelectedMember(name))

    selected.flatMap { term =>
      if term.symbol == Symbol.noSymbol ||
          term.symbol.flags.is(Flags.Private) ||
          term.symbol.flags.is(Flags.Protected)
      then
        Left(QuasiquoteError.MissingOrInaccessibleSelectedMember(name))
      else
        try Right(if normalizeSelection then normalizeTerm(term) else term)
        catch case NonFatal(_) => Left(QuasiquoteError.SelectedMemberLoweringFailure(name))
    }

  private def isNonUniqueSelectionFailure(error: Throwable): Boolean =
    val detail = Option(error.getMessage).getOrElse("").toLowerCase(java.util.Locale.ROOT)
    detail.contains("overload") ||
      detail.contains("more than one") ||
      detail.contains("not unique") ||
      detail.contains("multiple alternative")

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

  private def lowerConstructorType(using q: Quotes)(
      typeTree: untpd.Tree,
      arguments: List[q.reflect.Term],
      placeholderIndex: CategorizedPlaceholderIndex[q.reflect.Term, q.reflect.TypeRepr]
  ): Either[QuasiquoteLoweringFailure, q.reflect.Term] =
    typeTree match
      case untpd.Ident(name) =>
        val text = name.toString
        placeholderIndex
          .resolve(
            text,
            PlaceholderCategory.ReflectedTypeSplice,
            PlaceholderPosition.ConstructorType
          )
          .left.map(located(_, typeTree))
          .flatMap {
            case Some(PlaceholderBinding(_, QuasiquoteHole.ReflectedTypeSplice(reflectedType))) =>
              lowerReflectedConstructor(reflectedType, arguments)
                .left.map(located(_, typeTree))
            case Some(_) =>
              Left(located(QuasiquoteError.UnknownPlaceholder(text), typeTree))
            case None =>
              lowerNamedConstructor(typeTree, arguments)
          }
      case _ =>
        unsupportedConstructorTypePlaceholderFailure(typeTree, placeholderIndex) match
          case Some(failure) => Left(failure)
          case None => lowerNamedConstructor(typeTree, arguments)

  private def lowerNamedConstructor(using q: Quotes)(
      typeTree: untpd.Tree,
      arguments: List[q.reflect.Term]
  ): Either[QuasiquoteLoweringFailure, q.reflect.Term] =
    for
      constructorName <- renderConstructorName(typeTree).left.map(located(_, typeTree))
      _ <- ConstructorNamePolicy
        .validate(constructorName)
        .left.map(detail => located(QuasiquoteError.InvalidConstructorName(constructorName, detail), typeTree))
      lowered <- lowerConstructor(constructorName, arguments).left.map(located(_, typeTree))
    yield lowered

  private def lowerReflectedConstructor(using q: Quotes)(
      constructorType: q.reflect.TypeRepr,
      arguments: List[q.reflect.Term]
  ): Either[QuasiquoteError, q.reflect.Term] =
    import q.reflect.*

    val label =
      try
        val fullName = constructorType.typeSymbol.fullName
        if fullName.nonEmpty then fullName else constructorType.show
      catch case NonFatal(_) => "<caller-owned TypeRepr>"

    try
      val created = New(Inferred(constructorType))
      Right(Select.overloaded(created, "<init>", Nil, arguments))
    catch
      case NonFatal(error) =>
        Left(
          QuasiquoteError.UnsupportedConstructorApplication(
            label,
            Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
          )
        )

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

  private def unsupportedTermPlaceholderFailure[T, ReflectedType](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T, ReflectedType]
  ): Option[QuasiquoteLoweringFailure] =
    placeholderIndex.firstUnknownOccurrence(tree).map { occurrence =>
      QuasiquoteLoweringFailure(QuasiquoteError.UnknownPlaceholder(occurrence.name), occurrence.generatedSpan)
    }.orElse {
      placeholderIndex.findOccurrences(tree).collectFirst {
        case PlaceholderOccurrence(
              PlaceholderBinding(_, _: QuasiquoteHole.SelectedMemberNameSplice),
              span
            ) =>
          QuasiquoteLoweringFailure(
            QuasiquoteError.UnsupportedSelectedMemberNamePosition("in unsupported term syntax"),
            span
          )
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
        case PlaceholderOccurrence(binding @ PlaceholderBinding(_, _: QuasiquoteHole.ReflectedTypeSplice[?]), span) =>
          val position = tree match
            case _: untpd.TypeApply => PlaceholderPosition.UnsupportedType("method type arguments")
            case _ => PlaceholderPosition.UnsupportedTerm("unsupported term syntax")
          QuasiquoteLoweringFailure(
            QuasiquoteError.UnsupportedPlaceholderPosition(
              binding.name,
              PlaceholderCategory.ReflectedTypeSplice,
              position
            ),
            span
          )
      }
    }

  private def unsupportedTypePlaceholderFailure[T, ReflectedType](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T, ReflectedType]
  ): Option[QuasiquoteLoweringFailure] =
    placeholderIndex.firstUnknownOccurrence(tree).map { occurrence =>
      QuasiquoteLoweringFailure(QuasiquoteError.UnknownPlaceholder(occurrence.name), occurrence.generatedSpan)
    }.orElse {
      placeholderIndex.findOccurrences(tree).headOption.map { occurrence =>
        val error = placeholderIndex.categoryOf(occurrence.binding.hole) match
          case PlaceholderCategory.ReflectedTypeSplice =>
            QuasiquoteError.UnsupportedPlaceholderPosition(
              occurrence.binding.name,
              PlaceholderCategory.ReflectedTypeSplice,
              PlaceholderPosition.UnsupportedType("non-constructor type syntax")
            )
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
          case PlaceholderCategory.SelectedMemberNameSplice =>
            QuasiquoteError.UnsupportedSelectedMemberNamePosition("in type position")
        QuasiquoteLoweringFailure(error, occurrence.generatedSpan)
      }
    }

  private def unsupportedConstructorTypePlaceholderFailure[T, ReflectedType](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T, ReflectedType]
  ): Option[QuasiquoteLoweringFailure] =
    val occurrences = placeholderIndex.findOccurrences(tree) ++
      selectedTypeNameOccurrences(tree, placeholderIndex)

    occurrences.headOption.map { occurrence =>
      val binding = occurrence.binding
      val error = placeholderIndex.categoryOf(binding.hole) match
        case PlaceholderCategory.ReflectedTypeSplice =>
          QuasiquoteError.UnsupportedPlaceholderPosition(
            binding.name,
            PlaceholderCategory.ReflectedTypeSplice,
            PlaceholderPosition.UnsupportedType("partial or applied constructor type syntax")
          )
        case PlaceholderCategory.ConstructedTypeSplice =>
          QuasiquoteError.UnsupportedPlaceholderPosition(
            binding.name,
            PlaceholderCategory.ConstructedTypeSplice,
            PlaceholderPosition.ConstructorType
          )
        case PlaceholderCategory.TermSplice =>
          QuasiquoteError.PlaceholderCategoryMismatch(
            binding.name,
            PlaceholderCategory.TermSplice,
            PlaceholderPosition.ConstructorType
          )
        case PlaceholderCategory.SelectedMemberNameSplice =>
          QuasiquoteError.UnsupportedSelectedMemberNamePosition("in constructor-name position")
      QuasiquoteLoweringFailure(error, occurrence.generatedSpan)
    }

  private def selectedTypeNameOccurrences[T, ReflectedType](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T, ReflectedType]
  ): List[PlaceholderOccurrence[T, ReflectedType]] =
    tree match
      case selected @ untpd.Select(qualifier, name) =>
        val current = placeholderIndex.lookup(name.toString).toList.map { binding =>
          PlaceholderOccurrence(binding, DottySourceSpanAdapter.fromTree(selected))
        }
        selectedTypeNameOccurrences(qualifier, placeholderIndex) ++ current
      case untpd.AppliedTypeTree(constructor, arguments) =>
        selectedTypeNameOccurrences(constructor, placeholderIndex) ++
          arguments.flatMap(selectedTypeNameOccurrences(_, placeholderIndex))
      case _ => Nil

  private def rejectSelectedMemberNameHole[T, ReflectedType](
      tree: untpd.Tree,
      placeholderIndex: CategorizedPlaceholderIndex[T, ReflectedType],
      context: String
  ): Either[QuasiquoteLoweringFailure, Unit] =
    placeholderIndex.findOccurrences(tree).collectFirst {
      case PlaceholderOccurrence(
            PlaceholderBinding(_, _: QuasiquoteHole.SelectedMemberNameSplice),
            span
          ) =>
        QuasiquoteLoweringFailure(
          QuasiquoteError.UnsupportedSelectedMemberNamePosition(context),
          span
        )
    }.toLeft(())

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
