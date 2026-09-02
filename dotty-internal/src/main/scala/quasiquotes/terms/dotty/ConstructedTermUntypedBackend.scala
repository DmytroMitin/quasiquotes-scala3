package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{BinderId, BlockStatement, ConstructorNamePolicy, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

private[quasiquotes] object ConstructedTermUntypedBackend:
  import ConstructedTermUntypedBackendError.*

  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val DecimalInteger = "-?[0-9]+".r

  private final case class LoweringState(
      sidecars: Vector[TypeNormalForm],
      typedOrdinal: Int,
      binders: Map[BinderId, String],
      lambdaActive: Boolean,
      ambientBindersPresent: Boolean
  ):
    def consume:
        Either[ConstructedTermUntypedBackendError, (TypeNormalForm, LoweringState)] =
      sidecars
        .lift(typedOrdinal)
        .map(value => value -> copy(typedOrdinal = typedOrdinal + 1))
        .toRight(MissingTypeSidecar(typedOrdinal))

  def lower(
      constructed: ConstructedTerm
  ): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    Option(constructed).toRight(MissingConstructedTerm).flatMap(lowerUsing(_, Map.empty))

  private[quasiquotes] def lowerInScope(
      constructed: ConstructedTerm,
      binderId: BinderId,
      declarationName: String
  ): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    lowerInScopes(constructed, Vector(binderId -> declarationName))

  private[quasiquotes] def lowerInScopes(
      constructed: ConstructedTerm,
      binders: Vector[(BinderId, String)]
  ): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    validateBinderScope(binders).flatMap(lowerUsing(constructed, _))

  private def validateBinderScope(
      binders: Vector[(BinderId, String)]
  ): Either[ConstructedTermUntypedBackendError, Map[BinderId, String]] =
    if binders == null then Left(MalformedBinderScope("the binding vector was null."))
    else
      binders.zipWithIndex.collectFirst {
        case (null, index) =>
          MalformedBinderScope(s"binding $index was null.")
        case ((null, _), index) =>
          MalformedBinderScope(s"binder identity at binding $index was null.")
        case ((_, null), index) =>
          MalformedBinderScope(s"declaration name at binding $index was null.")
        case ((_, name), index) if name.isEmpty =>
          MalformedBinderScope(s"declaration name at binding $index was empty.")
      } match
        case Some(error) => Left(error)
        case None =>
          binders
            .groupMapReduce(_._1)(_ => 1)(_ + _)
            .collectFirst { case (binderId, count) if count > 1 => binderId }
            .toLeft(binders.toMap)
            .left
            .map(binderId =>
              MalformedBinderScope(
                s"duplicate binder identity ${binderId.value}."
              )
            )

  private def lowerUsing(
      constructed: ConstructedTerm,
      binders: Map[BinderId, String]
  ): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    given SourceFile = NoSource

    lowerTerm(
      constructed.root,
      LoweringState(
        constructed.ascriptionTypes,
        typedOrdinal = 0,
        binders = binders,
        lambdaActive = false,
        ambientBindersPresent = binders.nonEmpty
      )
    ).flatMap { case (tree, state) =>
      Either.cond(
        state.typedOrdinal == state.sidecars.size,
        tree,
        UnconsumedTypeSidecars(state.typedOrdinal, state.sidecars.size)
      )
    }

  private def lowerTerm(
      shape: TermShape,
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.Tree, LoweringState)
  ] =
    if shape == null then Left(MissingTermShape)
    else shape match
      case TermShape.BoundReference(binderId, _) =>
        state.binders
          .get(binderId)
          .map(name => untpd.Ident(termName(name)) -> state)
          .toRight(OutOfScopeBoundReference(binderId.value))
      case TermShape.Lambda1(binderId, displayName, _, body) =>
        lowerLambda1(binderId, displayName, body, state)
      case TermShape.Identifier(name, _) =>
        Right(untpd.Ident(termName(name)) -> state)
      case TermShape.Literal(value) =>
        lowerLiteral(value).map(_ -> state)
      case TermShape.Select(qualifier, name) =>
        lowerTerm(qualifier, state).map { case (rawQualifier, next) =>
          untpd.Select(rawQualifier, termName(name)) -> next
        }
      case TermShape.Apply(function, arguments) =>
        for
          loweredFunction <- lowerTerm(function, state)
          (rawFunction, afterFunction) = loweredFunction
          loweredArguments <- lowerTerms(arguments, afterFunction)
          (rawArguments, afterArguments) = loweredArguments
        yield untpd.Apply(rawFunction, rawArguments) -> afterArguments
      case TermShape.New(constructor, arguments) =>
        lowerNew(constructor, arguments, state)
      case TermShape.Infix(left, operator, right) =>
        for
          loweredLeft <- lowerTerm(left, state)
          (rawLeft, afterLeft) = loweredLeft
          loweredRight <- lowerTerm(right, afterLeft)
          (rawRight, afterRight) = loweredRight
        yield
          untpd.InfixOp(
            rawLeft,
            untpd.Ident(termName(operator)),
            rawRight
          ) -> afterRight
      case TermShape.Unary(operator, operand)
          if SupportedUnaryOperators(operator) =>
        lowerTerm(operand, state).map { case (rawOperand, next) =>
          untpd.PrefixOp(
            untpd.Ident(termName(operator)),
            rawOperand
          ) -> next
        }
      case TermShape.Unary(operator, _) =>
        Left(UnsupportedUnaryOperator(operator))
      case TermShape.InterpolatedString(prefix, parts, arguments) =>
        lowerInterpolation(prefix, parts, arguments, state)
      case TermShape.Typed(expression, _) =>
        for
          consumed <- state.consume
          (sidecar, afterSidecar) = consumed
          rawType <- CompletedTypeUntypedLowerer
            .lower(sidecar)
            .left
            .map(_ => UnsupportedTypeSidecar(state.typedOrdinal, sidecar.render))
          loweredExpression <- lowerTerm(expression, afterSidecar)
          (rawExpression, afterExpression) = loweredExpression
        yield untpd.Typed(rawExpression, rawType) -> afterExpression
      case TermShape.Tuple(elements) =>
        lowerTerms(elements, state).map { case (rawElements, next) =>
          untpd.Tuple(rawElements) -> next
        }
      case TermShape.If(condition, thenBranch, elseBranch) =>
        for
          loweredCondition <- lowerTerm(condition, state)
          (rawCondition, afterCondition) = loweredCondition
          loweredThen <- lowerTerm(thenBranch, afterCondition)
          (rawThen, afterThen) = loweredThen
          loweredElse <- lowerTerm(elseBranch, afterThen)
          (rawElse, afterElse) = loweredElse
        yield untpd.If(rawCondition, rawThen, rawElse) -> afterElse
      case TermShape.Parenthesized(expression) =>
        lowerTerm(expression, state).map { case (rawExpression, next) =>
          untpd.Parens(rawExpression) -> next
        }
      case TermShape.Block(statements, result) =>
        lowerBlock(statements, result, state)
      case TermShape.Unsupported(nodeKind, _) =>
        Left(UnsupportedTermNode(nodeKind))

  private def lowerBlock(
      statements: List[BlockStatement],
      result: TermShape,
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.Tree, LoweringState)
  ] =
    val incomingBinders = state.binders
    for
      loweredPrefix <- lowerBlockStatements(statements, state)
      (rawPrefix, afterPrefix) = loweredPrefix
      loweredResult <- lowerTerm(result, afterPrefix)
      (rawResult, afterResult) = loweredResult
      restored = afterResult.copy(binders = incomingBinders)
    yield untpd.Block(rawPrefix, rawResult) -> restored

  private def lowerBlockStatements(
      statements: List[BlockStatement],
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (List[untpd.Tree], LoweringState)
  ] =
    statements.zipWithIndex.foldLeft[
      Either[
        ConstructedTermUntypedBackendError,
        (List[untpd.Tree], LoweringState)
      ]
    ](Right(Nil -> state)) { case (accumulated, (statement, index)) =>
      accumulated.flatMap { case (values, current) =>
        statement match
          case expression: TermShape =>
            lowerTerm(expression, current).map { case (raw, next) =>
              (raw :: values) -> next
            }
          case local: BlockStatement.LocalVal =>
            lowerLocalVal(local, current).map { case (raw, next) =>
              (raw :: values) -> next
            }
          case local: BlockStatement.LocalDef =>
            lowerLocalDef(local, current).map { case (raw, next) =>
              (raw :: values) -> next
            }
          case null => Left(MalformedBlock(s"prefix entry $index is null."))
      }
    }.map { case (reversed, next) => reversed.reverse -> next }

  private def lowerLocalVal(
      local: BlockStatement.LocalVal,
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.ValDef, LoweringState)
  ] =
    if !isValidBinderName(local.displayName) then
      Left(UnsupportedTermNode("LocalValName"))
    else
      for
        consumed <- state.consume
        (declaredType, afterDeclaredType) = consumed
        rawDeclaredType <- CompletedTypeUntypedLowerer
          .lower(declaredType)
          .left
          .map(_ =>
            UnsupportedTypeSidecar(
              state.typedOrdinal,
              declaredType.render
            )
          )
        loweredInitializer <- lowerTerm(local.initializer, afterDeclaredType)
        (rawInitializer, afterInitializer) = loweredInitializer
        definition = untpd.ValDef(
          termName(local.displayName),
          rawDeclaredType,
          rawInitializer
        )
        next = afterInitializer.copy(
          binders = afterInitializer.binders.updated(
            local.binderId,
            local.displayName
          )
        )
      yield definition -> next

  private def lowerLocalDef(
      local: BlockStatement.LocalDef,
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.DefDef, LoweringState)
  ] =
    if local.methodBinderId == null || local.parameterBinderId == null then
      Left(MalformedLocalDef("a binder identity was null."))
    else if local.methodBinderId == local.parameterBinderId then
      Left(MalformedLocalDef("method and parameter binder identities were equal."))
    else if !isValidBinderName(local.methodDisplayName) then
      Left(UnsupportedTermNode("LocalDefName"))
    else if !isValidBinderName(local.parameterDisplayName) then
      Left(UnsupportedTermNode("LocalDefParameterName"))
    else
      val unavailable = LocalDefBinderSpelling.unavailableKeys(state.binders.values)
      val methodName = LocalDefBinderSpelling.freshen(
        local.methodDisplayName,
        unavailable
      )
      val parameterName = LocalDefBinderSpelling.freshen(
        local.parameterDisplayName,
        unavailable
      )
      for
        consumedParameter <- state.consume
        (parameterType, afterParameterType) = consumedParameter
        rawParameterType <- CompletedTypeUntypedLowerer
          .lower(parameterType)
          .left
          .map(_ =>
            UnsupportedTypeSidecar(
              state.typedOrdinal,
              parameterType.render
            )
          )
        consumedResult <- afterParameterType.consume
        (resultType, afterResultType) = consumedResult
        rawResultType <- CompletedTypeUntypedLowerer
          .lower(resultType)
          .left
          .map(_ =>
            UnsupportedTypeSidecar(
              afterParameterType.typedOrdinal,
              resultType.render
            )
          )
        bodyState = afterResultType.copy(
          binders = state.binders
            .removed(local.methodBinderId)
            .updated(local.parameterBinderId, parameterName)
        )
        loweredBody <- lowerTerm(local.body, bodyState)
        (rawBody, afterBody) = loweredBody
        parameter = untpd
          .ValDef(
            termName(parameterName),
            rawParameterType,
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        definition = untpd
          .DefDef(
            termName(methodName),
            List(List(parameter)),
            rawResultType,
            rawBody
          )
          .withMods(untpd.Modifiers(Flags.Method))
        next = afterBody.copy(
          binders = state.binders.updated(
            local.methodBinderId,
            methodName
          )
        )
      yield definition -> next

  private def lowerLambda1(
      binderId: BinderId,
      displayName: String,
      body: TermShape,
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.Tree, LoweringState)
  ] =
    if state.lambdaActive || state.ambientBindersPresent then
      Left(NestedLambda1Unsupported)
    else if !isValidBinderName(displayName) then
      Left(UnsupportedTermNode("Lambda1ParameterName"))
    else
      for
        consumed <- state.consume
        (parameterType, afterParameterType) = consumed
        rawParameterType <- CompletedTypeUntypedLowerer
          .lower(parameterType)
          .left
          .map(_ =>
            UnsupportedTypeSidecar(
              state.typedOrdinal,
              parameterType.render
            )
          )
        bodyState = afterParameterType.copy(
          binders = afterParameterType.binders.updated(binderId, displayName),
          lambdaActive = true
        )
        loweredBody <- lowerTerm(body, bodyState)
        (rawBody, afterBody) = loweredBody
        parameter = untpd
          .ValDef(
            termName(displayName),
            rawParameterType,
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        restored = afterBody.copy(
          binders = state.binders,
          lambdaActive = state.lambdaActive
        )
        parserEquivalentBody =
          body match
            case _: TermShape.Typed => untpd.Parens(rawBody)
            case _ => rawBody
      yield untpd.Function(parameter :: Nil, parserEquivalentBody) -> restored

  private def isValidBinderName(name: String): Boolean =
    Option(name).exists(StandardSInterpolationEncoding.isPlainIdentifier)

  private def lowerNew(
      constructor: String,
      arguments: List[TermShape],
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.Tree, LoweringState)
  ] =
    for
      validated <- ConstructorNamePolicy
        .validate(constructor)
        .left
        .map(detail => InvalidConstructorName(String.valueOf(constructor), detail))
      _ <- validateConstructorArguments(arguments)
      lowered <- lowerTerms(arguments, state)
      (rawArguments, next) = lowered
      rawType = lowerConstructorTypePath(validated)
      rawNew = untpd.New(rawType)
      rawConstructor = untpd.Select(rawNew, termName("<init>"))
    yield untpd.Apply(rawConstructor, rawArguments) -> next

  private def validateConstructorArguments(
      arguments: List[TermShape]
  ): Either[ConstructedTermUntypedBackendError, Unit] =
    if arguments == null then Left(MalformedConstructorArguments(-1))
    else
      arguments.zipWithIndex.collectFirst { case (null, index) => index } match
        case Some(index) => Left(NullConstructorArgument(index))
        case None => Right(())

  private def lowerConstructorTypePath(
      constructor: String
  )(using SourceFile): untpd.Tree =
    val segments = constructor.split("\\.").toList
    val qualifier = segments.init.tail.foldLeft[untpd.Tree](
      untpd.Ident(termName(segments.head))
    ) { (prefix, segment) =>
      untpd.Select(prefix, termName(segment))
    }
    untpd.Select(qualifier, typeName(segments.last))

  private def lowerInterpolation(
      prefix: String,
      parts: List[String],
      arguments: List[TermShape],
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (untpd.Tree, LoweringState)
  ] =
    for
      _ <- validateInterpolation(prefix, parts, arguments)
      lowered <- lowerTerms(arguments, state)
      (rawArguments, next) = lowered
      rawSegments = parts.init
        .zip(arguments)
        .zip(rawArguments)
        .map { case ((part, shape), rawArgument) =>
          val encoded = StandardSInterpolationEncoding.encodePart(part)
          val literal = untpd.Literal(Constant(encoded.rawLiteralValue))
          val argument =
            if StandardSInterpolationEncoding.isDirectArgument(shape) then
              rawArgument
            else untpd.Block(Nil, rawArgument)
          untpd.Thicket(literal :: argument :: Nil)
        } :+
        untpd.Literal(
          Constant(
            StandardSInterpolationEncoding
              .encodePart(parts.last)
              .rawLiteralValue
          )
        )
    yield untpd.InterpolatedString(termName(prefix), rawSegments) -> next

  private def validateInterpolation(
      prefix: String,
      parts: List[String],
      arguments: List[TermShape]
  ): Either[ConstructedTermUntypedBackendError, Unit] =
    if prefix != "s" then
      Left(UnsupportedInterpolationPrefix(String.valueOf(prefix)))
    else if parts == null || arguments == null then
      Left(
        MalformedInterpolation(
          Option(parts).fold(-1)(_.size),
          Option(arguments).fold(-1)(_.size)
        )
      )
    else if parts.size != arguments.size + 1 then
      Left(MalformedInterpolation(parts.size, arguments.size))
    else
      parts.zipWithIndex.collectFirst { case (null, index) => index } match
        case Some(index) => Left(NullInterpolationPart(index))
        case None =>
          arguments.zipWithIndex.collectFirst { case (null, index) => index } match
            case Some(index) => Left(NullInterpolationArgument(index))
            case None => Right(())

  private def lowerTerms(
      shapes: List[TermShape],
      state: LoweringState
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    (List[untpd.Tree], LoweringState)
  ] =
    shapes.foldLeft[
      Either[
        ConstructedTermUntypedBackendError,
        (List[untpd.Tree], LoweringState)
      ]
    ](Right(Nil -> state)) { case (result, shape) =>
      result.flatMap { case (reversed, current) =>
        lowerTerm(shape, current).map { case (tree, next) =>
          (tree :: reversed) -> next
        }
      }
    }.map { case (reversed, next) => reversed.reverse -> next }

  private def lowerLiteral(
      value: String
  )(using SourceFile): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    value match
      case "true" =>
        Right(untpd.Literal(Constant(true)))
      case "false" =>
        Right(untpd.Literal(Constant(false)))
      case DecimalInteger() =>
        Right(untpd.Number(value, untpd.NumberKind.Whole(10)))
      case semanticString
          if semanticString.length >= 2 &&
            semanticString.head == '"' &&
            semanticString.last == '"' =>
        Right(
          untpd.Literal(
            Constant(semanticString.substring(1, semanticString.length - 1))
          )
        )
      case unsupported =>
        Left(UnsupportedLiteral(unsupported))
