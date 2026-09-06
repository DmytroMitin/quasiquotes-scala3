package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.terms.{ConstructedTerm, TermConstructionError}
import quasiquotes.types.TypeNormalForm

/** Exact-version source-free lowering for public semantic Term values. */
object TermUntypedLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Lowers one admitted `TermShape` through the richer completed-Term path. */
  def lower(
      term: TermShape
  )(using Context): Either[Failure, untpd.Tree] =
    Option(term)
      .toRight(
        Failure(
          "MISSING_INPUT",
          "the semantic TermShape must be present."
        )
      )
      .flatMap { present =>
        for
          _ <- validateStructure(present)
          completed <- ConstructedTerm
            .fromShape(present)
            .left
            .map(classifyCompletionFailure)
          _ <- validateFacadeLimits(completed.root)
          raw <- ConstructedTermUntypedBackend
            .lower(completed)
            .left
            .map(classifyBackendFailure)
          _ <- CoreTermShapeUntypedLowerer
            .verifySourceFree(raw)
            .left
            .map(problem => invariant("term", problem.message))
          _ <- validateRaw(completed, raw, "term")
        yield raw
      }

  private val AdmittedInfixOperators = Set(
    "+",
    "-",
    "*",
    "/",
    "%",
    "==",
    "!=",
    "<",
    "<=",
    ">",
    ">="
  )

  private type TopologyState =
    (Vector[TypeNormalForm], Int, Map[BinderId, String])

  extension (state: TopologyState)
    private def sidecars: Vector[TypeNormalForm] = state._1
    private def typedOrdinal: Int = state._2
    private def binders: Map[BinderId, String] = state._3
    private def withBinders(value: Map[BinderId, String]): TopologyState =
      (state.sidecars, state.typedOrdinal, value)
    private def consume(path: String): Either[Failure, (TypeNormalForm, TopologyState)] =
      state.sidecars
        .lift(state.typedOrdinal)
        .map(value => value -> (state.sidecars, state.typedOrdinal + 1, state.binders))
        .toRight(invariant(path, s"missing completed type sidecar ${state.typedOrdinal}."))

  private def validateStructure(term: TermShape): Either[Failure, Unit] =
    val declared = scala.collection.mutable.Set.empty[BinderId]

    def required(
        value: String,
        path: String,
        label: String,
        allowEmpty: Boolean = false
    ): Either[Failure, Unit] =
      Option(value)
        .filter(candidate => allowEmpty || candidate.nonEmpty)
        .toRight(malformed(path, s"$label must be present${if allowEmpty then "." else " and nonempty."}"))
        .map(_ => ())

    def declare(id: BinderId, path: String): Either[Failure, Unit] =
      if id == null then Left(malformed(path, "binder identity must be present."))
      else if declared(id) then
        Left(malformed(path, s"binder identity ${id.value} collides with another declaration."))
      else
        declared += id
        Right(())

    def typeShape(shape: TypeShape, path: String): Either[Failure, Unit] =
      Option(shape)
        .toRight(malformed(path, "type sidecar must be present."))
        .flatMap {
          case TypeShape.Identifier(name) => required(name, path, "type identifier")
          case TypeShape.Select(qualifier, name) =>
            for
              _ <- typeShape(qualifier, s"$path qualifier")
              _ <- required(name, path, "selected type name")
            yield ()
          case TypeShape.Apply(constructor, arguments) =>
            for
              _ <- typeShape(constructor, s"$path constructor")
              _ <- termElements(arguments, s"$path argument")(typeShape)
            yield ()
          case TypeShape.Tuple(elements) =>
            termElements(elements, s"$path tuple element")(typeShape)
          case TypeShape.Function(arguments, result) =>
            for
              _ <- termElements(arguments, s"$path function argument")(typeShape)
              _ <- typeShape(result, s"$path function result")
            yield ()
          case TypeShape.Parenthesized(inner) =>
            typeShape(inner, s"$path parenthesized type")
          case TypeShape.Unsupported(nodeKind, detail) =>
            for
              _ <- required(nodeKind, path, "unsupported type node kind")
              _ <- required(detail, path, "unsupported type detail", allowEmpty = true)
            yield ()
        }

    def loop(
        current: TermShape,
        path: String,
        scope: Set[BinderId]
    ): Either[Failure, Unit] =
      Option(current)
        .toRight(malformed(path, "Term child must be present."))
        .flatMap {
          case TermShape.BoundReference(id, displayName) =>
            for
              _ <- Option(id).toRight(malformed(path, "bound-reference identity must be present."))
              _ <- required(displayName, path, "bound-reference display name")
              _ <- Either.cond(
                scope(id),
                (),
                malformed(path, "bound reference has no live declaration in this semantic graph.")
              )
            yield ()
          case TermShape.Lambda1(id, displayName, parameterType, body) =>
            for
              _ <- declare(id, path)
              _ <- required(displayName, path, "lambda parameter name")
              _ <- required(parameterType, path, "lambda parameter type")
              _ <- loop(body, s"$path lambda body", scope + id)
            yield ()
          case TermShape.Identifier(name, _) => required(name, path, "identifier name")
          case TermShape.Literal(value) => required(value, path, "literal value", allowEmpty = true)
          case TermShape.Select(qualifier, name) =>
            for
              _ <- loop(qualifier, s"$path qualifier", scope)
              _ <- required(name, path, "selected name")
            yield ()
          case TermShape.Apply(function, arguments) =>
            for
              _ <- loop(function, s"$path function", scope)
              _ <- termElements(arguments, s"$path argument")((child, childPath) => loop(child, childPath, scope))
            yield ()
          case TermShape.New(constructor, arguments) =>
            for
              _ <- required(constructor, path, "constructor name")
              _ <- termElements(arguments, s"$path constructor argument")((child, childPath) => loop(child, childPath, scope))
            yield ()
          case TermShape.Infix(left, operator, right) =>
            for
              _ <- loop(left, s"$path left operand", scope)
              _ <- required(operator, path, "infix operator")
              _ <- loop(right, s"$path right operand", scope)
            yield ()
          case TermShape.Unary(operator, operand) =>
            for
              _ <- required(operator, path, "unary operator")
              _ <- loop(operand, s"$path unary operand", scope)
            yield ()
          case TermShape.InterpolatedString(prefix, parts, arguments) =>
            for
              _ <- required(prefix, path, "interpolation prefix")
              presentParts <- Option(parts)
                .toRight(malformed(path, "interpolation parts must be present."))
              presentArguments <- Option(arguments)
                .toRight(malformed(path, "interpolation arguments must be present."))
              _ <- validateStrings(presentParts, s"$path interpolation part")
              _ <- termElements(presentArguments, s"$path interpolation argument")((child, childPath) => loop(child, childPath, scope))
            yield ()
          case TermShape.Typed(expression, typeName) =>
            for
              _ <- loop(expression, s"$path typed expression", scope)
              _ <- required(typeName, path, "ascribed type")
            yield ()
          case TermShape.Tuple(elements) =>
            termElements(elements, s"$path tuple element")((child, childPath) => loop(child, childPath, scope))
          case TermShape.If(condition, thenBranch, elseBranch) =>
            for
              _ <- loop(condition, s"$path condition", scope)
              _ <- loop(thenBranch, s"$path then branch", scope)
              _ <- loop(elseBranch, s"$path else branch", scope)
            yield ()
          case TermShape.Block(statements, result) =>
            for
              present <- Option(statements)
                .toRight(malformed(path, "block statements must be present."))
              _ <- Either.cond(
                present.nonEmpty,
                (),
                malformed(path, "block statements must be nonempty.")
              )
              resultScope <- present.zipWithIndex.foldLeft[
                Either[Failure, Set[BinderId]]
              ](Right(scope)) { case (accumulated, (statement, index)) =>
                accumulated.flatMap { currentScope =>
                  Option(statement)
                    .toRight(malformed(s"$path statement $index", "block statement must be present."))
                    .flatMap {
                      case term: TermShape =>
                        loop(term, s"$path statement $index", currentScope).map(_ => currentScope)
                      case local: BlockStatement.LocalVal =>
                        for
                          _ <- declare(local.binderId, s"$path local value $index")
                          _ <- required(local.displayName, s"$path local value $index", "name")
                          _ <- required(local.declaredType, s"$path local value $index", "declared type")
                          _ <- loop(local.initializer, s"$path local value $index initializer", currentScope)
                        yield currentScope + local.binderId
                      case local: BlockStatement.LocalDef =>
                        for
                          _ <- declare(local.methodBinderId, s"$path local method $index")
                          _ <- declare(local.parameterBinderId, s"$path local method $index parameter")
                          _ <- required(local.methodDisplayName, s"$path local method $index", "method name")
                          _ <- required(local.parameterDisplayName, s"$path local method $index", "parameter name")
                          _ <- typeShape(local.parameterType, s"$path local method $index parameter type")
                          _ <- typeShape(local.resultType, s"$path local method $index result type")
                          _ <- loop(local.body, s"$path local method $index body", currentScope + local.parameterBinderId)
                        yield currentScope + local.methodBinderId
                    }
                }
              }
              _ <- loop(result, s"$path result", resultScope)
            yield ()
          case TermShape.Parenthesized(expression) =>
            loop(expression, s"$path parenthesized expression", scope)
          case TermShape.Unsupported(nodeKind, detail) =>
            for
              _ <- required(nodeKind, path, "unsupported Term node kind")
              _ <- required(detail, path, "unsupported Term detail", allowEmpty = true)
            yield ()
        }

    loop(term, "term", Set.empty)

  private def validateFacadeLimits(term: TermShape): Either[Failure, Unit] =
    def loop(current: TermShape): Either[Failure, Unit] =
      current match
        case TermShape.Apply(_: TermShape.Apply, _) =>
          Left(unsupported("multiple application lists are outside the current completed exact intersection."))
        case TermShape.Apply(function, arguments) =>
          loop(function).flatMap(_ => validateAll(arguments)(loop))
        case TermShape.Infix(left, operator, right) if !AdmittedInfixOperators(operator) =>
          Left(unsupported(s"infix operator `$operator` is outside the current exact operator policy."))
        case TermShape.Infix(left, _, right) => loop(left).flatMap(_ => loop(right))
        case TermShape.Lambda1(_, _, _, body) => loop(body)
        case TermShape.Select(qualifier, _) => loop(qualifier)
        case TermShape.New(_, arguments) => validateAll(arguments)(loop)
        case TermShape.Unary(_, operand) => loop(operand)
        case TermShape.InterpolatedString(_, _, arguments) => validateAll(arguments)(loop)
        case TermShape.Typed(expression, _) => loop(expression)
        case TermShape.Tuple(elements) => validateAll(elements)(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition).flatMap(_ => loop(thenBranch)).flatMap(_ => loop(elseBranch))
        case TermShape.Block(statements, result) =>
          statements.foldLeft[Either[Failure, Unit]](Right(())) { (checked, statement) =>
            checked.flatMap { _ =>
              statement match
                case term: TermShape => loop(term)
                case local: BlockStatement.LocalVal => loop(local.initializer)
                case local: BlockStatement.LocalDef => loop(local.body)
            }
          }.flatMap(_ => loop(result))
        case TermShape.Parenthesized(expression) => loop(expression)
        case _ => Right(())

    loop(term)

  private def validateRaw(
      constructed: ConstructedTerm,
      raw: untpd.Tree,
      path: String
  )(using Context): Either[Failure, Unit] =
    validateTerm(
      constructed.root,
      raw,
      (constructed.ascriptionTypes, 0, Map.empty),
      path
    ).flatMap { state =>
      Either.cond(
        state.typedOrdinal == state.sidecars.size,
        (),
        invariant(
          path,
          s"raw topology consumed ${state.typedOrdinal} of ${state.sidecars.size} completed type sidecars."
        )
      )
    }

  private def validateTerm(
      semantic: TermShape,
      raw: untpd.Tree,
      state: TopologyState,
      path: String
  )(using Context): Either[Failure, TopologyState] =
    (semantic, raw) match
      case (TermShape.BoundReference(id, _), untpd.Ident(actual)) =>
        state.binders.get(id) match
          case Some(expected) if actual.toString == expected => Right(state)
          case Some(expected) => Left(topology(path, s"bound reference expected `$expected` but found `$actual`."))
          case None => Left(topology(path, "bound reference had no active raw declaration."))
      case (TermShape.Identifier(expected, _), untpd.Ident(actual))
          if actual.toString == expected => Right(state)
      case (TermShape.Literal(expected), actual) if literalMatches(expected, actual) =>
        Right(state)
      case (TermShape.Select(expectedQualifier, expectedName), untpd.Select(actualQualifier, actualName))
          if actualName.toString == expectedName =>
        validateTerm(expectedQualifier, actualQualifier, state, s"$path qualifier")
      case (TermShape.Apply(expectedFunction, expectedArguments), untpd.Apply(actualFunction, actualArguments)) =>
        for
          afterFunction <- validateTerm(expectedFunction, actualFunction, state, s"$path function")
          afterArguments <- validateTermPairs(expectedArguments, actualArguments, afterFunction, s"$path argument")
        yield afterArguments
      case (TermShape.New(expectedConstructor, expectedArguments), untpd.Apply(untpd.Select(fresh: untpd.New, constructor), actualArguments))
          if constructor.toString == "<init>" && rawTypePath(fresh.tpt).contains(expectedConstructor) =>
        validateTermPairs(expectedArguments, actualArguments, state, s"$path constructor argument")
      case (TermShape.Infix(expectedLeft, expectedOperator, expectedRight), untpd.InfixOp(actualLeft, untpd.Ident(actualOperator), actualRight))
          if actualOperator.toString == expectedOperator =>
        for
          afterLeft <- validateTerm(expectedLeft, actualLeft, state, s"$path left operand")
          afterRight <- validateTerm(expectedRight, actualRight, afterLeft, s"$path right operand")
        yield afterRight
      case (TermShape.Unary(expectedOperator, expectedOperand), untpd.PrefixOp(untpd.Ident(actualOperator), actualOperand))
          if actualOperator.toString == expectedOperator =>
        validateTerm(expectedOperand, actualOperand, state, s"$path unary operand")
      case (TermShape.InterpolatedString(expectedPrefix, parts, arguments), untpd.InterpolatedString(actualPrefix, segments))
          if actualPrefix.toString == expectedPrefix =>
        validateInterpolationTopology(parts, arguments, segments, state, path)
      case (TermShape.Typed(expectedExpression, _), untpd.Typed(actualExpression, actualType)) =>
        for
          consumed <- state.consume(path)
          (expectedType, afterType) = consumed
          _ <- validateCompletedType(expectedType, actualType, s"$path ascribed type")
          afterExpression <- validateTerm(expectedExpression, actualExpression, afterType, s"$path typed expression")
        yield afterExpression
      case (TermShape.Tuple(expectedElements), untpd.Tuple(actualElements)) =>
        validateTermPairs(expectedElements, actualElements, state, s"$path tuple element")
      case (TermShape.If(expectedCondition, expectedThen, expectedElse), untpd.If(actualCondition, actualThen, actualElse)) =>
        for
          afterCondition <- validateTerm(expectedCondition, actualCondition, state, s"$path condition")
          afterThen <- validateTerm(expectedThen, actualThen, afterCondition, s"$path then branch")
          afterElse <- validateTerm(expectedElse, actualElse, afterThen, s"$path else branch")
        yield afterElse
      case (TermShape.Block(expectedStatements, expectedResult), untpd.Block(actualStatements, actualResult)) =>
        val incomingBinders = state.binders
        for
          afterStatements <- validateStatementPairs(expectedStatements, actualStatements, state, s"$path statement")
          afterResult <- validateTerm(expectedResult, actualResult, afterStatements, s"$path result")
        yield afterResult.withBinders(incomingBinders)
      case (TermShape.Parenthesized(expectedExpression), untpd.Parens(actualExpression)) =>
        validateTerm(expectedExpression, actualExpression, state, s"$path parenthesized expression")
      case (TermShape.Lambda1(id, expectedName, _, expectedBody), untpd.Function(List(parameter: untpd.ValDef), actualBody))
          if parameter.name.toString == expectedName &&
            parameter.mods.flags == Flags.Param && parameter.rhs.isEmpty =>
        for
          consumed <- state.consume(path)
          (expectedType, afterType) = consumed
          _ <- validateCompletedType(expectedType, parameter.tpt, s"$path parameter type")
          bodyState = afterType.withBinders(afterType.binders.updated(id, expectedName))
          afterBody <- expectedBody match
            case typed: TermShape.Typed =>
              actualBody match
                case untpd.Parens(inner) => validateTerm(typed, inner, bodyState, s"$path body")
                case _ => Left(topology(s"$path body", "typed Lambda1 body lacked parser-equivalent Parens."))
            case _ => validateTerm(expectedBody, actualBody, bodyState, s"$path body")
        yield afterBody.withBinders(state.binders)
      case _ =>
        Left(
          topology(
            path,
            s"raw topology ${Option(raw).fold("null")(_.getClass.getSimpleName)} did not match ${semantic.render}."
          )
        )

  private def validateStatementPairs(
      semantic: List[BlockStatement],
      raw: List[untpd.Tree],
      state: TopologyState,
      path: String
  )(using Context): Either[Failure, TopologyState] =
    if raw == null || raw.size != semantic.size then
      Left(topology(path, "raw block statement count did not match the semantic block."))
    else
      semantic.zip(raw).zipWithIndex.foldLeft[Either[Failure, TopologyState]](Right(state)) {
        case (checked, ((expected, actual), index)) =>
          checked.flatMap { current =>
            expected match
              case term: TermShape => validateTerm(term, actual, current, s"$path $index")
              case local: BlockStatement.LocalVal =>
                actual match
                  case definition: untpd.ValDef
                      if definition.name.toString == local.displayName &&
                        definition.mods.flags == Flags.EmptyFlags =>
                    for
                      consumed <- current.consume(s"$path $index")
                      (expectedType, afterType) = consumed
                      _ <- validateCompletedType(expectedType, definition.tpt, s"$path $index declared type")
                      afterInitializer <- validateTerm(local.initializer, definition.rhs, afterType, s"$path $index initializer")
                    yield afterInitializer.withBinders(
                      afterInitializer.binders.updated(local.binderId, local.displayName)
                    )
                  case _ => Left(topology(s"$path $index", "raw local value topology did not match."))
              case local: BlockStatement.LocalDef =>
                validateLocalDef(local, actual, current, s"$path $index")
          }
      }

  private def validateLocalDef(
      local: BlockStatement.LocalDef,
      raw: untpd.Tree,
      state: TopologyState,
      path: String
  )(using Context): Either[Failure, TopologyState] =
    val unavailable = LocalDefBinderSpelling.unavailableKeys(state.binders.values)
    val expectedMethodName = LocalDefBinderSpelling.freshen(local.methodDisplayName, unavailable)
    val expectedParameterName = LocalDefBinderSpelling.freshen(local.parameterDisplayName, unavailable)
    raw match
      case definition: untpd.DefDef
          if definition.name.toString == expectedMethodName &&
            definition.mods.flags == Flags.Method &&
            definition.paramss.size == 1 && definition.paramss.head.size == 1 =>
        definition.paramss.head.head match
          case parameter: untpd.ValDef
              if parameter.name.toString == expectedParameterName &&
                parameter.mods.flags == Flags.Param && parameter.rhs.isEmpty =>
            for
              consumedParameter <- state.consume(s"$path parameter")
              (expectedParameterType, afterParameterType) = consumedParameter
              _ <- validateCompletedType(expectedParameterType, parameter.tpt, s"$path parameter type")
              consumedResult <- afterParameterType.consume(s"$path result")
              (expectedResultType, afterResultType) = consumedResult
              _ <- validateCompletedType(expectedResultType, definition.tpt, s"$path result type")
              bodyState = afterResultType.withBinders(
                state.binders
                  .removed(local.methodBinderId)
                  .updated(local.parameterBinderId, expectedParameterName)
              )
              afterBody <- validateTerm(local.body, definition.rhs, bodyState, s"$path body")
            yield afterBody.withBinders(
              state.binders.updated(local.methodBinderId, expectedMethodName)
            )
          case _ => Left(topology(path, "raw local method parameter topology did not match."))
      case _ => Left(topology(path, "raw local method topology did not match."))

  private def validateInterpolationTopology(
      parts: List[String],
      arguments: List[TermShape],
      segments: List[untpd.Tree],
      state: TopologyState,
      path: String
  )(using Context): Either[Failure, TopologyState] =
    if segments == null || segments.size != parts.size then
      Left(topology(path, "raw interpolation segment count did not match."))
    else
      parts.init.zip(arguments).zip(segments.init).zipWithIndex
        .foldLeft[Either[Failure, TopologyState]](Right(state)) {
          case (checked, ((((part, argument), segment), index))) =>
            checked.flatMap { current =>
              segment match
                case untpd.Thicket(List(untpd.Literal(value), rawArgument))
                    if value == Constant(StandardSInterpolationEncoding.encodePart(part).rawLiteralValue) =>
                  if StandardSInterpolationEncoding.isDirectArgument(argument) then
                    validateTerm(argument, rawArgument, current, s"$path interpolation argument $index")
                  else
                    rawArgument match
                      case untpd.Block(Nil, inner) =>
                        validateTerm(argument, inner, current, s"$path interpolation argument $index")
                      case _ => Left(topology(path, s"interpolation argument $index lacked its exact wrapper."))
                case _ => Left(topology(path, s"interpolation segment $index did not match."))
            }
        }
        .flatMap { next =>
          segments.last match
            case untpd.Literal(value)
                if value == Constant(StandardSInterpolationEncoding.encodePart(parts.last).rawLiteralValue) =>
              Right(next)
            case _ => Left(topology(path, "final interpolation literal did not match."))
        }

  private def validateTermPairs(
      semantic: List[TermShape],
      raw: List[untpd.Tree],
      state: TopologyState,
      path: String
  )(using Context): Either[Failure, TopologyState] =
    if raw == null || raw.size != semantic.size then
      Left(topology(path, "raw child count did not match the semantic value."))
    else
      semantic.zip(raw).zipWithIndex.foldLeft[Either[Failure, TopologyState]](Right(state)) {
        case (checked, ((expected, actual), index)) =>
          checked.flatMap(validateTerm(expected, actual, _, s"$path $index"))
      }

  private def validateCompletedType(
      semantic: TypeNormalForm,
      raw: untpd.Tree,
      path: String
  )(using Context): Either[Failure, Unit] =
    (semantic, raw) match
      case (TypeNormalForm.STypeIdent(expected), untpd.Ident(actual))
          if actual.isTypeName && actual.toString == expected => Right(())
      case (TypeNormalForm.STypeApply(expectedConstructor, expectedArguments), untpd.AppliedTypeTree(actualConstructor, actualArguments))
          if actualArguments != null && actualArguments.size == expectedArguments.size =>
        for
          _ <- validateCompletedType(expectedConstructor, actualConstructor, s"$path constructor")
          _ <- validateTypePairs(expectedArguments, actualArguments, s"$path argument")
        yield ()
      case (TypeNormalForm.STypeTuple(expectedElements), untpd.Tuple(actualElements))
          if actualElements != null && actualElements.size == expectedElements.size =>
        validateTypePairs(expectedElements, actualElements, s"$path tuple element")
      case (TypeNormalForm.STypeFunction(expectedArguments, expectedResult), untpd.Function(actualArguments, actualResult))
          if actualArguments != null && actualArguments.size == expectedArguments.size =>
        for
          _ <- validateTypePairs(expectedArguments, actualArguments, s"$path function argument")
          _ <- validateCompletedType(expectedResult, actualResult, s"$path function result")
        yield ()
      case _ => Left(topology(path, s"raw Type topology did not match ${semantic.render}."))

  private def validateTypePairs(
      semantic: List[TypeNormalForm],
      raw: List[untpd.Tree],
      path: String
  )(using Context): Either[Failure, Unit] =
    semantic.zip(raw).zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) {
      case (checked, ((expected, actual), index)) =>
        checked.flatMap(_ => validateCompletedType(expected, actual, s"$path $index"))
    }

  private def literalMatches(expected: String, raw: untpd.Tree): Boolean =
    raw match
      case untpd.Number(actual, untpd.NumberKind.Whole(10)) => actual == expected
      case untpd.Literal(actual) if expected == "true" => actual == Constant(true)
      case untpd.Literal(actual) if expected == "false" => actual == Constant(false)
      case untpd.Literal(actual)
          if expected.length >= 2 && expected.head == '"' && expected.last == '"' =>
        actual == Constant(expected.substring(1, expected.length - 1))
      case _ => false

  private def rawTypePath(raw: untpd.Tree): Option[String] =
    raw match
      case untpd.Ident(name) => Some(name.toString)
      case untpd.Select(qualifier, name) => rawTypePath(qualifier).map(_ + "." + name.toString)
      case _ => None

  private def termElements[A](
      values: List[A],
      path: String
  )(
      validate: (A, String) => Either[Failure, Unit]
  ): Either[Failure, Unit] =
    Option(values)
      .toRight(malformed(path, "values must be present."))
      .flatMap { present =>
        present.zipWithIndex.foldLeft[Either[Failure, Unit]](Right(())) {
          case (checked, (value, index)) =>
            checked.flatMap(_ =>
              Option(value)
                .toRight(malformed(s"$path $index", "value must be present."))
                .flatMap(validate(_, s"$path $index"))
            )
        }
      }

  private def validateStrings(values: List[String], path: String): Either[Failure, Unit] =
    termElements(values, path)((_, _) => Right(()))

  private def validateAll[A](
      values: List[A]
  )(
      validate: A => Either[Failure, Unit]
  ): Either[Failure, Unit] =
    values.foldLeft[Either[Failure, Unit]](Right(())) { (checked, value) =>
      checked.flatMap(_ => validate(value))
    }

  private def classifyCompletionFailure(problem: TermConstructionError): Failure =
    problem match
      case _: TermConstructionError.UnsupportedTermShape |
          _: TermConstructionError.UnsupportedUnaryOperator |
          _: TermConstructionError.InvalidTupleArity |
          _: TermConstructionError.InvalidTypeTemplateSidecar =>
        Failure("UNSUPPORTED_SEMANTIC_VALUE", problem.message)
      case _: TermConstructionError.TypedSidecarCountMismatch |
          _: TermConstructionError.TypedSidecarRenderingMismatch |
          _: TermConstructionError.CompletionInvariantFailure =>
        invariant("term completion", problem.message)
      case _ => Failure("UNSUPPORTED_SEMANTIC_VALUE", problem.message)

  private def classifyBackendFailure(
      problem: ConstructedTermUntypedBackendError
  ): Failure =
    problem match
      case _: ConstructedTermUntypedBackendError.UnsupportedLiteral =>
        Failure("EXACT_LOWERING_FAILED", problem.message)
      case _: ConstructedTermUntypedBackendError.UnsupportedTermNode |
          ConstructedTermUntypedBackendError.NestedLambda1Unsupported |
          _: ConstructedTermUntypedBackendError.InvalidConstructorName |
          _: ConstructedTermUntypedBackendError.UnsupportedUnaryOperator |
          _: ConstructedTermUntypedBackendError.UnsupportedInterpolationPrefix |
          _: ConstructedTermUntypedBackendError.UnsupportedTypeSidecar =>
        Failure("UNSUPPORTED_SEMANTIC_VALUE", problem.message)
      case ConstructedTermUntypedBackendError.MissingTermShape |
          _: ConstructedTermUntypedBackendError.OutOfScopeBoundReference |
          _: ConstructedTermUntypedBackendError.MalformedBinderScope |
          _: ConstructedTermUntypedBackendError.MalformedConstructorArguments |
          _: ConstructedTermUntypedBackendError.NullConstructorArgument |
          _: ConstructedTermUntypedBackendError.MalformedBlock |
          _: ConstructedTermUntypedBackendError.MalformedLocalDef |
          _: ConstructedTermUntypedBackendError.MalformedInterpolation |
          _: ConstructedTermUntypedBackendError.NullInterpolationPart |
          _: ConstructedTermUntypedBackendError.NullInterpolationArgument =>
        Failure("MALFORMED_SEMANTIC_VALUE", problem.message)
      case ConstructedTermUntypedBackendError.MissingConstructedTerm |
          _: ConstructedTermUntypedBackendError.MissingTypeSidecar |
          _: ConstructedTermUntypedBackendError.UnconsumedTypeSidecars =>
        invariant("term backend", problem.message)

  private def malformed(path: String, detail: String): Failure =
    Failure("MALFORMED_SEMANTIC_VALUE", s"$path: $detail")

  private def unsupported(detail: String): Failure =
    Failure("UNSUPPORTED_SEMANTIC_VALUE", detail)

  private def invariant(path: String, detail: String): Failure =
    Failure("INTERNAL_INVARIANT_FAILED", s"$path: $detail")

  private def topology(path: String, detail: String): Failure =
    invariant(path, detail)
