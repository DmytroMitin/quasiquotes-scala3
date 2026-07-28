package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

private[quasiquotes] object ConstructedTermUntypedBackend:
  import ConstructedTermUntypedBackendError.*

  private val SupportedUnaryOperators = Set("+", "-", "!", "~")
  private val DecimalInteger = "-?[0-9]+".r

  private final case class LoweringState(
      sidecars: Vector[TypeNormalForm],
      typedOrdinal: Int
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
    given SourceFile = NoSource

    lowerTerm(
      constructed.root,
      LoweringState(constructed.ascriptionTypes, typedOrdinal = 0)
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
    shape match
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
      case TermShape.Typed(expression, _) =>
        for
          consumed <- state.consume
          (sidecar, afterSidecar) = consumed
          rawType <- lowerType(sidecar, state.typedOrdinal)
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
      case TermShape.Unsupported(nodeKind, _) =>
        Left(UnsupportedTermNode(nodeKind))

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

  private def lowerType(
      normalForm: TypeNormalForm,
      typedOrdinal: Int
  )(using SourceFile): Either[ConstructedTermUntypedBackendError, untpd.Tree] =
    normalForm match
      case TypeNormalForm.STypeIdent(name @ ("Int" | "String" | "Boolean")) =>
        Right(untpd.Ident(typeName(name)))
      case TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent(name @ ("List" | "Option")),
            argument :: Nil
          ) =>
        lowerType(argument, typedOrdinal).map { rawArgument =>
          untpd.AppliedTypeTree(
            untpd.Ident(typeName(name)),
            rawArgument :: Nil
          )
        }
      case TypeNormalForm.STypeTuple(elements)
          if elements.size == 2 || elements.size == 3 =>
        lowerTypes(elements, typedOrdinal).map(untpd.Tuple(_))
      case TypeNormalForm.STypeFunction(arguments, result)
          if arguments.size == 1 || arguments.size == 2 =>
        for
          rawArguments <- lowerTypes(arguments, typedOrdinal)
          rawResult <- lowerType(result, typedOrdinal)
        yield untpd.Function(rawArguments, rawResult)
      case unsupported =>
        Left(UnsupportedTypeSidecar(typedOrdinal, unsupported.render))

  private def lowerTypes(
      normalForms: List[TypeNormalForm],
      typedOrdinal: Int
  )(using SourceFile): Either[
    ConstructedTermUntypedBackendError,
    List[untpd.Tree]
  ] =
    normalForms.foldRight[
      Either[ConstructedTermUntypedBackendError, List[untpd.Tree]]
    ](Right(Nil)) { (normalForm, result) =>
      for
        raw <- lowerType(normalForm, typedOrdinal)
        rest <- result
      yield raw :: rest
    }
