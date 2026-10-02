package quasiquotes.matching

import scala.compiletime.erasedValue
import scala.quoted.*

import quasiquotes.parser.BinderId

/** Frontend-owned bridge for selectors which already compiled their ranked
  * source to the shared pattern IR.
  */
private[quasiquotes] object RankedTermPatternBridge:
  final case class Layout(
      effectiveSource: String,
      holeNames: Vector[String],
      sequenceIndex: Option[Int]
  ):
    def sequenceHoleName: Option[String] = sequenceIndex.map(holeNames)

  def classify(parts: List[String]): Either[String, Layout] =
    RankedPatternSource.classify(parts).map(template =>
      Layout(template.source, template.holeNames, template.sequenceIndex)
    )

  def validateSequencePosition(
      pattern: TermPattern,
      sequenceHoleName: String
  ): Either[String, Unit] =
    RankedPatternSource.validateSequencePosition(pattern, sequenceHoleName)

  def containsDirectNewSequenceHole(
      pattern: TermPattern,
      sequenceName: String
  ): Boolean =
    pattern match
      case TermPattern.Lambda1(_, _, _, body) => containsDirectNewSequenceHole(body, sequenceName)
      case TermPattern.Select(qualifier, _) => containsDirectNewSequenceHole(qualifier, sequenceName)
      case TermPattern.Apply(function, arguments) =>
        containsDirectNewSequenceHole(function, sequenceName) ||
          arguments.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.New(_, arguments) =>
        arguments.exists {
          case TermPattern.Hole(name) => name == sequenceName
          case argument => containsDirectNewSequenceHole(argument, sequenceName)
        }
      case TermPattern.Infix(left, _, right) =>
        containsDirectNewSequenceHole(left, sequenceName) ||
          containsDirectNewSequenceHole(right, sequenceName)
      case TermPattern.Unary(_, operand) => containsDirectNewSequenceHole(operand, sequenceName)
      case TermPattern.InterpolatedString(_, _, arguments) =>
        arguments.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.Typed(expression, _) =>
        containsDirectNewSequenceHole(expression, sequenceName)
      case TermPattern.Tuple(elements) =>
        elements.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        containsDirectNewSequenceHole(condition, sequenceName) ||
          containsDirectNewSequenceHole(thenBranch, sequenceName) ||
          containsDirectNewSequenceHole(elseBranch, sequenceName)
      case TermPattern.Block(prefix, result) =>
        prefix.exists {
          case term: TermPattern => containsDirectNewSequenceHole(term, sequenceName)
          case BlockPatternStatement.LocalVal(_, _, _, initializer) =>
            containsDirectNewSequenceHole(initializer, sequenceName)
        } || containsDirectNewSequenceHole(result, sequenceName)
      case TermPattern.Parenthesized(inner) =>
        containsDirectNewSequenceHole(inner, sequenceName)
      case _ => false

  def patternExpr(pattern: TermPattern)(using Quotes): Expr[TermPattern] =
    pattern match
      case TermPattern.Hole(name) => '{ TermPattern.Hole(${ Expr(name) }) }
      case TermPattern.Identifier(name) => '{ TermPattern.Identifier(${ Expr(name) }) }
      case TermPattern.BoundReference(binderId, displayName) =>
        '{ TermPattern.BoundReference(${ binderExpr(binderId) }, ${ Expr(displayName) }) }
      case TermPattern.Lambda1(binderId, displayName, parameterType, body) =>
        '{
          TermPattern.Lambda1(
            ${ binderExpr(binderId) },
            ${ Expr(displayName) },
            ${ Expr(parameterType) },
            ${ patternExpr(body) }
          )
        }
      case TermPattern.Literal(value) => '{ TermPattern.Literal(${ Expr(value) }) }
      case TermPattern.Select(qualifier, name) =>
        '{ TermPattern.Select(${ patternExpr(qualifier) }, ${ Expr(name) }) }
      case TermPattern.Apply(function, arguments) =>
        '{ TermPattern.Apply(${ patternExpr(function) }, ${ patternListExpr(arguments) }) }
      case TermPattern.New(constructor, arguments) =>
        '{ TermPattern.New(${ Expr(constructor) }, ${ patternListExpr(arguments) }) }
      case TermPattern.Infix(left, operator, right) =>
        '{
          TermPattern.Infix(
            ${ patternExpr(left) },
            ${ Expr(operator) },
            ${ patternExpr(right) }
          )
        }
      case TermPattern.Unary(operator, operand) =>
        '{ TermPattern.Unary(${ Expr(operator) }, ${ patternExpr(operand) }) }
      case TermPattern.InterpolatedString(prefix, parts, arguments) =>
        '{
          TermPattern.InterpolatedString(
            ${ Expr(prefix) },
            ${ Expr.ofList(parts.map(Expr(_))) },
            ${ patternListExpr(arguments) }
          )
        }
      case TermPattern.Typed(expression, typeName) =>
        '{ TermPattern.Typed(${ patternExpr(expression) }, ${ Expr(typeName) }) }
      case TermPattern.Tuple(elements) =>
        '{ TermPattern.Tuple(${ patternListExpr(elements) }) }
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        '{
          TermPattern.If(
            ${ patternExpr(condition) },
            ${ patternExpr(thenBranch) },
            ${ patternExpr(elseBranch) }
          )
        }
      case TermPattern.Block(statements, result) =>
        '{ TermPattern.Block(${ statementListExpr(statements) }, ${ patternExpr(result) }) }
      case TermPattern.Parenthesized(expression) =>
        '{ TermPattern.Parenthesized(${ patternExpr(expression) }) }

  def holeNamesExpr(holeNames: Vector[String])(using Quotes): Expr[Vector[String]] =
    '{ Vector.from(${ Expr.ofList(holeNames.toList.map(Expr(_))) }) }

  transparent inline def singleSequenceExtractor(
      pattern: TermPattern,
      holeNames: Vector[String],
      sequenceHoleName: String
  )(using q: Quotes): RankedSingleSequenceTermPatternExtractor[q.reflect.Term] =
    val ranked = extractor[SequenceTermCapture *: EmptyTuple](
      pattern,
      holeNames,
      sequenceHoleName
    )
    new RankedSingleSequenceTermPatternExtractor(term => ranked.unapply(term).map(_.head))

  transparent inline def extractor[Kinds <: Tuple](
      pattern: TermPattern,
      holeNames: Vector[String],
      sequenceHoleName: String
  )(using q: Quotes): RankedTermPatternExtractor[
    q.reflect.Term,
    RankedCaptureTypes[q.reflect.Term, Kinds]
  ] =
    new RankedTermPatternExtractor(term =>
      TermMatcher
        .matchTermRanked(pattern, sequenceHoleName, term)
        .toOption
        .map(result =>
          captureTuple[q.reflect.Term, Kinds](
            RankedTermMatch(result.scalarBindings, result.sequenceBindings, holeNames),
            0
          )
        )
    )

  private final case class RankedTermMatch[T](
      scalarBindings: Map[String, T],
      sequenceBindings: Map[String, Seq[T]],
      holeNames: Vector[String]
  ):
    def scalar(index: Int): T = scalarBindings(holeNames(index))
    def sequence(index: Int): Seq[T] = sequenceBindings(holeNames(index))

  private inline def captureTuple[T, Kinds <: Tuple](
      result: RankedTermMatch[T],
      index: Int
  ): RankedCaptureTypes[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ScalarTermCapture *: tail) =>
        result.scalar(index) *: captureTuple[T, tail](result, index + 1)
      case _: (SequenceTermCapture *: tail) =>
        result.sequence(index) *: captureTuple[T, tail](result, index + 1)

  private def patternListExpr(patterns: List[TermPattern])(using Quotes): Expr[List[TermPattern]] =
    Expr.ofList(patterns.map(patternExpr))

  private def statementListExpr(
      statements: List[BlockPatternStatement]
  )(using Quotes): Expr[List[BlockPatternStatement]] =
    Expr.ofList(statements.map(statementExpr))

  private def statementExpr(
      statement: BlockPatternStatement
  )(using Quotes): Expr[BlockPatternStatement] =
    statement match
      case pattern: TermPattern => patternExpr(pattern)
      case BlockPatternStatement.LocalVal(binderId, displayName, declaredType, initializer) =>
        '{
          BlockPatternStatement.LocalVal(
            ${ binderExpr(binderId) },
            ${ Expr(displayName) },
            ${ Expr(declaredType) },
            ${ patternExpr(initializer) }
          )
        }

  private def binderExpr(binderId: BinderId)(using Quotes): Expr[BinderId] =
    '{ BinderId(${ Expr(binderId.value) }) }
