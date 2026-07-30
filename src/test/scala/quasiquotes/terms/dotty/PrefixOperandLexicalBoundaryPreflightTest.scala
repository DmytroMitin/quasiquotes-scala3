package quasiquotes.terms.dotty

import quasiquotes.parser.{TermShape, TermShapeInspector, TinyTermParser}

class PrefixOperandLexicalBoundaryPreflightTest extends munit.FunSuite:
  private val operators = Vector("-", "+", "!", "~")

  operators.foreach { operator =>
    test(s"adjacent symbolic prefix text is not a truthful stable serialization: $operator-1") {
      val source = s"$operator-1"
      val intended =
        TermShape.Unary(operator, TermShape.Literal("-1"))
      val observed =
        TinyTermParser.parse(source).map(parsed =>
          eraseGrouping(TermShapeInspector.inspect(parsed.rawTree))
        )

      println(
        s"PREFIX_LEXICAL_PREFLIGHT unsafe=$source observed=${renderObserved(observed)}"
      )
      assert(
        observed.fold(_ => true, _ != intended),
        clues(source, intended, observed)
      )
    }

    test(s"parenthesized prefix operand preserves the intended structure: $operator(-1)") {
      val source = s"$operator(-1)"
      val intended =
        TermShape.Unary(operator, TermShape.Literal("-1"))
      val observed =
        TinyTermParser.parse(source).map(parsed =>
          eraseGrouping(TermShapeInspector.inspect(parsed.rawTree))
        )

      println(
        s"PREFIX_LEXICAL_PREFLIGHT corrected=$source observed=${renderObserved(observed)}"
      )
      assertEquals(observed, Right(intended))
    }
  }

  private def eraseGrouping(shape: TermShape): TermShape =
    shape match
      case TermShape.Select(qualifier, name) =>
        TermShape.Select(eraseGrouping(qualifier), name)
      case TermShape.Apply(function, arguments) =>
        TermShape.Apply(
          eraseGrouping(function),
          arguments.map(eraseGrouping)
        )
      case TermShape.Infix(left, operator, right) =>
        TermShape.Infix(
          eraseGrouping(left),
          operator,
          eraseGrouping(right)
        )
      case TermShape.Unary(operator, operand) =>
        TermShape.Unary(operator, eraseGrouping(operand))
      case TermShape.Tuple(elements) =>
        TermShape.Tuple(elements.map(eraseGrouping))
      case TermShape.If(condition, thenBranch, elseBranch) =>
        TermShape.If(
          eraseGrouping(condition),
          eraseGrouping(thenBranch),
          eraseGrouping(elseBranch)
        )
      case TermShape.Parenthesized(expression) =>
        eraseGrouping(expression)
      case other =>
        other

  private def renderObserved(
      observed: Either[Throwable, TermShape]
  ): String =
    observed.fold(
      error => s"parse-error:${error.getMessage}",
      _.render
    )
