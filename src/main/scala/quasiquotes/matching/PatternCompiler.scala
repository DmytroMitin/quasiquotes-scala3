package quasiquotes.matching

import dotty.tools.dotc.ast.untpd
import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.source.{GeneratedHoleIndex, SourceSpan}

private[matching] final case class PatternCompileFailure(
    error: PatternError,
    generatedSpan: Option[SourceSpan]
)

object PatternCompiler:
  def compile(tree: untpd.Tree): Either[PatternError, TermPattern] =
    compileLocatedUsing(tree, PatternSource.extractHoleName).left.map(_.error)

  /** Compatibility path for direct low-level callers that lack rewrite metadata. */
  private[matching] def compileLocated(tree: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
    compileLocatedUsing(tree, PatternSource.extractHoleName)

  private[matching] def compileLocated(
      tree: untpd.Tree,
      generatedHoles: GeneratedHoleIndex
  ): Either[PatternCompileFailure, TermPattern] =
    compileLocatedUsing(tree, generatedHoles.semanticNameFor)

  private def compileLocatedUsing(
      tree: untpd.Tree,
      semanticHoleName: String => Option[String]
  ): Either[PatternCompileFailure, TermPattern] =
    tree match
      case untpd.Ident(name) =>
        semanticHoleName(name.toString) match
          case Some(holeName) => Right(TermPattern.Hole(holeName))
          case None => Right(TermPattern.Identifier(name.toString))
      case untpd.Literal(constant) =>
        Right(TermPattern.Literal(renderConstant(constant.value)))
      case untpd.Number(digits, _) =>
        Right(TermPattern.Literal(digits))
      case untpd.Select(qualifier, name) =>
        compileLocatedUsing(qualifier, semanticHoleName).map(TermPattern.Select(_, name.toString))
      case untpd.Apply(function, arguments) =>
        for
          compiledFunction <- compileLocatedUsing(function, semanticHoleName)
          compiledArguments <- sequence(arguments.map(compileLocatedUsing(_, semanticHoleName)))
        yield TermPattern.Apply(compiledFunction, compiledArguments)
      case untpd.InfixOp(left, op, right) =>
        for
          compiledLeft <- compileLocatedUsing(left, semanticHoleName)
          compiledRight <- compileLocatedUsing(right, semanticHoleName)
        yield TermPattern.Infix(compiledLeft, op.name.toString, compiledRight)
      case untpd.Typed(expression, typeTree) =>
        compileLocatedUsing(expression, semanticHoleName).map(TermPattern.Typed(_, renderType(typeTree)))
      case untpd.Tuple(elements) =>
        sequence(elements.map(compileLocatedUsing(_, semanticHoleName))).map(TermPattern.Tuple.apply)
      case untpd.If(condition, thenBranch, elseBranch) =>
        for
          compiledCondition <- compileLocatedUsing(condition, semanticHoleName)
          compiledThenBranch <- compileLocatedUsing(thenBranch, semanticHoleName)
          compiledElseBranch <- compileLocatedUsing(elseBranch, semanticHoleName)
        yield TermPattern.If(compiledCondition, compiledThenBranch, compiledElseBranch)
      case untpd.Parens(inner) =>
        compileLocatedUsing(inner, semanticHoleName).map(TermPattern.Parenthesized.apply)
      case untpd.TypedSplice(inner) =>
        compileLocatedUsing(inner, semanticHoleName)
      case other =>
        Left(
          PatternCompileFailure(
            PatternError.UnsupportedPatternShape(other.getClass.getSimpleName, other.toString),
            DottySourceSpanAdapter.fromTree(other).filter(!_.isEmpty)
          )
        )

  private def sequence[A](values: List[Either[PatternCompileFailure, A]]): Either[PatternCompileFailure, List[A]] =
    values.foldRight(Right(Nil): Either[PatternCompileFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }

  private def renderConstant(value: Any): String =
    value match
      case string: String => "\"" + string + "\""
      case other => String.valueOf(other)

  private def renderType(tree: untpd.Tree): String =
    normalizeTypeName(tree match
      case untpd.Ident(name) => name.toString
      case untpd.Select(qualifier, name) => s"${renderType(qualifier)}.${name.toString}"
      case other => other.toString
    )

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other
