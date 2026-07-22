package quasiquotes.matching

import dotty.tools.dotc.ast.untpd
import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.source.SourceSpan

private[matching] final case class PatternCompileFailure(
    error: PatternError,
    generatedSpan: Option[SourceSpan]
)

object PatternCompiler:
  def compile(tree: untpd.Tree): Either[PatternError, TermPattern] =
    compileLocated(tree).left.map(_.error)

  private[matching] def compileLocated(tree: untpd.Tree): Either[PatternCompileFailure, TermPattern] =
    tree match
      case untpd.Ident(name) =>
        PatternSource.extractHoleName(name.toString) match
          case Some(holeName) => Right(TermPattern.Hole(holeName))
          case None => Right(TermPattern.Identifier(name.toString))
      case untpd.Literal(constant) =>
        Right(TermPattern.Literal(renderConstant(constant.value)))
      case untpd.Number(digits, _) =>
        Right(TermPattern.Literal(digits))
      case untpd.Select(qualifier, name) =>
        compileLocated(qualifier).map(TermPattern.Select(_, name.toString))
      case untpd.Apply(function, arguments) =>
        for
          compiledFunction <- compileLocated(function)
          compiledArguments <- sequence(arguments.map(compileLocated))
        yield TermPattern.Apply(compiledFunction, compiledArguments)
      case untpd.InfixOp(left, op, right) =>
        for
          compiledLeft <- compileLocated(left)
          compiledRight <- compileLocated(right)
        yield TermPattern.Infix(compiledLeft, op.name.toString, compiledRight)
      case untpd.Typed(expression, typeTree) =>
        compileLocated(expression).map(TermPattern.Typed(_, renderType(typeTree)))
      case untpd.Tuple(elements) =>
        sequence(elements.map(compileLocated)).map(TermPattern.Tuple.apply)
      case untpd.If(condition, thenBranch, elseBranch) =>
        for
          compiledCondition <- compileLocated(condition)
          compiledThenBranch <- compileLocated(thenBranch)
          compiledElseBranch <- compileLocated(elseBranch)
        yield TermPattern.If(compiledCondition, compiledThenBranch, compiledElseBranch)
      case untpd.Parens(inner) =>
        compileLocated(inner).map(TermPattern.Parenthesized.apply)
      case untpd.TypedSplice(inner) =>
        compileLocated(inner)
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
