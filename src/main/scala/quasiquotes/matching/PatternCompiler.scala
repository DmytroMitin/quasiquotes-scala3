package quasiquotes.matching

import dotty.tools.dotc.ast.untpd

object PatternCompiler:
  def compile(tree: untpd.Tree): Either[PatternError, TermPattern] =
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
        compile(qualifier).map(TermPattern.Select(_, name.toString))
      case untpd.Apply(function, arguments) =>
        for
          compiledFunction <- compile(function)
          compiledArguments <- sequence(arguments.map(compile))
        yield TermPattern.Apply(compiledFunction, compiledArguments)
      case untpd.InfixOp(left, op, right) =>
        for
          compiledLeft <- compile(left)
          compiledRight <- compile(right)
        yield TermPattern.Infix(compiledLeft, op.name.toString, compiledRight)
      case untpd.Parens(inner) =>
        compile(inner).map(TermPattern.Parenthesized.apply)
      case untpd.TypedSplice(inner) =>
        compile(inner)
      case other =>
        Left(PatternError.UnsupportedPatternShape(other.getClass.getSimpleName, other.toString))

  private def sequence[A](values: List[Either[PatternError, A]]): Either[PatternError, List[A]] =
    values.foldRight(Right(Nil): Either[PatternError, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }

  private def renderConstant(value: Any): String =
    value match
      case string: String => "\"" + string + "\""
      case other => String.valueOf(other)
