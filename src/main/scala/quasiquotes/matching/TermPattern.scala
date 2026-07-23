package quasiquotes.matching

sealed trait TermPattern derives CanEqual:
  final def render: String = TermPattern.render(this)

object TermPattern:
  final case class Hole(name: String) extends TermPattern
  final case class Identifier(name: String) extends TermPattern
  final case class Literal(value: String) extends TermPattern
  final case class Select(qualifier: TermPattern, name: String) extends TermPattern
  final case class Apply(function: TermPattern, arguments: List[TermPattern]) extends TermPattern
  final case class Infix(left: TermPattern, operator: String, right: TermPattern) extends TermPattern
  final case class Unary(operator: String, operand: TermPattern) extends TermPattern
  final case class Typed(expression: TermPattern, typeName: String) extends TermPattern
  final case class Tuple(elements: List[TermPattern]) extends TermPattern
  final case class If(condition: TermPattern, thenBranch: TermPattern, elseBranch: TermPattern) extends TermPattern
  final case class Parenthesized(expression: TermPattern) extends TermPattern

  def render(pattern: TermPattern): String =
    pattern match
      case Hole(name) => s"Hole($$${name})"
      case Identifier(name) => s"Ident($name)"
      case Literal(value) => s"Literal($value)"
      case Select(qualifier, name) => s"Select(${render(qualifier)}, $name)"
      case Apply(function, arguments) =>
        s"Apply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right) =>
        s"Infix(${render(left)}, $operator, ${render(right)})"
      case Unary(operator, operand) =>
        s"Unary($operator, ${render(operand)})"
      case Typed(expression, typeName) => s"Typed(${render(expression)}, Type($typeName))"
      case Tuple(elements) => s"Tuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch) =>
        s"If(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"
      case Parenthesized(expression) => s"Parens(${render(expression)})"
