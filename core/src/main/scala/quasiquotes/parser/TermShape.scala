package quasiquotes.parser

sealed trait TermShape derives CanEqual:
  final def render: String = TermShape.render(this)

object TermShape:
  final case class Identifier(name: String, isPlaceholder: Boolean) extends TermShape
  final case class Literal(value: String) extends TermShape
  final case class Select(qualifier: TermShape, name: String) extends TermShape
  final case class Apply(function: TermShape, arguments: List[TermShape]) extends TermShape
  final case class New(constructor: String, arguments: List[TermShape]) extends TermShape
  final case class Infix(left: TermShape, operator: String, right: TermShape) extends TermShape
  final case class Unary(operator: String, operand: TermShape) extends TermShape
  final case class InterpolatedString(
      prefix: String,
      parts: List[String],
      arguments: List[TermShape]
  ) extends TermShape:
    require(parts.size == arguments.size + 1, "Interpolated string parts/arguments invariant")
  final case class Typed(expression: TermShape, typeName: String) extends TermShape
  final case class Tuple(elements: List[TermShape]) extends TermShape
  final case class If(condition: TermShape, thenBranch: TermShape, elseBranch: TermShape) extends TermShape
  final case class Parenthesized(expression: TermShape) extends TermShape
  final case class Unsupported(nodeKind: String, detail: String) extends TermShape

  def render(shape: TermShape): String =
    shape match
      case Identifier(name, true) => s"Placeholder($name)"
      case Identifier(name, false) => s"Ident($name)"
      case Literal(value) => s"Literal($value)"
      case Select(qualifier, name) => s"Select(${render(qualifier)}, $name)"
      case Apply(function, arguments) =>
        s"Apply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case New(constructor, arguments) =>
        s"New($constructor, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right) =>
        s"Infix(${render(left)}, $operator, ${render(right)})"
      case Unary(operator, operand) =>
        s"Unary($operator, ${render(operand)})"
      case InterpolatedString(prefix, parts, arguments) =>
        s"InterpolatedString($prefix, [${parts.map(quote).mkString(", ")}], [${arguments.map(render).mkString(", ")}])"
      case Typed(expression, typeName) => s"Typed(${render(expression)}, Type($typeName))"
      case Tuple(elements) => s"Tuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch) =>
        s"If(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"
      case Parenthesized(expression) => s"Parens(${render(expression)})"
      case Unsupported(nodeKind, detail) => s"Unsupported($nodeKind, $detail)"

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
