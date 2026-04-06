package quasiquotes.parser

sealed trait TermShape derives CanEqual:
  final def render: String = TermShape.render(this)

object TermShape:
  final case class Identifier(name: String, isPlaceholder: Boolean) extends TermShape
  final case class Literal(value: String) extends TermShape
  final case class Select(qualifier: TermShape, name: String) extends TermShape
  final case class Apply(function: TermShape, arguments: List[TermShape]) extends TermShape
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
      case Parenthesized(expression) => s"Parens(${render(expression)})"
      case Unsupported(nodeKind, detail) => s"Unsupported($nodeKind, $detail)"
