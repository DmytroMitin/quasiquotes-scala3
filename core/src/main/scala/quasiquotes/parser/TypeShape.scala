package quasiquotes.parser

sealed trait TypeShape derives CanEqual:
  final def render: String = TypeShape.render(this)

object TypeShape:
  final case class Identifier(name: String) extends TypeShape
  final case class Select(qualifier: TypeShape, name: String) extends TypeShape
  final case class Apply(constructor: TypeShape, arguments: List[TypeShape]) extends TypeShape
  final case class Tuple(elements: List[TypeShape]) extends TypeShape
  final case class Function(arguments: List[TypeShape], result: TypeShape) extends TypeShape
  final case class Parenthesized(typeShape: TypeShape) extends TypeShape
  final case class Unsupported(nodeKind: String, detail: String) extends TypeShape

  def render(shape: TypeShape): String =
    shape match
      case Identifier(name) => s"TypeIdent($name)"
      case Select(qualifier, name) => s"TypeSelect(${render(qualifier)}, $name)"
      case Apply(constructor, arguments) =>
        s"TypeApply(${render(constructor)}, [${arguments.map(render).mkString(", ")}])"
      case Tuple(elements) =>
        s"TypeTuple([${elements.map(render).mkString(", ")}])"
      case Function(arguments, result) =>
        s"TypeFunction([${arguments.map(render).mkString(", ")}], ${render(result)})"
      case Parenthesized(typeShape) => s"TypeParens(${render(typeShape)})"
      case Unsupported(nodeKind, detail) => s"TypeUnsupported($nodeKind, $detail)"
