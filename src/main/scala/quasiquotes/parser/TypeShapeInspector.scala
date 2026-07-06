package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

object TypeShapeInspector:
  def inspect(tree: untpd.Tree): TypeShape =
    tree match
      case untpd.Ident(name) =>
        TypeShape.Identifier(name.toString)
      case untpd.Select(qualifier, name) =>
        TypeShape.Select(inspect(qualifier), name.toString)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        TypeShape.Apply(inspect(constructor), arguments.map(inspect))
      case untpd.Tuple(elements) =>
        TypeShape.Tuple(elements.map(inspect))
      case untpd.Function(arguments, result) =>
        TypeShape.Function(arguments.map(inspect), inspect(result))
      case untpd.Parens(typeTree) =>
        TypeShape.Parenthesized(inspect(typeTree))
      case untpd.WildcardTypeBoundsTree() =>
        TypeShape.Unsupported("WildcardTypeBoundsTree", tree.toString)
      case other =>
        TypeShape.Unsupported(other.getClass.getSimpleName, other.toString)

  def rawStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) =>
        s"Ident(${name.toString})"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawStructure(qualifier)}, ${name.toString})"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"AppliedTypeTree(${rawStructure(constructor)}, [${arguments.map(rawStructure).mkString(", ")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(rawStructure).mkString(", ")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(rawStructure).mkString(", ")}], ${rawStructure(result)})"
      case untpd.Parens(typeTree) =>
        s"Parens(${rawStructure(typeTree)})"
      case untpd.WildcardTypeBoundsTree() =>
        "WildcardTypeBoundsTree"
      case other =>
        other.getClass.getSimpleName
