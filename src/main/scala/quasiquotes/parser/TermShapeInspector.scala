package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.{
  BooleanTag,
  CharTag,
  Constant,
  DoubleTag,
  FloatTag,
  LongTag,
  NullTag,
  StringTag
}
import dotty.tools.dotc.core.Names.Name

object TermShapeInspector:
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  def inspect(tree: untpd.Tree): TermShape =
    tree match
      case untpd.Ident(name) =>
        val text = name.toString
        TermShape.Identifier(text, Placeholder.isPlaceholder(text))
      case untpd.Literal(constant) =>
        inspectConstant(constant)
      case untpd.Number(digits, untpd.NumberKind.Whole(10)) =>
        TermShape.Literal(digits)
      case untpd.Number(_, untpd.NumberKind.Whole(radix)) =>
        TermShape.Unsupported("NonDecimalIntegerLiteral", s"radix=$radix")
      case untpd.Number(_, untpd.NumberKind.Decimal) =>
        TermShape.Unsupported("DecimalNumberLiteral", "numberKind=Decimal")
      case untpd.Number(_, untpd.NumberKind.Floating) =>
        TermShape.Unsupported("FloatingNumberLiteral", "numberKind=Floating")
      case untpd.Select(qualifier, name) =>
        TermShape.Select(inspect(qualifier), name.toString)
      case untpd.Apply(function, arguments) =>
        TermShape.Apply(inspect(function), arguments.map(inspect))
      case untpd.InfixOp(left, op, right) =>
        TermShape.Infix(inspect(left), op.name.toString, inspect(right))
      case untpd.PrefixOp(untpd.Ident(operator), operand) if SupportedUnaryOperators(operator.toString) =>
        TermShape.Unary(operator.toString, inspect(operand))
      case untpd.Typed(expression, typeTree) =>
        TermShape.Typed(inspect(expression), inspectType(typeTree))
      case untpd.Tuple(elements) =>
        TermShape.Tuple(elements.map(inspect))
      case untpd.If(condition, thenBranch, elseBranch) =>
        TermShape.If(inspect(condition), inspect(thenBranch), inspect(elseBranch))
      case untpd.TypedSplice(tree) =>
        inspect(tree)
      case untpd.Parens(tree) =>
        TermShape.Parenthesized(inspect(tree))
      case other =>
        TermShape.Unsupported(other.getClass.getSimpleName, other.toString)

  def rawStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) =>
        s"Ident(${name.toString})"
      case untpd.Literal(constant) =>
        rawConstantStructure(constant)
      case untpd.Number(digits, kind) =>
        s"Number($digits,$kind)"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawStructure(qualifier)}, ${name.toString})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${rawStructure(function)}, [${arguments.map(rawStructure).mkString(", ")}])"
      case untpd.InfixOp(left, op, right) =>
        s"InfixOp(${rawStructure(left)},${rawStructure(op)},${rawStructure(right)})"
      case untpd.PrefixOp(untpd.Ident(operator), operand) if SupportedUnaryOperators(operator.toString) =>
        s"PrefixOp(${operator.toString},${rawStructure(operand)})"
      case untpd.Typed(expression, typeTree) =>
        s"Typed(${rawStructure(expression)},${rawTypeStructure(typeTree)})"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(rawStructure).mkString(", ")}])"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${rawStructure(condition)},${rawStructure(thenBranch)},${rawStructure(elseBranch)})"
      case untpd.TypedSplice(tree) =>
        s"TypedSplice(${rawStructure(tree)})"
      case untpd.Parens(tree) =>
        s"Parens(${rawStructure(tree)})"
      case other =>
        other.getClass.getSimpleName

  private def inspectConstant(constant: Constant): TermShape =
    constant.tag match
      case BooleanTag =>
        TermShape.Literal(constant.booleanValue.toString)
      case StringTag =>
        TermShape.Literal(renderStringConstant(constant))
      case LongTag =>
        unsupportedConstant("Long")
      case CharTag =>
        unsupportedConstant("Character")
      case FloatTag =>
        unsupportedConstant("Float")
      case DoubleTag =>
        unsupportedConstant("Double")
      case NullTag =>
        unsupportedConstant("Null")
      case tag =>
        TermShape.Unsupported(
          "UnsupportedConstantLiteral",
          s"constantTag=$tag"
        )

  private def unsupportedConstant(kind: String): TermShape.Unsupported =
    TermShape.Unsupported(
      s"${kind}Literal",
      s"parser-origin constant kind $kind is not supported"
    )

  private def rawConstantStructure(constant: Constant): String =
    constant.tag match
      case BooleanTag =>
        s"Literal(Boolean(${constant.booleanValue}))"
      case StringTag =>
        s"Literal(String(${renderStringConstant(constant)}))"
      case LongTag =>
        "Literal(Long)"
      case CharTag =>
        "Literal(Character)"
      case FloatTag =>
        "Literal(Float)"
      case DoubleTag =>
        "Literal(Double)"
      case NullTag =>
        "Literal(Null)"
      case tag =>
        s"Literal(UnsupportedConstantTag($tag))"

  private def renderStringConstant(constant: Constant): String =
    "\"" + constant.value.asInstanceOf[String] + "\""

  private def inspectType(tree: untpd.Tree): String =
    normalizeTypeName(renderTypeShape(TypeShapeInspector.inspect(tree)))

  private def renderTypeShape(shape: TypeShape): String =
    shape match
      case TypeShape.Identifier(name) =>
        name
      case TypeShape.Select(qualifier, name) =>
        s"${renderTypeShape(qualifier)}.$name"
      case TypeShape.Apply(constructor, arguments) =>
        s"${renderTypeShape(constructor)}[${arguments.map(renderTypeShape).mkString(", ")}]"
      case TypeShape.Tuple(elements) =>
        s"(${elements.map(renderTypeShape).mkString(", ")})"
      case TypeShape.Function(argument :: Nil, result) =>
        s"${renderTypeShape(argument)} => ${renderTypeShape(result)}"
      case TypeShape.Function(arguments, result) =>
        s"(${arguments.map(renderTypeShape).mkString(", ")}) => ${renderTypeShape(result)}"
      case TypeShape.Parenthesized(typeShape) =>
        s"(${renderTypeShape(typeShape)})"
      case TypeShape.Unsupported(_, detail) =>
        detail

  private def rawTypeStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident(${name.toString})"
      case untpd.Select(qualifier, name) => s"Select(${rawTypeStructure(qualifier)}, ${name.toString})"
      case other => other.getClass.getSimpleName

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other
