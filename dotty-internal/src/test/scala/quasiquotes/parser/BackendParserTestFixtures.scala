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
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.source.SourceSpan

/** Test-only backend oracle. It deliberately recreates only the compiler parser
  * observations needed by backend tests, without placing a frontend artifact on
  * the dotty-internal classpath.
  */
private[quasiquotes] object TinyTermParser:
  def parse(source: String): Either[ParseError, ParsedExpression] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val parser = new Parser(SourceFile.virtual("BackendExpr.scala", source))
    val rawTree = parser.expr()
    finish(source, parser, reporter).map(_ =>
      ParsedExpression(
        source,
        rawTree,
        TermShapeInspector.inspect(rawTree),
        TermShapeInspector.rawStructure(rawTree)
      )
    )

  def parseOrThrow(source: String): ParsedExpression =
    parse(source).fold(throw _, identity)

  private def finish(
      source: String,
      parser: Parser,
      reporter: StoreReporter
  )(using Context): Either[ParseError, Unit] =
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty then Left(ParseError.syntax(source, messages))
    else if parser.in.token != Tokens.EOF then
      Left(
        ParseError.trailing(
          source,
          parser.in.offset,
          source.drop(parser.in.offset).trim,
          Tokens.tokenString(parser.in.token)
        )
      )
    else Right(())

private[quasiquotes] final case class ParsedExpression(
    source: String,
    rawTree: untpd.Tree,
    shape: TermShape,
    rawStructure: String
)

private[quasiquotes] object TinyTypeParser:
  def parse(source: String): Either[ParseError, ParsedType] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val parser = new Parser(SourceFile.virtual("BackendType.scala", source))
    val rawTree = parser.typ()
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty then Left(ParseError.syntax(source, messages))
    else if parser.in.token != Tokens.EOF then
      Left(
        ParseError.trailing(
          source,
          parser.in.offset,
          source.drop(parser.in.offset).trim,
          Tokens.tokenString(parser.in.token)
        )
      )
    else
      Right(
        ParsedType(
          source,
          rawTree,
          TypeShapeInspector.inspect(rawTree),
          TypeShapeInspector.rawStructure(rawTree)
        )
      )

  def parseOrThrow(source: String): ParsedType =
    parse(source).fold(throw _, identity)

private[quasiquotes] final case class ParsedType(
    source: String,
    rawTree: untpd.Tree,
    shape: TypeShape,
    rawStructure: String
)

private[quasiquotes] object DottySourceSpanAdapter:
  def fromSpan(span: Span): Option[SourceSpan] =
    Option.when(span.exists && span.start >= 0 && span.end >= span.start) {
      SourceSpan(span.start, span.end)
    }

  def fromTree(tree: untpd.Tree): Option[SourceSpan] =
    fromSpan(tree.span)

private[quasiquotes] object TypeShapeInspector:
  def inspect(tree: untpd.Tree): TypeShape =
    tree match
      case untpd.Ident(name) => TypeShape.Identifier(name.toString)
      case untpd.Select(qualifier, name) =>
        TypeShape.Select(inspect(qualifier), name.toString)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        TypeShape.Apply(inspect(constructor), arguments.map(inspect))
      case untpd.Tuple(elements) => TypeShape.Tuple(elements.map(inspect))
      case untpd.Function(arguments, result) =>
        TypeShape.Function(arguments.map(inspect), inspect(result))
      case untpd.Parens(typeTree) => TypeShape.Parenthesized(inspect(typeTree))
      case untpd.WildcardTypeBoundsTree() =>
        TypeShape.Unsupported("WildcardTypeBoundsTree", tree.toString)
      case other => TypeShape.Unsupported(other.getClass.getSimpleName, other.toString)

  def rawStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident(${name.toString})"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawStructure(qualifier)}, ${name.toString})"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"AppliedTypeTree(${rawStructure(constructor)}, [${arguments.map(rawStructure).mkString(", ")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(rawStructure).mkString(", ")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(rawStructure).mkString(", ")}], ${rawStructure(result)})"
      case untpd.Parens(typeTree) => s"Parens(${rawStructure(typeTree)})"
      case untpd.WildcardTypeBoundsTree() => "WildcardTypeBoundsTree"
      case other => other.getClass.getSimpleName

private[quasiquotes] object TermShapeInspector:
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  def inspect(tree: untpd.Tree): TermShape =
    tree match
      case untpd.Ident(name) =>
        val text = name.toString
        TermShape.Identifier(text, Placeholder.isPlaceholder(text))
      case untpd.Literal(constant) => inspectConstant(constant)
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
      case untpd.PrefixOp(untpd.Ident(operator), operand)
          if SupportedUnaryOperators(operator.toString) =>
        TermShape.Unary(operator.toString, inspect(operand))
      case untpd.Typed(expression, typeTree) =>
        TermShape.Typed(inspect(expression), inspectType(typeTree))
      case untpd.Tuple(elements) => TermShape.Tuple(elements.map(inspect))
      case untpd.If(condition, thenBranch, elseBranch) =>
        TermShape.If(inspect(condition), inspect(thenBranch), inspect(elseBranch))
      case untpd.TypedSplice(tree) => inspect(tree)
      case untpd.Parens(tree) => TermShape.Parenthesized(inspect(tree))
      case other => TermShape.Unsupported(other.getClass.getSimpleName, other.toString)

  def rawStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident(${name.toString})"
      case untpd.Literal(constant) => rawConstantStructure(constant)
      case untpd.Number(digits, kind) => s"Number($digits,$kind)"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawStructure(qualifier)}, ${name.toString})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${rawStructure(function)}, [${arguments.map(rawStructure).mkString(", ")}])"
      case untpd.InfixOp(left, op, right) =>
        s"InfixOp(${rawStructure(left)},${rawStructure(op)},${rawStructure(right)})"
      case untpd.PrefixOp(untpd.Ident(operator), operand)
          if SupportedUnaryOperators(operator.toString) =>
        s"PrefixOp(${operator.toString},${rawStructure(operand)})"
      case untpd.Typed(expression, typeTree) =>
        s"Typed(${rawStructure(expression)},${rawTypeStructure(typeTree)})"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(rawStructure).mkString(", ")}])"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${rawStructure(condition)},${rawStructure(thenBranch)},${rawStructure(elseBranch)})"
      case untpd.TypedSplice(tree) => s"TypedSplice(${rawStructure(tree)})"
      case untpd.Parens(tree) => s"Parens(${rawStructure(tree)})"
      case other => other.getClass.getSimpleName

  private def inspectConstant(constant: Constant): TermShape =
    constant.tag match
      case BooleanTag => TermShape.Literal(constant.booleanValue.toString)
      case StringTag => TermShape.Literal(renderStringConstant(constant))
      case LongTag => unsupportedConstant("Long")
      case CharTag => unsupportedConstant("Character")
      case FloatTag => unsupportedConstant("Float")
      case DoubleTag => unsupportedConstant("Double")
      case NullTag => unsupportedConstant("Null")
      case tag =>
        TermShape.Unsupported("UnsupportedConstantLiteral", s"constantTag=$tag")

  private def unsupportedConstant(kind: String): TermShape.Unsupported =
    TermShape.Unsupported(
      s"${kind}Literal",
      s"parser-origin constant kind $kind is not supported"
    )

  private def rawConstantStructure(constant: Constant): String =
    constant.tag match
      case BooleanTag => s"Literal(Boolean(${constant.booleanValue}))"
      case StringTag => s"Literal(String(${renderStringConstant(constant)}))"
      case LongTag => "Literal(Long)"
      case CharTag => "Literal(Character)"
      case FloatTag => "Literal(Float)"
      case DoubleTag => "Literal(Double)"
      case NullTag => "Literal(Null)"
      case tag => s"Literal(UnsupportedConstantTag($tag))"

  private def renderStringConstant(constant: Constant): String =
    "\"" + constant.value.asInstanceOf[String] + "\""

  private def inspectType(tree: untpd.Tree): String =
    normalizeTypeName(renderTypeShape(TypeShapeInspector.inspect(tree)))

  private def renderTypeShape(shape: TypeShape): String =
    shape match
      case TypeShape.Identifier(name) => name
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
      case TypeShape.Unsupported(_, detail) => detail

  private def rawTypeStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident(${name.toString})"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawTypeStructure(qualifier)}, ${name.toString})"
      case other => other.getClass.getSimpleName

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other
