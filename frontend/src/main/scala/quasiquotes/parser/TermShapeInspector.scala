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
    var nextBinderId = 0

    def loop(
        current: untpd.Tree,
        scope: List[(String, BinderId)]
    ): TermShape =
      current match
      case untpd.Ident(name) =>
        val text = name.toString
        if Placeholder.isPlaceholder(text) then
          TermShape.Identifier(text, isPlaceholder = true)
        else
          scope.collectFirst { case (`text`, binderId) => binderId } match
            case Some(binderId) => TermShape.BoundReference(binderId, text)
            case None => TermShape.Identifier(text, isPlaceholder = false)
      case untpd.Function(parameters, body) =>
        if scope.nonEmpty then
          TermShape.Unsupported("Lambda1", Lambda1DiagnosticMessages.NestedLambda)
        else
          parameters match
            case (parameter: untpd.ValDef) :: Nil =>
              val parameterName = parameter.name.toString
              if parameter.tpt.isEmpty then
                TermShape.Unsupported("Lambda1", Lambda1DiagnosticMessages.ExplicitParameterType)
              else
                val parameterTypeShape = TypeShapeInspector.inspect(parameter.tpt)
                parameterTypeShape match
                  case TypeShape.Unsupported(_, detail) =>
                    TermShape.Unsupported("Lambda1", s"unsupported parameter type: $detail")
                  case _ =>
                    val binderId = BinderId(nextBinderId)
                    nextBinderId += 1
                    TermShape.Lambda1(
                      binderId,
                      parameterName,
                      normalizeTypeName(renderTypeShape(parameterTypeShape)),
                      loop(body, (parameterName -> binderId) :: scope)
                    )
            case _ =>
              TermShape.Unsupported("Lambda1", Lambda1DiagnosticMessages.ExactlyOneParameter)
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
        TermShape.Select(loop(qualifier, scope), name.toString)
      case untpd.Apply(untpd.Apply(untpd.Select(_: untpd.New, init), _), _)
          if init.toString == "<init>" =>
        TermShape.Unsupported("ConstructorNew", "multiple constructor argument lists are not supported")
      case untpd.Apply(untpd.Select(untpd.New(typeTree), init), arguments)
          if init.toString == "<init>" =>
        inspectNew(typeTree, arguments, scope, loop)
      case untpd.Apply(function, arguments) =>
        TermShape.Apply(loop(function, scope), arguments.map(loop(_, scope)))
      case untpd.InfixOp(left, op, right) =>
        TermShape.Infix(loop(left, scope), op.name.toString, loop(right, scope))
      case untpd.PrefixOp(untpd.Ident(operator), operand) if SupportedUnaryOperators(operator.toString) =>
        TermShape.Unary(operator.toString, loop(operand, scope))
      case interpolation @ untpd.InterpolatedString(prefix, segments) =>
        inspectInterpolation(prefix.toString, segments, scope, loop)
      case untpd.Typed(expression, typeTree) =>
        TermShape.Typed(loop(expression, scope), inspectType(typeTree))
      case untpd.Tuple(elements) =>
        TermShape.Tuple(elements.map(loop(_, scope)))
      case untpd.If(condition, thenBranch, elseBranch) =>
        TermShape.If(loop(condition, scope), loop(thenBranch, scope), loop(elseBranch, scope))
      case untpd.Block(Nil, result) =>
        loop(result, scope)
      case untpd.Block(statements, result) =>
        inspectBlock(statements, result, scope, loop)
      case untpd.TypedSplice(tree) =>
        loop(tree, scope)
      case untpd.Parens(tree) =>
        TermShape.Parenthesized(loop(tree, scope))
      case untpd.New(_: untpd.Template) =>
        TermShape.Unsupported("ConstructorNew", "anonymous constructor templates are not supported")
      case other =>
        val kind = other.getClass.getSimpleName
        if kind.contains("Context") && kind.contains("Function") then
          TermShape.Unsupported("Lambda1", Lambda1DiagnosticMessages.ContextFunction)
        else TermShape.Unsupported(kind, other.toString)

    loop(tree, Nil)

  def rawStructure(tree: untpd.Tree): String =
    tree match
      case untpd.Function(parameters, body) =>
        s"Function([${parameters.map(rawStructure).mkString(", ")}], ${rawStructure(body)})"
      case parameter: untpd.ValDef =>
        s"ValDef(${parameter.name.toString}, ${rawTypeStructure(parameter.tpt)})"
      case untpd.Ident(name) =>
        s"Ident(${name.toString})"
      case untpd.Literal(constant) =>
        rawConstantStructure(constant)
      case untpd.Number(digits, kind) =>
        s"Number($digits,$kind)"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawStructure(qualifier)}, ${name.toString})"
      case untpd.New(typeTree) =>
        s"New(${rawTypeStructure(typeTree)})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${rawStructure(function)}, [${arguments.map(rawStructure).mkString(", ")}])"
      case untpd.InfixOp(left, op, right) =>
        s"InfixOp(${rawStructure(left)},${rawStructure(op)},${rawStructure(right)})"
      case untpd.PrefixOp(untpd.Ident(operator), operand) if SupportedUnaryOperators(operator.toString) =>
        s"PrefixOp(${operator.toString},${rawStructure(operand)})"
      case untpd.InterpolatedString(prefix, segments) =>
        s"InterpolatedString(${prefix.toString}, [${segments.map(rawInterpolationSegment).mkString(", ")}])"
      case untpd.Typed(expression, typeTree) =>
        s"Typed(${rawStructure(expression)},${rawTypeStructure(typeTree)})"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(rawStructure).mkString(", ")}])"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${rawStructure(condition)},${rawStructure(thenBranch)},${rawStructure(elseBranch)})"
      case untpd.Block(statements, result) =>
        s"Block([${statements.map(rawStructure).mkString(", ")}], ${rawStructure(result)})"
      case untpd.TypedSplice(tree) =>
        s"TypedSplice(${rawStructure(tree)})"
      case untpd.Parens(tree) =>
        s"Parens(${rawStructure(tree)})"
      case other =>
        other.getClass.getSimpleName

  private def inspectNew(
      typeTree: untpd.Tree,
      arguments: List[untpd.Tree],
      scope: List[(String, BinderId)],
      inspectInScope: (untpd.Tree, List[(String, BinderId)]) => TermShape
  ): TermShape =
    val constructor = constructorName(typeTree)
    if arguments.exists(_.isInstanceOf[untpd.NamedArg]) then
      TermShape.Unsupported("ConstructorNew", "named constructor arguments are not supported")
    else
      constructor
        .flatMap(ConstructorNamePolicy.validate)
        .fold(
          detail => TermShape.Unsupported("ConstructorNew", detail),
          name => TermShape.New(name, arguments.map(inspectInScope(_, scope)))
        )

  private def inspectBlock(
      statements: List[untpd.Tree],
      result: untpd.Tree,
      scope: List[(String, BinderId)],
      inspectInScope: (untpd.Tree, List[(String, BinderId)]) => TermShape
  ): TermShape =
    statements.collectFirst {
      case _: untpd.ValDef => P1BlockDiagnosticMessages.LocalVal
      case _: untpd.DefDef => P1BlockDiagnosticMessages.LocalDef
      case statement if !statement.isTerm =>
        P1BlockDiagnosticMessages.UnsupportedStatement(statement.getClass.getSimpleName)
    } match
      case Some(detail) => TermShape.Unsupported("Block", detail)
      case None =>
        TermShape.Block(
          statements.map(inspectInScope(_, scope)),
          inspectInScope(result, scope)
        )

  private def constructorName(tree: untpd.Tree): Either[String, String] =
    tree match
      case untpd.Ident(name) => Right(name.toString)
      case untpd.Select(qualifier, name) =>
        constructorName(qualifier).map(_ + "." + name.toString)
      case _: untpd.AppliedTypeTree =>
        Left("constructor type arguments are not supported")
      case other =>
        Left(s"unsupported constructor type syntax: ${other.getClass.getSimpleName}")

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

  private def inspectInterpolation(
      prefix: String,
      segments: List[untpd.Tree],
      scope: List[(String, BinderId)],
      inspectInScope: (untpd.Tree, List[(String, BinderId)]) => TermShape
  ): TermShape =
    if prefix != "s" then
      TermShape.Unsupported("InterpolatedStringPrefix", s"unsupported prefix: $prefix")
    else
      InterpolatedStringSegments.decode(segments) match
        case Right(decoded) =>
          TermShape.InterpolatedString(
            prefix,
            decoded.parts,
            decoded.arguments.map(inspectInScope(_, scope))
          )
        case Left(detail) =>
          TermShape.Unsupported("InterpolatedStringSegments", detail)

  private def rawInterpolationSegment(tree: untpd.Tree): String =
    tree match
      case untpd.Literal(constant) if constant.value.isInstanceOf[String] =>
        s"Part(${renderString(constant.value.asInstanceOf[String])})"
      case untpd.Thicket(untpd.Literal(constant) :: argument :: Nil)
          if constant.value.isInstanceOf[String] =>
        val unwrapped = argument match
          case untpd.Block(Nil, expression) => expression
          case other => other
        s"PartArgument(${renderString(constant.value.asInstanceOf[String])}, ${rawStructure(unwrapped)})"
      case other =>
        s"UnsupportedSegment(${other.getClass.getSimpleName})"

  private def renderString(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

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
