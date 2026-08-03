package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[terms] object TermShapeTraversal:
  val SupportedUnaryOperators: Set[String] = Set("+", "-", "!", "~")

  final case class IdentifierEntry(
      ordinal: Int,
      name: String,
      isPlaceholder: Boolean
  )

  def validateSupported(shape: TermShape): Either[TermConstructionError, Unit] =
    shape match
      case TermShape.Identifier(_, _) | TermShape.Literal(_) =>
        Right(())
      case TermShape.Select(qualifier, _) =>
        validateSupported(qualifier)
      case TermShape.Apply(function, arguments) =>
        validateSupported(function).flatMap(_ => validateAll(arguments))
      case TermShape.Infix(left, _, right) =>
        validateSupported(left).flatMap(_ => validateSupported(right))
      case TermShape.Unary(operator, operand) =>
        if !SupportedUnaryOperators(operator) then
          Left(TermConstructionError.UnsupportedUnaryOperator(operator))
        else validateSupported(operand)
      case TermShape.Typed(expression, _) =>
        validateSupported(expression)
      case TermShape.Tuple(elements) =>
        if elements.size < 2 || elements.size > 22 then
          Left(TermConstructionError.InvalidTupleArity(elements.size))
        else validateAll(elements)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        validateSupported(condition)
          .flatMap(_ => validateSupported(thenBranch))
          .flatMap(_ => validateSupported(elseBranch))
      case TermShape.Parenthesized(expression) =>
        validateSupported(expression)
      case TermShape.Unsupported(_, _) =>
        Left(TermConstructionError.UnsupportedTermShape())

  def canonicalizePlaceholders(shape: TermShape): TermShape =
    shape match
      case TermShape.Identifier(name, _) =>
        TermShape.Identifier(name, false)
      case literal: TermShape.Literal =>
        literal
      case TermShape.Select(qualifier, name) =>
        TermShape.Select(canonicalizePlaceholders(qualifier), name)
      case TermShape.Apply(function, arguments) =>
        TermShape.Apply(
          canonicalizePlaceholders(function),
          arguments.map(canonicalizePlaceholders)
        )
      case TermShape.Infix(left, operator, right) =>
        TermShape.Infix(
          canonicalizePlaceholders(left),
          operator,
          canonicalizePlaceholders(right)
        )
      case TermShape.Unary(operator, operand) =>
        TermShape.Unary(operator, canonicalizePlaceholders(operand))
      case TermShape.Typed(expression, typeName) =>
        TermShape.Typed(canonicalizePlaceholders(expression), typeName)
      case TermShape.Tuple(elements) =>
        TermShape.Tuple(elements.map(canonicalizePlaceholders))
      case TermShape.If(condition, thenBranch, elseBranch) =>
        TermShape.If(
          canonicalizePlaceholders(condition),
          canonicalizePlaceholders(thenBranch),
          canonicalizePlaceholders(elseBranch)
        )
      case TermShape.Parenthesized(expression) =>
        TermShape.Parenthesized(canonicalizePlaceholders(expression))
      case unsupported: TermShape.Unsupported =>
        unsupported

  def identifierEntries(shape: TermShape): Vector[IdentifierEntry] =
    val builder = Vector.newBuilder[IdentifierEntry]
    var ordinal = 0

    def loop(current: TermShape): Unit =
      current match
        case TermShape.Identifier(name, isPlaceholder) =>
          builder += IdentifierEntry(ordinal, name, isPlaceholder)
          ordinal += 1
        case TermShape.Literal(_) =>
          ()
        case TermShape.Select(qualifier, _) =>
          loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case TermShape.Infix(left, _, right) =>
          loop(left)
          loop(right)
        case TermShape.Unary(_, operand) =>
          loop(operand)
        case TermShape.Typed(expression, _) =>
          loop(expression)
        case TermShape.Tuple(elements) =>
          elements.foreach(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition)
          loop(thenBranch)
          loop(elseBranch)
        case TermShape.Parenthesized(expression) =>
          loop(expression)
        case TermShape.Unsupported(_, _) =>
          ()

    loop(shape)
    builder.result()

  def typedNames(shape: TermShape): Vector[String] =
    val builder = Vector.newBuilder[String]

    def loop(current: TermShape): Unit =
      current match
        case TermShape.Typed(expression, typeName) =>
          builder += typeName
          loop(expression)
        case TermShape.Select(qualifier, _) =>
          loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case TermShape.Infix(left, _, right) =>
          loop(left)
          loop(right)
        case TermShape.Unary(_, operand) =>
          loop(operand)
        case TermShape.Tuple(elements) =>
          elements.foreach(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition)
          loop(thenBranch)
          loop(elseBranch)
        case TermShape.Parenthesized(expression) =>
          loop(expression)
        case TermShape.Identifier(_, _) | TermShape.Literal(_) |
            TermShape.Unsupported(_, _) =>
          ()

    loop(shape)
    builder.result()

  def nonIdentifierFields(shape: TermShape): Vector[String] =
    val builder = Vector.newBuilder[String]

    def loop(current: TermShape): Unit =
      current match
        case TermShape.Select(qualifier, name) =>
          builder += name
          loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case TermShape.Infix(left, operator, right) =>
          builder += operator
          loop(left)
          loop(right)
        case TermShape.Unary(operator, operand) =>
          builder += operator
          loop(operand)
        case TermShape.Typed(expression, typeName) =>
          builder += typeName
          loop(expression)
        case TermShape.Tuple(elements) =>
          elements.foreach(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition)
          loop(thenBranch)
          loop(elseBranch)
        case TermShape.Parenthesized(expression) =>
          loop(expression)
        case TermShape.Identifier(_, _) | TermShape.Literal(_) |
            TermShape.Unsupported(_, _) =>
          ()

    loop(shape)
    builder.result()

  def renderNormalForm(normalForm: TypeNormalForm): String =
    normalForm match
      case TypeNormalForm.STypeIdent(name) =>
        name
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        s"${renderNormalForm(constructor)}[${arguments.map(renderNormalForm).mkString(", ")}]"
      case TypeNormalForm.STypeTuple(elements) =>
        s"(${elements.map(renderNormalForm).mkString(", ")})"
      case TypeNormalForm.STypeFunction(argument :: Nil, result) =>
        s"${renderNormalForm(argument)} => ${renderNormalForm(result)}"
      case TypeNormalForm.STypeFunction(arguments, result) =>
        s"(${arguments.map(renderNormalForm).mkString(", ")}) => ${renderNormalForm(result)}"

  def renderTypeTemplate(
      template: TypeTemplate,
      generatedNameFor: String => Option[String]
  ): Either[String, String] =
    template match
      case TypeTemplate.TTHole(name) =>
        generatedNameFor(name).toRight(s"unknown type hole `$name`")
      case TypeTemplate.TTIdent(name) =>
        Right(name)
      case TypeTemplate.TTApply(constructor, arguments) =>
        for
          renderedConstructor <- renderTypeTemplate(constructor, generatedNameFor)
          renderedArguments <- collect(arguments.map(renderTypeTemplate(_, generatedNameFor)))
        yield s"$renderedConstructor[${renderedArguments.mkString(", ")}]"
      case TypeTemplate.TTTuple(elements) =>
        collect(elements.map(renderTypeTemplate(_, generatedNameFor)))
          .map(rendered => s"(${rendered.mkString(", ")})")
      case TypeTemplate.TTFunction(argument :: Nil, result) =>
        for
          renderedArgument <- renderTypeTemplate(argument, generatedNameFor)
          renderedResult <- renderTypeTemplate(result, generatedNameFor)
        yield s"$renderedArgument => $renderedResult"
      case TypeTemplate.TTFunction(arguments, result) =>
        for
          renderedArguments <- collect(arguments.map(renderTypeTemplate(_, generatedNameFor)))
          renderedResult <- renderTypeTemplate(result, generatedNameFor)
        yield s"(${renderedArguments.mkString(", ")}) => $renderedResult"

  def renderLogicalTypeTemplate(template: TypeTemplate): String =
    template match
      case TypeTemplate.TTHole(name) =>
        s"TypeHole($name)"
      case TypeTemplate.TTIdent(name) =>
        s"TypeIdent($name)"
      case TypeTemplate.TTApply(constructor, arguments) =>
        s"TypeApply(${renderLogicalTypeTemplate(constructor)}, [${arguments.map(renderLogicalTypeTemplate).mkString(", ")}])"
      case TypeTemplate.TTTuple(elements) =>
        s"TypeTuple([${elements.map(renderLogicalTypeTemplate).mkString(", ")}])"
      case TypeTemplate.TTFunction(arguments, result) =>
        s"TypeFunction([${arguments.map(renderLogicalTypeTemplate).mkString(", ")}], ${renderLogicalTypeTemplate(result)})"

  def typeHoleOccurrences(template: TypeTemplate): Vector[String] =
    template match
      case TypeTemplate.TTHole(name) =>
        Vector(name)
      case TypeTemplate.TTIdent(_) =>
        Vector.empty
      case TypeTemplate.TTApply(constructor, arguments) =>
        typeHoleOccurrences(constructor) ++ arguments.toVector.flatMap(typeHoleOccurrences)
      case TypeTemplate.TTTuple(elements) =>
        elements.toVector.flatMap(typeHoleOccurrences)
      case TypeTemplate.TTFunction(arguments, result) =>
        arguments.toVector.flatMap(typeHoleOccurrences) ++ typeHoleOccurrences(result)

  def validateTypeTemplate(template: TypeTemplate): Either[String, Unit] =
    val bindings =
      typeHoleOccurrences(template).distinct.map(
        _ -> TypeNormalForm.STypeIdent("Int")
      ).toMap
    TypeTemplate
      .construct(template, bindings)
      .flatMap(TypeTemplate.validateConstructed)
      .left
      .map(_.message)

  private def validateAll(
      shapes: List[TermShape]
  ): Either[TermConstructionError, Unit] =
    shapes.foldLeft[Either[TermConstructionError, Unit]](Right(())) {
      (result, shape) => result.flatMap(_ => validateSupported(shape))
    }

  private def collect(
      values: List[Either[String, String]]
  ): Either[String, List[String]] =
    values.foldRight[Either[String, List[String]]](Right(Nil)) {
      (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
    }
