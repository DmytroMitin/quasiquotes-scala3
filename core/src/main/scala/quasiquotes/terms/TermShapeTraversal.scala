package quasiquotes.terms

import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] object TermShapeTraversal:
  val SupportedUnaryOperators: Set[String] = Set("+", "-", "!", "~")

  final case class IdentifierEntry(
      ordinal: Int,
      name: String,
      isPlaceholder: Boolean
  )

  def validateSupported(shape: TermShape): Either[TermConstructionError, Unit] =
    validateSupportedUsingScope(shape, Nil)

  def validateSupportedInScope(
      shape: TermShape,
      binderId: BinderId
  ): Either[TermConstructionError, Unit] =
    validateSupportedUsingScope(shape, binderId :: Nil)

  def canonicalizePlaceholders(shape: TermShape): TermShape =
    shape match
      case TermShape.Identifier(name, _) =>
        TermShape.Identifier(name, false)
      case bound: TermShape.BoundReference =>
        bound
      case TermShape.Lambda1(binderId, displayName, parameterType, body) =>
        TermShape.Lambda1(
          binderId,
          displayName,
          parameterType,
          canonicalizePlaceholders(body)
        )
      case literal: TermShape.Literal =>
        literal
      case TermShape.Select(qualifier, name) =>
        TermShape.Select(canonicalizePlaceholders(qualifier), name)
      case TermShape.Apply(function, arguments) =>
        TermShape.Apply(
          canonicalizePlaceholders(function),
          arguments.map(canonicalizePlaceholders)
        )
      case TermShape.New(constructor, arguments) =>
        TermShape.New(constructor, arguments.map(canonicalizePlaceholders))
      case TermShape.Infix(left, operator, right) =>
        TermShape.Infix(
          canonicalizePlaceholders(left),
          operator,
          canonicalizePlaceholders(right)
        )
      case TermShape.Unary(operator, operand) =>
        TermShape.Unary(operator, canonicalizePlaceholders(operand))
      case TermShape.InterpolatedString(prefix, parts, arguments) =>
        TermShape.InterpolatedString(prefix, parts, arguments.map(canonicalizePlaceholders))
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
        case TermShape.BoundReference(_, _) =>
          ()
        case TermShape.Lambda1(_, _, _, body) =>
          loop(body)
        case TermShape.Literal(_) =>
          ()
        case TermShape.Select(qualifier, _) =>
          loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case TermShape.New(_, arguments) =>
          arguments.foreach(loop)
        case TermShape.Infix(left, _, right) =>
          loop(left)
          loop(right)
        case TermShape.Unary(_, operand) =>
          loop(operand)
        case TermShape.InterpolatedString(_, _, arguments) =>
          arguments.foreach(loop)
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
        case TermShape.Lambda1(_, _, parameterType, body) =>
          builder += parameterType
          loop(body)
        case TermShape.Select(qualifier, _) =>
          loop(qualifier)
        case TermShape.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case TermShape.New(_, arguments) =>
          arguments.foreach(loop)
        case TermShape.Infix(left, _, right) =>
          loop(left)
          loop(right)
        case TermShape.Unary(_, operand) =>
          loop(operand)
        case TermShape.InterpolatedString(_, _, arguments) =>
          arguments.foreach(loop)
        case TermShape.Tuple(elements) =>
          elements.foreach(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition)
          loop(thenBranch)
          loop(elseBranch)
        case TermShape.Parenthesized(expression) =>
          loop(expression)
        case TermShape.Identifier(_, _) | TermShape.BoundReference(_, _) | TermShape.Literal(_) |
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
        case TermShape.New(constructor, arguments) =>
          builder += constructor
          arguments.foreach(loop)
        case TermShape.Infix(left, operator, right) =>
          builder += operator
          loop(left)
          loop(right)
        case TermShape.Unary(operator, operand) =>
          builder += operator
          loop(operand)
        case TermShape.InterpolatedString(prefix, parts, arguments) =>
          builder += prefix
          builder ++= parts
          arguments.foreach(loop)
        case TermShape.Typed(expression, typeName) =>
          builder += typeName
          loop(expression)
        case TermShape.Lambda1(_, displayName, parameterType, body) =>
          builder += displayName
          builder += parameterType
          loop(body)
        case TermShape.Tuple(elements) =>
          elements.foreach(loop)
        case TermShape.If(condition, thenBranch, elseBranch) =>
          loop(condition)
          loop(thenBranch)
          loop(elseBranch)
        case TermShape.Parenthesized(expression) =>
          loop(expression)
        case TermShape.Identifier(_, _) | TermShape.BoundReference(_, _) | TermShape.Literal(_) |
            TermShape.Unsupported(_, _) =>
          ()

    loop(shape)
    builder.result()

  def alphaNormalize(shape: TermShape): TermShape =
    alphaNormalizeUsing(shape, Nil)

  def alphaNormalizeInScope(
      shape: TermShape,
      binderId: BinderId
  ): TermShape =
    alphaNormalizeUsing(shape, binderId :: Nil)

  private def alphaNormalizeUsing(
      shape: TermShape,
      enclosingBinders: List[BinderId]
  ): TermShape =
    def loop(
        current: TermShape,
        scope: List[(BinderId, BinderId)]
    ): TermShape =
      current match
        case TermShape.BoundReference(binderId, _) =>
          val canonical = scope.collectFirst { case (`binderId`, normalized) => normalized }
          TermShape.BoundReference(canonical.getOrElse(binderId), "")
        case TermShape.Lambda1(binderId, _, parameterType, body) =>
          val normalizedId = BinderId(scope.size)
          TermShape.Lambda1(
            normalizedId,
            "",
            parameterType,
            loop(body, (binderId -> normalizedId) :: scope)
          )
        case identifier: TermShape.Identifier => identifier
        case literal: TermShape.Literal => literal
        case TermShape.Select(qualifier, name) =>
          TermShape.Select(loop(qualifier, scope), name)
        case TermShape.Apply(function, arguments) =>
          TermShape.Apply(loop(function, scope), arguments.map(loop(_, scope)))
        case TermShape.New(constructor, arguments) =>
          TermShape.New(constructor, arguments.map(loop(_, scope)))
        case TermShape.Infix(left, operator, right) =>
          TermShape.Infix(loop(left, scope), operator, loop(right, scope))
        case TermShape.Unary(operator, operand) =>
          TermShape.Unary(operator, loop(operand, scope))
        case TermShape.InterpolatedString(prefix, parts, arguments) =>
          TermShape.InterpolatedString(prefix, parts, arguments.map(loop(_, scope)))
        case TermShape.Typed(expression, typeName) =>
          TermShape.Typed(loop(expression, scope), typeName)
        case TermShape.Tuple(elements) =>
          TermShape.Tuple(elements.map(loop(_, scope)))
        case TermShape.If(condition, thenBranch, elseBranch) =>
          TermShape.If(
            loop(condition, scope),
            loop(thenBranch, scope),
            loop(elseBranch, scope)
          )
        case TermShape.Parenthesized(expression) =>
          TermShape.Parenthesized(loop(expression, scope))
        case unsupported: TermShape.Unsupported => unsupported

    val initialScope = enclosingBinders.zipWithIndex.map { case (binderId, index) =>
      binderId -> BinderId(index)
    }
    loop(shape, initialScope)

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
      shapes: List[TermShape],
      scope: List[BinderId]
  ): Either[TermConstructionError, Unit] =
    shapes.foldLeft[Either[TermConstructionError, Unit]](Right(())) {
      (result, shape) => result.flatMap(_ => validateSupportedUsingScope(shape, scope))
    }

  private def validateSupportedUsingScope(
      shape: TermShape,
      scope: List[BinderId]
  ): Either[TermConstructionError, Unit] =
    shape match
      case TermShape.BoundReference(binderId, _) =>
        Either.cond(scope.contains(binderId), (), TermConstructionError.UnsupportedTermShape())
      case TermShape.Lambda1(binderId, _, _, body) =>
        validateSupportedUsingScope(body, binderId :: scope)
      case TermShape.Identifier(_, _) | TermShape.Literal(_) => Right(())
      case TermShape.Select(qualifier, _) => validateSupportedUsingScope(qualifier, scope)
      case TermShape.Apply(function, arguments) =>
        validateSupportedUsingScope(function, scope).flatMap(_ => validateAll(arguments, scope))
      case TermShape.New(_, arguments) => validateAll(arguments, scope)
      case TermShape.Infix(left, _, right) =>
        validateSupportedUsingScope(left, scope).flatMap(_ => validateSupportedUsingScope(right, scope))
      case TermShape.Unary(operator, operand) =>
        if !SupportedUnaryOperators(operator) then Left(TermConstructionError.UnsupportedUnaryOperator(operator))
        else validateSupportedUsingScope(operand, scope)
      case TermShape.InterpolatedString("s", parts, arguments) =>
        if parts.size != arguments.size + 1 then Left(TermConstructionError.UnsupportedTermShape())
        else validateAll(arguments, scope)
      case TermShape.InterpolatedString(_, _, _) => Left(TermConstructionError.UnsupportedTermShape())
      case TermShape.Typed(expression, _) => validateSupportedUsingScope(expression, scope)
      case TermShape.Tuple(elements) =>
        if elements.size < 2 || elements.size > 22 then Left(TermConstructionError.InvalidTupleArity(elements.size))
        else validateAll(elements, scope)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        validateSupportedUsingScope(condition, scope)
          .flatMap(_ => validateSupportedUsingScope(thenBranch, scope))
          .flatMap(_ => validateSupportedUsingScope(elseBranch, scope))
      case TermShape.Parenthesized(expression) => validateSupportedUsingScope(expression, scope)
      case TermShape.Unsupported(_, _) => Left(TermConstructionError.UnsupportedTermShape())

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
