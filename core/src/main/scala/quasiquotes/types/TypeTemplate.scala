package quasiquotes.types

import quasiquotes.parser.TypeShape
import quasiquotes.source.*

sealed trait TypeTemplate derives CanEqual

object TypeTemplate:
  final case class TTHole(name: String) extends TypeTemplate
  final case class TTIdent(name: String) extends TypeTemplate
  final case class TTResolved(id: ResolvedTypeNameId) extends TypeTemplate
  final case class TTApply(constructor: TypeTemplate, arguments: List[TypeTemplate]) extends TypeTemplate
  final case class TTTuple(elements: List[TypeTemplate]) extends TypeTemplate
  final case class TTFunction(arguments: List[TypeTemplate], result: TypeTemplate) extends TypeTemplate

  private val HolePrefix = "__tqconstructhole_"
  private val ConstructibleIdentifiers = Set("Int", "String", "Boolean")

  def rewriteSourceMapped(source: String): MappedHoleSource =
    HoleSourceRewriter.rewrite(
      source,
      HolePrefix,
      HoleRole.TypeTemplate,
      SourceId.TypeTemplate,
      SourceId.VirtualTypeTemplateParserInput
    )

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypeTemplate] =
    fromShapeUsing(shape, name => Option.when(name.startsWith(HolePrefix))(name.drop(HolePrefix.length)))

  private[quasiquotes] def fromShapeWithHoles(
      shape: TypeShape,
      generatedHoles: GeneratedHoleIndex
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    fromShapeUsing(shape, generatedHoles.semanticNameFor)

  private[quasiquotes] def fromShapeResolvedWithHoles(
      shape: TypeShape,
      generatedHoles: GeneratedHoleIndex,
      environment: ResolvedTypeEnvironment
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    fromShapeResolvedUsing(shape, generatedHoles.semanticNameFor, environment)

  private[quasiquotes] def fromShapeResolved(
      shape: TypeShape,
      environment: ResolvedTypeEnvironment
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    fromShapeResolvedUsing(
      shape,
      name => Option.when(name.startsWith(HolePrefix))(name.drop(HolePrefix.length)),
      environment
    )

  private def fromShapeUsing(
      shape: TypeShape,
      semanticHoleName: String => Option[String]
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    shape match
      case TypeShape.Identifier(name) =>
        semanticHoleName(name) match
          case Some(holeName) => Right(TTHole(holeName))
          case None => validateTemplateIdentifier(name).map(_ => TTIdent(name))
      case TypeShape.Parenthesized(typeShape) =>
        fromShapeUsing(typeShape, semanticHoleName)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy
            .forConstruction(name, arguments.size)
            .isDefined =>
        collect(arguments.map(fromShapeUsing(_, semanticHoleName)))
          .map(argumentTemplates => TTApply(TTIdent(name), argumentTemplates))
      case TypeShape.Apply(TypeShape.Identifier(name), arguments) =>
        semanticHoleName(name) match
          case Some(holeName) =>
            Left(TypeQuasiquoteError(TypeDiagnosticMessages.constructorHole(holeName)))
          case None =>
            Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedAppliedConstructor(name, arguments.size)))
      case TypeShape.Apply(TypeShape.Select(qualifier, name), _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedConstructor(qualifier, name)))
      case TypeShape.Apply(_, _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("type-template construction")))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShapeUsing(_, semanticHoleName))).map(TTTuple(_))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTupleArity("type-template construction", elements.size)))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentTemplates <- collect(arguments.map(fromShapeUsing(_, semanticHoleName)))
          resultTemplate <- fromShapeUsing(result, semanticHoleName)
        yield TTFunction(argumentTemplates, resultTemplate)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedFunctionArity("type-template construction", arguments.size)))
      case TypeShape.Select(qualifier, name) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedType(qualifier, name)))
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("type-template construction")))

  private def fromShapeResolvedUsing(
      shape: TypeShape,
      semanticHoleName: String => Option[String],
      environment: ResolvedTypeEnvironment
  ): Either[TypeQuasiquoteError, TypeTemplate] =
    shape match
      case TypeShape.Identifier(name) =>
        semanticHoleName(name) match
          case Some(holeName) => Right(TTHole(holeName))
          case None => validateTemplateIdentifier(name).map(_ => TTIdent(name))
      case selected @ TypeShape.Select(_, _) =>
        environment.resolveSelected(selected).map(TTResolved(_))
      case TypeShape.Parenthesized(inner) =>
        fromShapeResolvedUsing(inner, semanticHoleName, environment)
      case TypeShape.Apply(selected @ TypeShape.Select(_, _), arguments) =>
        for
          id <- environment.resolveSelected(selected)
          _ <- AppliedTypeConstructorPolicy
            .forResolved(id, arguments.size)
            .toRight(
              TypeQuasiquoteError(
                TypeNameResolutionDiagnostics.constructorPolicyMismatch(id, arguments.size)
              )
            )
          argumentTemplates <- collect(
            arguments.map(fromShapeResolvedUsing(_, semanticHoleName, environment))
          )
        yield TTApply(TTResolved(id), argumentTemplates)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy.forConstruction(name, arguments.size).isDefined =>
        collect(arguments.map(fromShapeResolvedUsing(_, semanticHoleName, environment)))
          .map(forms => TTApply(TTIdent(name), forms))
      case TypeShape.Apply(TypeShape.Identifier(name), arguments) =>
        semanticHoleName(name) match
          case Some(holeName) => Left(TypeQuasiquoteError(TypeDiagnosticMessages.constructorHole(holeName)))
          case None => Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedAppliedConstructor(name, arguments.size)))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShapeResolvedUsing(_, semanticHoleName, environment))).map(TTTuple(_))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentTemplates <- collect(arguments.map(fromShapeResolvedUsing(_, semanticHoleName, environment)))
          resultTemplate <- fromShapeResolvedUsing(result, semanticHoleName, environment)
        yield TTFunction(argumentTemplates, resultTemplate)
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("resolved type-template construction")))

  def construct(template: TypeTemplate, bindings: Map[String, TypeNormalForm]): Either[TypeQuasiquoteError, TypeNormalForm] =
    template match
      case TTHole(name) =>
        bindings.get(name).toRight(TypeQuasiquoteError(s"Missing type-construction binding `$$$name`."))
      case TTIdent(name) =>
        Right(TypeNormalForm.STypeIdent(name))
      case TTResolved(id) =>
        Right(TypeNormalForm.STypeResolved(id))
      case TTApply(constructor, arguments) =>
        for
          constructorForm <- construct(constructor, bindings)
          argumentForms <- collect(arguments.map(construct(_, bindings)))
        yield TypeNormalForm.STypeApply(constructorForm, argumentForms)
      case TTTuple(elements) =>
        collect(elements.map(construct(_, bindings))).map(TypeNormalForm.STypeTuple(_))
      case TTFunction(arguments, result) =>
        for
          argumentForms <- collect(arguments.map(construct(_, bindings)))
          resultForm <- construct(result, bindings)
        yield TypeNormalForm.STypeFunction(argumentForms, resultForm)

  def validateConstructed(normalForm: TypeNormalForm): Either[TypeQuasiquoteError, Unit] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) if ConstructibleIdentifiers(name) =>
        Right(())
      case TypeNormalForm.STypeIdent(name) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedConstructionIdentifier(name)))
      case TypeNormalForm.STypeResolved(_) =>
        Right(())
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), arguments)
          if AppliedTypeConstructorPolicy
            .forConstruction(name, arguments.size)
            .isDefined =>
        collect(arguments.map(validateConstructed)).map(_ => ())
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeResolved(id), arguments)
          if AppliedTypeConstructorPolicy.forResolved(id, arguments.size).isDefined =>
        collect(arguments.map(validateConstructed)).map(_ => ())
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported constructed applied type `${ConstructedType.renderSource(TypeNormalForm.STypeApply(constructor, arguments))}`; supported constructors are ${TypeDiagnosticMessages.SupportedConstructors}."))
      case TypeNormalForm.STypeTuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(validateConstructed)).map(_ => ())
      case TypeNormalForm.STypeTuple(elements) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTupleArity("constructed types", elements.size)))
      case TypeNormalForm.STypeFunction(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        collect(arguments.map(validateConstructed) :+ validateConstructed(result)).map(_ => ())
      case TypeNormalForm.STypeFunction(arguments, result) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedFunctionArity("constructed types", arguments.size)))

  def holeNames(template: TypeTemplate): Set[String] =
    template match
      case TTHole(name) => Set(name)
      case TTIdent(_) => Set.empty
      case TTResolved(_) => Set.empty
      case TTApply(constructor, arguments) => holeNames(constructor) ++ arguments.flatMap(holeNames).toSet
      case TTTuple(elements) => elements.flatMap(holeNames).toSet
      case TTFunction(arguments, result) => arguments.flatMap(holeNames).toSet ++ holeNames(result)

  /** Logical hole names in first structural occurrence order.
    *
    * This is deterministic internal traversal evidence, not a stable public
    * ordering promise.
    */
  private[quasiquotes] def requiredBindings(
      template: TypeTemplate
  ): Vector[String] =
    holeOccurrences(template).distinct

  private[quasiquotes] def validateTemplate(
      template: TypeTemplate
  ): Either[TypeQuasiquoteError, Unit] =
    val required = requiredBindings(template)
    required
      .find(name => !isValidHoleName(name))
      .map(name =>
        Left(
          TypeQuasiquoteError(
            s"Invalid type-construction hole name `$name`: expected a nonempty ASCII identifier."
          )
        )
      )
      .getOrElse {
        val bindings =
          required.map(_ -> TypeNormalForm.STypeIdent("Int")).toMap
        construct(template, bindings).flatMap(validateConstructed)
      }

  private[types] def firstMissingHole(
      template: TypeTemplate,
      bindings: Map[String, TypeNormalForm]
  ): Option[String] =
    template match
      case TTHole(name) => Option.when(!bindings.contains(name))(name)
      case TTIdent(_) => None
      case TTResolved(_) => None
      case TTApply(constructor, arguments) =>
        firstMissingHole(constructor, bindings).orElse(firstMissingIn(arguments, bindings))
      case TTTuple(elements) =>
        firstMissingIn(elements, bindings)
      case TTFunction(arguments, result) =>
        firstMissingIn(arguments, bindings).orElse(firstMissingHole(result, bindings))

  private def validateTemplateIdentifier(name: String): Either[TypeQuasiquoteError, Unit] =
    if ConstructibleIdentifiers(name) then Right(())
    else Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedConstructionIdentifier(name)))

  private def isValidHoleName(name: String): Boolean =
    name.nonEmpty &&
      isAsciiIdentifierStart(name.head) &&
      name.tail.forall(isAsciiIdentifierPart)

  private def isAsciiIdentifierStart(char: Char): Boolean =
    char == '_' ||
      ('A' <= char && char <= 'Z') ||
      ('a' <= char && char <= 'z')

  private def isAsciiIdentifierPart(char: Char): Boolean =
    isAsciiIdentifierStart(char) || ('0' <= char && char <= '9')

  private def firstMissingIn(
      templates: List[TypeTemplate],
      bindings: Map[String, TypeNormalForm]
  ): Option[String] =
    templates.iterator.flatMap(firstMissingHole(_, bindings)).nextOption()

  private[quasiquotes] def holeOccurrences(
      template: TypeTemplate
  ): Vector[String] =
    template match
      case TTHole(name) =>
        Vector(name)
      case TTIdent(_) =>
        Vector.empty
      case TTResolved(_) =>
        Vector.empty
      case TTApply(constructor, arguments) =>
        holeOccurrences(constructor) ++ arguments.toVector.flatMap(
          holeOccurrences
        )
      case TTTuple(elements) =>
        elements.toVector.flatMap(holeOccurrences)
      case TTFunction(arguments, result) =>
        arguments.toVector.flatMap(holeOccurrences) ++ holeOccurrences(result)

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
