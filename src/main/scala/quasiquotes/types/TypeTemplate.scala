package quasiquotes.types

import quasiquotes.parser.*
import quasiquotes.source.*

sealed trait TypeTemplate derives CanEqual

object TypeTemplate:
  final case class TTHole(name: String) extends TypeTemplate
  final case class TTIdent(name: String) extends TypeTemplate
  final case class TTApply(constructor: TypeTemplate, arguments: List[TypeTemplate]) extends TypeTemplate
  final case class TTTuple(elements: List[TypeTemplate]) extends TypeTemplate
  final case class TTFunction(arguments: List[TypeTemplate], result: TypeTemplate) extends TypeTemplate

  private val HolePrefix = "__tqconstructhole_"
  private val ConstructibleIdentifiers = Set("Int", "String", "Boolean")
  private val ConstructibleTypeConstructors = Set("List", "Option")

  def fromSource(source: String): Either[TypeQuasiquoteError, TypeTemplate] =
    fromSourceLocated(source).left.map(_.diagnostic)

  def fromSourceLocated(source: String): Either[LocatedDiagnostic[TypeQuasiquoteError], TypeTemplate] =
    val mapped = rewriteSourceMapped(source)
    TinyTypeParser.parse(mapped.generatedSource) match
      case Left(error) =>
        Left(
          LocatedDiagnostic(
            TypeQuasiquoteError(error.summary),
            DiagnosticLocationMapper.fromParseError(error, mapped.originMap)
          )
        )
      case Right(parsed) =>
        fromShape(parsed.shape).left.map { error =>
          LocatedDiagnostic(
            error,
            DiagnosticLocationMapper.wholeGeneratedSource(
              mapped.originMap,
              DottySourceSpanAdapter.fromTree(parsed.rawTree)
            )
          )
        }

  def rewriteSourceMapped(source: String): MappedHoleSource =
    HoleSourceRewriter.rewrite(
      source,
      HolePrefix,
      HoleRole.TypeTemplate,
      SourceId.TypeTemplate,
      SourceId.VirtualTypeTemplateParserInput
    )

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypeTemplate] =
    shape match
      case TypeShape.Identifier(name) if name.startsWith(HolePrefix) =>
        Right(TTHole(name.drop(HolePrefix.length)))
      case TypeShape.Identifier(name) =>
        validateTemplateIdentifier(name).map(_ => TTIdent(name))
      case TypeShape.Parenthesized(typeShape) =>
        fromShape(typeShape)
      case TypeShape.Apply(TypeShape.Identifier("List"), argument :: Nil) =>
        fromShape(argument).map(argumentTemplate => TTApply(TTIdent("List"), List(argumentTemplate)))
      case TypeShape.Apply(TypeShape.Identifier("Option"), argument :: Nil) =>
        fromShape(argument).map(argumentTemplate => TTApply(TTIdent("Option"), List(argumentTemplate)))
      case TypeShape.Apply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported type construction template shape for Phase 21: ${TypeShape.Apply(constructor, arguments).render}"))
      case TypeShape.Tuple(first :: second :: Nil) =>
        for
          firstTemplate <- fromShape(first)
          secondTemplate <- fromShape(second)
        yield TTTuple(List(firstTemplate, secondTemplate))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(s"Unsupported tuple type construction template shape for Phase 21: ${TypeShape.Tuple(elements).render}"))
      case TypeShape.Function(argument :: Nil, result) =>
        for
          argumentTemplate <- fromShape(argument)
          resultTemplate <- fromShape(result)
        yield TTFunction(List(argumentTemplate), resultTemplate)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(s"Unsupported function type construction template shape for Phase 21: ${TypeShape.Function(arguments, result).render}"))
      case TypeShape.Select(_, _) =>
        Left(TypeQuasiquoteError("Selected type syntax is not supported for Phase 21 type construction; `scala.Int` vs `Int` remains an explicit TODO."))
      case unsupported =>
        Left(TypeQuasiquoteError(s"Unsupported type construction template shape for Phase 21: ${unsupported.render}"))

  def construct(template: TypeTemplate, bindings: Map[String, TypeNormalForm]): Either[TypeQuasiquoteError, TypeNormalForm] =
    template match
      case TTHole(name) =>
        bindings.get(name).toRight(TypeQuasiquoteError(s"Missing type-construction binding `$name`"))
      case TTIdent(name) =>
        Right(TypeNormalForm.STypeIdent(name))
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
        Left(TypeQuasiquoteError(s"Unsupported constructed type identifier for Phase 21: $name"))
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), argument :: Nil) if ConstructibleTypeConstructors(name) =>
        validateConstructed(argument)
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported constructed applied type for Phase 21: ${ConstructedType.renderSource(TypeNormalForm.STypeApply(constructor, arguments))}"))
      case TypeNormalForm.STypeTuple(first :: second :: Nil) =>
        collect(List(validateConstructed(first), validateConstructed(second))).map(_ => ())
      case TypeNormalForm.STypeTuple(elements) =>
        Left(TypeQuasiquoteError(s"Unsupported constructed tuple type for Phase 21: ${ConstructedType.renderSource(TypeNormalForm.STypeTuple(elements))}"))
      case TypeNormalForm.STypeFunction(argument :: Nil, result) =>
        collect(List(validateConstructed(argument), validateConstructed(result))).map(_ => ())
      case TypeNormalForm.STypeFunction(arguments, result) =>
        Left(TypeQuasiquoteError(s"Unsupported constructed function type for Phase 21: ${ConstructedType.renderSource(TypeNormalForm.STypeFunction(arguments, result))}"))

  def holeNames(template: TypeTemplate): Set[String] =
    template match
      case TTHole(name) => Set(name)
      case TTIdent(_) => Set.empty
      case TTApply(constructor, arguments) => holeNames(constructor) ++ arguments.flatMap(holeNames).toSet
      case TTTuple(elements) => elements.flatMap(holeNames).toSet
      case TTFunction(arguments, result) => arguments.flatMap(holeNames).toSet ++ holeNames(result)

  private[types] def firstMissingHole(
      template: TypeTemplate,
      bindings: Map[String, TypeNormalForm]
  ): Option[String] =
    template match
      case TTHole(name) => Option.when(!bindings.contains(name))(name)
      case TTIdent(_) => None
      case TTApply(constructor, arguments) =>
        firstMissingHole(constructor, bindings).orElse(firstMissingIn(arguments, bindings))
      case TTTuple(elements) =>
        firstMissingIn(elements, bindings)
      case TTFunction(arguments, result) =>
        firstMissingIn(arguments, bindings).orElse(firstMissingHole(result, bindings))

  private def validateTemplateIdentifier(name: String): Either[TypeQuasiquoteError, Unit] =
    if ConstructibleIdentifiers(name) then Right(())
    else Left(TypeQuasiquoteError(s"Unsupported type construction template identifier for Phase 21: $name"))

  private def firstMissingIn(
      templates: List[TypeTemplate],
      bindings: Map[String, TypeNormalForm]
  ): Option[String] =
    templates.iterator.flatMap(firstMissingHole(_, bindings)).nextOption()

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
