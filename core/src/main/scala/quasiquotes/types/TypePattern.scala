package quasiquotes.types

import quasiquotes.parser.TypeShape
import quasiquotes.source.*

sealed trait TypePattern derives CanEqual:
  final def containsHole: Boolean = TypePattern.containsHole(this)

object TypePattern:
  final case class TPHole(name: String) extends TypePattern
  final case class TPIdent(name: String) extends TypePattern
  final case class TPApply(constructor: TypePattern, arguments: List[TypePattern]) extends TypePattern
  final case class TPTuple(elements: List[TypePattern]) extends TypePattern
  final case class TPFunction(arguments: List[TypePattern], result: TypePattern) extends TypePattern

  private val HolePrefix = "__tqhole_"

  def rewriteSourceMapped(source: String): MappedHoleSource =
    HoleSourceRewriter.rewrite(
      source,
      HolePrefix,
      HoleRole.TypePattern,
      SourceId.TypePattern,
      SourceId.VirtualTypePatternParserInput
    )

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypePattern] =
    fromShapeUsing(shape, name => Option.when(name.startsWith(HolePrefix))(name.drop(HolePrefix.length)))

  private[types] def fromShapeWithHoles(
      shape: TypeShape,
      generatedHoles: GeneratedHoleIndex
  ): Either[TypeQuasiquoteError, TypePattern] =
    fromShapeUsing(shape, generatedHoles.semanticNameFor)

  private def fromShapeUsing(
      shape: TypeShape,
      semanticHoleName: String => Option[String]
  ): Either[TypeQuasiquoteError, TypePattern] =
    shape match
      case TypeShape.Identifier(name) =>
        semanticHoleName(name) match
          case Some(holeName) => Right(TPHole(holeName))
          case None => TypeNormalForm.fromShape(TypeShape.Identifier(name)).map(_ => TPIdent(name))
      case TypeShape.Parenthesized(typeShape) =>
        fromShapeUsing(typeShape, semanticHoleName)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        collect(arguments.map(fromShapeUsing(_, semanticHoleName)))
          .map(argumentPatterns => TPApply(TPIdent(name), argumentPatterns))
      case TypeShape.Apply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported type pattern shape for Phase 18 type-hole matching: ${TypeShape.Apply(constructor, arguments).render}"))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShapeUsing(_, semanticHoleName))).map(TPTuple(_))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(s"Unsupported tuple type pattern shape for Phase 18 type-hole matching: ${TypeShape.Tuple(elements).render}"))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentPatterns <- collect(arguments.map(fromShapeUsing(_, semanticHoleName)))
          resultPattern <- fromShapeUsing(result, semanticHoleName)
        yield TPFunction(argumentPatterns, resultPattern)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(s"Unsupported function type pattern shape for Phase 18 type-hole matching: ${TypeShape.Function(arguments, result).render}"))
      case TypeShape.Select(_, _) =>
        Left(TypeQuasiquoteError("Selected type syntax is not supported for Phase 18 type-hole matching; `scala.Int` vs `Int` remains an explicit TODO."))
      case unsupported =>
        Left(TypeQuasiquoteError(s"Unsupported type pattern shape for Phase 18 type-hole matching: ${unsupported.render}"))

  def matchNormalForm(pattern: TypePattern, target: TypeNormalForm): Option[TypeMatchResult] =
    matchInto(pattern, target, Map.empty).map(TypeMatchResult(_))

  def containsHole(pattern: TypePattern): Boolean =
    pattern match
      case TPHole(_) => true
      case TPIdent(_) => false
      case TPApply(constructor, arguments) => containsHole(constructor) || arguments.exists(containsHole)
      case TPTuple(elements) => elements.exists(containsHole)
      case TPFunction(arguments, result) => arguments.exists(containsHole) || containsHole(result)

  private def matchInto(
      pattern: TypePattern,
      target: TypeNormalForm,
      bindings: Map[String, TypeNormalForm]
  ): Option[Map[String, TypeNormalForm]] =
    (pattern, target) match
      case (TPHole(name), normalForm) =>
        bindings.get(name) match
          case Some(existing) if existing == normalForm => Some(bindings)
          case Some(_) => None
          case None => Some(bindings.updated(name, normalForm))
      case (TPIdent(name), TypeNormalForm.STypeIdent(targetName)) if name == targetName =>
        Some(bindings)
      case (TPApply(patternConstructor, patternArguments), TypeNormalForm.STypeApply(targetConstructor, targetArguments))
          if patternArguments.size == targetArguments.size =>
        matchInto(patternConstructor, targetConstructor, bindings).flatMap { constructorBindings =>
          patternArguments.zip(targetArguments).foldLeft(Option(constructorBindings)) {
            case (Some(currentBindings), (patternArgument, targetArgument)) =>
              matchInto(patternArgument, targetArgument, currentBindings)
            case (None, _) => None
          }
        }
      case (TPTuple(patternElements), TypeNormalForm.STypeTuple(targetElements)) if patternElements.size == targetElements.size =>
        patternElements.zip(targetElements).foldLeft(Option(bindings)) {
          case (Some(currentBindings), (patternElement, targetElement)) =>
            matchInto(patternElement, targetElement, currentBindings)
          case (None, _) => None
        }
      case (TPFunction(patternArguments, patternResult), TypeNormalForm.STypeFunction(targetArguments, targetResult))
          if patternArguments.size == targetArguments.size =>
        patternArguments.zip(targetArguments).foldLeft(Option(bindings)) {
          case (Some(currentBindings), (patternArgument, targetArgument)) =>
            matchInto(patternArgument, targetArgument, currentBindings)
          case (None, _) => None
        }.flatMap(matchInto(patternResult, targetResult, _))
      case _ =>
        None

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
