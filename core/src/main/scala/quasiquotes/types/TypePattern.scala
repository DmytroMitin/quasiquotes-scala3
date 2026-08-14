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

  private[quasiquotes] final case class MatchTrace(
      result: TypeMatchResult,
      holePaths: Map[String, Vector[Int]]
  )

  private final case class MatchState(
      bindings: Map[String, TypeNormalForm],
      holePaths: Map[String, Vector[Int]]
  )

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
      case TypeShape.Apply(TypeShape.Identifier(name), arguments) =>
        semanticHoleName(name) match
          case Some(holeName) =>
            Left(TypeQuasiquoteError(TypeDiagnosticMessages.constructorHole(holeName)))
          case None =>
            Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedAppliedConstructor(name, arguments.size)))
      case TypeShape.Apply(TypeShape.Select(qualifier, name), _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedConstructor(qualifier, name)))
      case TypeShape.Apply(_, _) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("type-pattern construction")))
      case TypeShape.Tuple(elements) if elements.size == 2 || elements.size == 3 =>
        collect(elements.map(fromShapeUsing(_, semanticHoleName))).map(TPTuple(_))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTupleArity("type-pattern construction", elements.size)))
      case TypeShape.Function(arguments, result) if arguments.size == 1 || arguments.size == 2 =>
        for
          argumentPatterns <- collect(arguments.map(fromShapeUsing(_, semanticHoleName)))
          resultPattern <- fromShapeUsing(result, semanticHoleName)
        yield TPFunction(argumentPatterns, resultPattern)
      case TypeShape.Function(arguments, result) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedFunctionArity("type-pattern construction", arguments.size)))
      case TypeShape.Select(qualifier, name) =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.selectedType(qualifier, name)))
      case _ =>
        Left(TypeQuasiquoteError(TypeDiagnosticMessages.unsupportedTypeSyntax("type-pattern construction")))

  def matchNormalForm(pattern: TypePattern, target: TypeNormalForm): Option[TypeMatchResult] =
    matchNormalFormWithPaths(pattern, target).map(_.result)

  private[quasiquotes] def matchNormalFormWithPaths(
      pattern: TypePattern,
      target: TypeNormalForm
  ): Option[MatchTrace] =
    matchInto(pattern, target, Vector.empty, MatchState(Map.empty, Map.empty))
      .map(state => MatchTrace(TypeMatchResult(state.bindings), state.holePaths))

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
      path: Vector[Int],
      state: MatchState
  ): Option[MatchState] =
    (pattern, target) match
      case (TPHole(name), normalForm) =>
        state.bindings.get(name) match
          case Some(existing) if existing == normalForm => Some(state)
          case Some(_) => None
          case None =>
            Some(
              state.copy(
                bindings = state.bindings.updated(name, normalForm),
                holePaths = state.holePaths.updated(name, path)
              )
            )
      case (TPIdent(name), TypeNormalForm.STypeIdent(targetName)) if name == targetName =>
        Some(state)
      case (TPApply(patternConstructor, patternArguments), TypeNormalForm.STypeApply(targetConstructor, targetArguments))
          if patternArguments.size == targetArguments.size =>
        matchInto(patternConstructor, targetConstructor, path, state).flatMap { constructorState =>
          patternArguments.zip(targetArguments).zipWithIndex.foldLeft(Option(constructorState)) {
            case (Some(currentState), ((patternArgument, targetArgument), index)) =>
              matchInto(patternArgument, targetArgument, path :+ index, currentState)
            case (None, _) => None
          }
        }
      case (TPTuple(patternElements), TypeNormalForm.STypeTuple(targetElements)) if patternElements.size == targetElements.size =>
        patternElements.zip(targetElements).zipWithIndex.foldLeft(Option(state)) {
          case (Some(currentState), ((patternElement, targetElement), index)) =>
            matchInto(patternElement, targetElement, path :+ index, currentState)
          case (None, _) => None
        }
      case (TPFunction(patternArguments, patternResult), TypeNormalForm.STypeFunction(targetArguments, targetResult))
          if patternArguments.size == targetArguments.size =>
        patternArguments.zip(targetArguments).zipWithIndex.foldLeft(Option(state)) {
          case (Some(currentState), ((patternArgument, targetArgument), index)) =>
            matchInto(patternArgument, targetArgument, path :+ index, currentState)
          case (None, _) => None
        }.flatMap(matchInto(patternResult, targetResult, path :+ patternArguments.size, _))
      case _ =>
        None

  private def collect[A](values: List[Either[TypeQuasiquoteError, A]]): Either[TypeQuasiquoteError, List[A]] =
    values.foldRight[Either[TypeQuasiquoteError, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
