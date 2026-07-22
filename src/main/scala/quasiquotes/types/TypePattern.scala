package quasiquotes.types

import quasiquotes.parser.*
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

  def fromSource(source: String): Either[TypeQuasiquoteError, TypePattern] =
    fromSourceLocated(source).left.map(_.diagnostic)

  def fromSourceLocated(source: String): Either[LocatedDiagnostic[TypeQuasiquoteError], TypePattern] =
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
      HoleRole.TypePattern,
      SourceId.TypePattern,
      SourceId.VirtualTypePatternParserInput
    )

  def fromShape(shape: TypeShape): Either[TypeQuasiquoteError, TypePattern] =
    shape match
      case TypeShape.Identifier(name) if name.startsWith(HolePrefix) =>
        Right(TPHole(name.drop(HolePrefix.length)))
      case TypeShape.Identifier(name) =>
        TypeNormalForm.fromShape(TypeShape.Identifier(name)).map(_ => TPIdent(name))
      case TypeShape.Parenthesized(typeShape) =>
        fromShape(typeShape)
      case TypeShape.Apply(TypeShape.Identifier("List"), argument :: Nil) =>
        fromShape(argument).map(argumentPattern => TPApply(TPIdent("List"), List(argumentPattern)))
      case TypeShape.Apply(TypeShape.Identifier("Option"), argument :: Nil) =>
        fromShape(argument).map(argumentPattern => TPApply(TPIdent("Option"), List(argumentPattern)))
      case TypeShape.Apply(constructor, arguments) =>
        Left(TypeQuasiquoteError(s"Unsupported type pattern shape for Phase 18 type-hole matching: ${TypeShape.Apply(constructor, arguments).render}"))
      case TypeShape.Tuple(first :: second :: Nil) =>
        for
          firstPattern <- fromShape(first)
          secondPattern <- fromShape(second)
        yield TPTuple(List(firstPattern, secondPattern))
      case TypeShape.Tuple(elements) =>
        Left(TypeQuasiquoteError(s"Unsupported tuple type pattern shape for Phase 18 type-hole matching: ${TypeShape.Tuple(elements).render}"))
      case TypeShape.Function(argument :: Nil, result) =>
        for
          argumentPattern <- fromShape(argument)
          resultPattern <- fromShape(result)
        yield TPFunction(List(argumentPattern), resultPattern)
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
