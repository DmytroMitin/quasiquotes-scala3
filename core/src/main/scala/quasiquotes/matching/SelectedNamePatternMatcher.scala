package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.TermShape

private[quasiquotes] final case class SelectedNamePatternOccurrence(
    name: String,
    selectOrdinal: Int,
    transport: String
) derives CanEqual

private[quasiquotes] final case class NeutralSelectedNameMatchResult(
    termBindings: Map[String, TermShape],
    selectedNameBindings: Map[String, SelectedMemberName]
) derives CanEqual:
  def termBinding(name: String): Option[TermShape] =
    Option(name).flatMap(value => termBindings.get(value.stripPrefix("$")))

  def selectedNameBinding(name: String): Option[SelectedMemberName] =
    Option(name).flatMap(value => selectedNameBindings.get(value.stripPrefix("$")))

private[quasiquotes] sealed trait NeutralSelectedNameMatchError derives CanEqual:
  def message: String

private[quasiquotes] object NeutralSelectedNameMatchError:
  final case class InvalidPattern(detail: String)
      extends NeutralSelectedNameMatchError:
    def message: String = s"Invalid selected-name pattern: $detail."

  final case class InvalidSelectedNameOccurrence(detail: String)
      extends NeutralSelectedNameMatchError:
    def message: String = s"Invalid selected-name occurrence: $detail."

  final case class DuplicateSelectedNameCapture()
      extends NeutralSelectedNameMatchError:
    def message: String =
      "The first selected-name matching tranche requires exactly one selected-name field."

  final case class DuplicateSelectedNameOccurrenceAddress(selectOrdinal: Int)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Selected-name occurrences share Select ordinal $selectOrdinal."

  final case class SelectedNameTransportConflict(transport: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Selected-name transport $transport does not identify exactly one selected-name occurrence."

  final case class CaptureCategoryConflict(name: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Capture name $name is used by both an ordinary Term hole and the selected-name hole."

  final case class UnsupportedPatternShape(detail: String)
      extends NeutralSelectedNameMatchError:
    def message: String = s"Unsupported selected-name pattern shape: $detail."

  final case class InvalidTarget(detail: String)
      extends NeutralSelectedNameMatchError:
    def message: String = s"Invalid selected-name match target: $detail."

  final case class ShapeMismatch(expected: String, actual: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Selected-name pattern shape mismatch: expected $expected, got $actual."

  final case class RepeatedTermCaptureMismatch(name: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Repeated ordinary Term capture $$${name} matched different TermShape values."

  final case class RepeatedSelectedNameCaptureMismatch(name: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Repeated selected-name capture $$${name} matched different SelectedMemberName values."

  final case class MissingSelectedNameCapture(name: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Validated selected-name capture $$${name} was not reached during matching."

  final case class SelectedNameLexicalUnsupported(name: String)
      extends NeutralSelectedNameMatchError:
    def message: String =
      s"Selected name $name is outside the current neutral plain-name intersection."

private[quasiquotes] final class ValidatedSelectedNamePattern private (
    val pattern: TermPattern,
    val occurrences: Vector[SelectedNamePatternOccurrence],
    private val semanticKey: ValidatedSelectedNamePattern.SemanticPatternKey
):
  def occurrence: SelectedNamePatternOccurrence =
    occurrences match
      case Vector(value) => value
      case _ =>
        throw new IllegalStateException(
          "The single selected-name occurrence accessor requires exactly one occurrence."
        )

  override def equals(other: Any): Boolean =
    other match
      case that: ValidatedSelectedNamePattern => semanticKey == that.semanticKey
      case _                                  => false

  override def hashCode: Int = semanticKey.hashCode

  override def toString: String =
    s"ValidatedSelectedNamePattern(${ValidatedSelectedNamePattern.renderKey(semanticKey)})"

private[quasiquotes] object ValidatedSelectedNamePattern:
  import NeutralSelectedNameMatchError.*

  private sealed trait SemanticSelectedNameKey derives CanEqual

  private object SemanticSelectedNameKey:
    final case class Fixed(name: String) extends SemanticSelectedNameKey
    final case class Captured(name: String) extends SemanticSelectedNameKey

  private sealed trait SemanticPatternKey derives CanEqual

  private object SemanticPatternKey:
    final case class Hole(name: String) extends SemanticPatternKey
    final case class Identifier(name: String) extends SemanticPatternKey
    final case class Literal(value: String) extends SemanticPatternKey
    final case class Select(
        qualifier: SemanticPatternKey,
        name: SemanticSelectedNameKey
    ) extends SemanticPatternKey
    final case class Apply(
        function: SemanticPatternKey,
        arguments: Vector[SemanticPatternKey]
    ) extends SemanticPatternKey

  private final case class PatternScan(
      selectNames: Vector[String],
      ordinaryHoleNames: Set[String]
  )

  def create(
      pattern: TermPattern,
      occurrence: SelectedNamePatternOccurrence
  ): Either[NeutralSelectedNameMatchError, ValidatedSelectedNamePattern] =
    for
      presentPattern <- Option(pattern).toRight(InvalidPattern("pattern must be present"))
      presentOccurrence <- Option(occurrence).toRight(
        InvalidSelectedNameOccurrence("metadata must be present")
      )
      _ <- validateOccurrence(presentOccurrence)
      scan <- scanRoot(presentPattern)
      _ <- validateOccurrencesAgainstPattern(
        Vector(presentOccurrence),
        scan,
        singleCompatibility = true
      )
      key = semanticKey(
        presentPattern,
        Map(presentOccurrence.selectOrdinal -> presentOccurrence),
        0
      )._1
    yield new ValidatedSelectedNamePattern(
      presentPattern,
      Vector(presentOccurrence),
      key
    )

  def createTwo(
      pattern: TermPattern,
      first: SelectedNamePatternOccurrence,
      second: SelectedNamePatternOccurrence
  ): Either[NeutralSelectedNameMatchError, ValidatedSelectedNamePattern] =
    for
      presentPattern <- Option(pattern).toRight(InvalidPattern("pattern must be present"))
      presentFirst <- Option(first).toRight(
        InvalidSelectedNameOccurrence("first metadata must be present")
      )
      presentSecond <- Option(second).toRight(
        InvalidSelectedNameOccurrence("second metadata must be present")
      )
      _ <- validateOccurrence(presentFirst)
      _ <- validateOccurrence(presentSecond)
      occurrences = Vector(presentFirst, presentSecond).sortBy(_.selectOrdinal)
      _ <- occurrences
        .groupBy(_.selectOrdinal)
        .collectFirst { case (ordinal, values) if values.size > 1 =>
          DuplicateSelectedNameOccurrenceAddress(ordinal)
        }
        .toLeft(())
      _ <- occurrences
        .groupBy(_.transport)
        .collectFirst { case (transport, values) if values.size > 1 =>
          SelectedNameTransportConflict(transport)
        }
        .toLeft(())
      scan <- scanRoot(presentPattern)
      _ <- validateOccurrencesAgainstPattern(
        occurrences,
        scan,
        singleCompatibility = false
      )
      key = semanticKey(
        presentPattern,
        occurrences.map(value => value.selectOrdinal -> value).toMap,
        0
      )._1
    yield new ValidatedSelectedNamePattern(
      presentPattern,
      occurrences,
      key
    )

  private def validateOccurrence(
      occurrence: SelectedNamePatternOccurrence
  ): Either[NeutralSelectedNameMatchError, Unit] =
    if !isValidPrivateHoleName(occurrence.name) then
      Left(
        InvalidSelectedNameOccurrence(
          "logical name must be a non-empty private hole identifier"
        )
      )
    else if occurrence.selectOrdinal < 0 then
      Left(
        InvalidSelectedNameOccurrence(
          "Select ordinal must be non-negative"
        )
      )
    else if !isValidPrivateHoleName(occurrence.transport) then
      Left(
        InvalidSelectedNameOccurrence(
          "transport must be a non-empty private hole identifier"
        )
      )
    else Right(())

  private def validateOccurrencesAgainstPattern(
      occurrences: Vector[SelectedNamePatternOccurrence],
      scan: PatternScan,
      singleCompatibility: Boolean
  ): Either[NeutralSelectedNameMatchError, Unit] =
    occurrences
      .foldLeft[Either[NeutralSelectedNameMatchError, Unit]](Right(())) {
        case (result, occurrence) =>
          result.flatMap { _ =>
            val transportCount = scan.selectNames.count(_ == occurrence.transport)
            if transportCount > 1 && singleCompatibility then
              Left(DuplicateSelectedNameCapture())
            else if transportCount > 1 then
              Left(SelectedNameTransportConflict(occurrence.transport))
            else if transportCount == 0 then
              Left(
                InvalidSelectedNameOccurrence(
                  "transport does not occur in a selected-name field"
                )
              )
            else
              scan.selectNames.lift(occurrence.selectOrdinal) match
                case None =>
                  Left(
                    InvalidSelectedNameOccurrence(
                      "Select ordinal does not exist in the admitted pattern"
                    )
                  )
                case Some(name) if name != occurrence.transport =>
                  Left(
                    InvalidSelectedNameOccurrence(
                      "pattern selected name at the declared ordinal does not equal the transport"
                    )
                  )
                case Some(_) if scan.ordinaryHoleNames(occurrence.name) =>
                  Left(CaptureCategoryConflict(occurrence.name))
                case Some(_) => Right(())
          }
      }
      .flatMap { _ =>
        val capturedOrdinals = occurrences.map(_.selectOrdinal).toSet
        scan.selectNames.zipWithIndex.collectFirst {
          case (name, ordinal)
              if !capturedOrdinals(ordinal) &&
                DefinitionName.plain(name).isLeft =>
            name
        } match
          case Some(name) => Left(SelectedNameLexicalUnsupported(name))
          case None       => Right(())
      }

  private def scanRoot(
      pattern: TermPattern
  ): Either[NeutralSelectedNameMatchError, PatternScan] =
    pattern match
      case select: TermPattern.Select => scanSelect(select)
      case TermPattern.Apply(function: TermPattern.Select, arguments) =>
        Option(arguments)
          .toRight(InvalidPattern("application arguments must be present"))
          .flatMap {
            case Nil => scanSelect(function)
            case argument :: Nil =>
              for
                functionScan <- scanSelect(function)
                argumentScan <- scanArgument(argument)
              yield combine(functionScan, argumentScan)
            case values =>
              Left(
                UnsupportedPatternShape(
                  s"selected application arity ${values.size}; only zero or one argument is admitted"
                )
              )
          }
      case TermPattern.Apply(_, _) =>
        Left(
          UnsupportedPatternShape(
            "Apply root must have a Select function"
          )
        )
      case other =>
        Left(
          UnsupportedPatternShape(
            s"root ${patternKind(other)}; expected direct Select or nullary/unary Apply(Select, ...)"
          )
        )

  private def scanSelect(
      pattern: TermPattern.Select
  ): Either[NeutralSelectedNameMatchError, PatternScan] =
    for
      name <- presentText(pattern.name, "selected name")
      qualifier <- Option(pattern.qualifier).toRight(
        InvalidPattern("Select qualifier must be present")
      )
      qualifierScan <- scanQualifier(qualifier)
    yield PatternScan(name +: qualifierScan.selectNames, qualifierScan.ordinaryHoleNames)

  private def scanQualifier(
      pattern: TermPattern
  ): Either[NeutralSelectedNameMatchError, PatternScan] =
    pattern match
      case select: TermPattern.Select => scanSelect(select)
      case simple                    => scanSimple(simple, "Select qualifier")

  private def scanArgument(
      pattern: TermPattern
  ): Either[NeutralSelectedNameMatchError, PatternScan] =
    scanSimple(pattern, "unary selected application argument")

  private def scanSimple(
      pattern: TermPattern,
      context: String
  ): Either[NeutralSelectedNameMatchError, PatternScan] =
    Option(pattern)
      .toRight(InvalidPattern(s"$context must be present"))
      .flatMap {
        case TermPattern.Hole(name) =>
          if isValidPrivateHoleName(name) then
            Right(PatternScan(Vector.empty, Set(name)))
          else Left(InvalidPattern(s"$context has an invalid ordinary Term-hole name"))
        case TermPattern.Identifier(name) =>
          presentText(name, s"$context identifier name")
            .map(_ => PatternScan(Vector.empty, Set.empty))
        case TermPattern.Literal(value) =>
          Option(value)
            .toRight(InvalidPattern(s"$context literal value must be present"))
            .map(_ => PatternScan(Vector.empty, Set.empty))
        case other =>
          Left(
            UnsupportedPatternShape(
              s"$context ${patternKind(other)}; only Hole, Identifier, or Literal is admitted"
            )
          )
      }

  private def combine(left: PatternScan, right: PatternScan): PatternScan =
    PatternScan(
      left.selectNames ++ right.selectNames,
      left.ordinaryHoleNames ++ right.ordinaryHoleNames
    )

  private def semanticKey(
      pattern: TermPattern,
      occurrencesByOrdinal: Map[Int, SelectedNamePatternOccurrence],
      nextSelectOrdinal: Int
  ): (SemanticPatternKey, Int) =
    pattern match
      case TermPattern.Hole(name) =>
        (SemanticPatternKey.Hole(name), nextSelectOrdinal)
      case TermPattern.Identifier(name) =>
        (SemanticPatternKey.Identifier(name), nextSelectOrdinal)
      case TermPattern.Literal(value) =>
        (SemanticPatternKey.Literal(value), nextSelectOrdinal)
      case TermPattern.Select(qualifier, name) =>
        val ordinal = nextSelectOrdinal
        val selectedKey = occurrencesByOrdinal.get(ordinal) match
          case Some(occurrence) =>
            SemanticSelectedNameKey.Captured(occurrence.name)
          case None => SemanticSelectedNameKey.Fixed(name)
        val (qualifierKey, afterQualifier) =
          semanticKey(qualifier, occurrencesByOrdinal, ordinal + 1)
        (SemanticPatternKey.Select(qualifierKey, selectedKey), afterQualifier)
      case TermPattern.Apply(function, arguments) =>
        val (functionKey, afterFunction) =
          semanticKey(function, occurrencesByOrdinal, nextSelectOrdinal)
        val (argumentKeys, afterArguments) = arguments.foldLeft(
          (Vector.empty[SemanticPatternKey], afterFunction)
        ) { case ((keys, ordinal), argument) =>
          val (argumentKey, afterArgument) =
            semanticKey(argument, occurrencesByOrdinal, ordinal)
          (keys :+ argumentKey, afterArgument)
        }
        (SemanticPatternKey.Apply(functionKey, argumentKeys), afterArguments)
      case other =>
        throw new IllegalStateException(
          s"validated selected-name pattern contained ${patternKind(other)}"
        )

  private def renderKey(key: SemanticPatternKey): String =
    key match
      case SemanticPatternKey.Hole(name)       => s"Hole($$${name})"
      case SemanticPatternKey.Identifier(name) => s"Identifier($name)"
      case SemanticPatternKey.Literal(value)   => s"Literal($value)"
      case SemanticPatternKey.Select(qualifier, name) =>
        val renderedName = name match
          case SemanticSelectedNameKey.Fixed(value)    => s"Fixed($value)"
          case SemanticSelectedNameKey.Captured(value) => s"Captured($$${value})"
        s"Select(${renderKey(qualifier)}, $renderedName)"
      case SemanticPatternKey.Apply(function, arguments) =>
        s"Apply(${renderKey(function)}, [${arguments.map(renderKey).mkString(", ")}])"

  private def presentText(
      value: String,
      label: String
  ): Either[NeutralSelectedNameMatchError, String] =
    Option(value)
      .filter(_.nonEmpty)
      .toRight(InvalidPattern(s"$label must be present and non-empty"))

  private def isValidPrivateHoleName(name: String): Boolean =
    name != null &&
      name.nonEmpty &&
      isIdentifierStart(name.head) &&
      name.tail.forall(isIdentifierPart)

  private def isIdentifierStart(char: Char): Boolean =
    char == '_' ||
      ('A' <= char && char <= 'Z') ||
      ('a' <= char && char <= 'z')

  private def isIdentifierPart(char: Char): Boolean =
    isIdentifierStart(char) || ('0' <= char && char <= '9')

  private def patternKind(pattern: TermPattern): String =
    pattern match
      case null                              => "missing pattern"
      case _: TermPattern.Hole               => "Hole"
      case _: TermPattern.Identifier         => "Identifier"
      case _: TermPattern.BoundReference     => "BoundReference"
      case _: TermPattern.Lambda1            => "Lambda1"
      case _: TermPattern.Literal            => "Literal"
      case _: TermPattern.Select             => "Select"
      case _: TermPattern.Apply              => "Apply"
      case _: TermPattern.New                => "New"
      case _: TermPattern.Infix              => "Infix"
      case _: TermPattern.Unary              => "Unary"
      case _: TermPattern.InterpolatedString => "InterpolatedString"
      case _: TermPattern.Typed              => "Typed"
      case _: TermPattern.Tuple              => "Tuple"
      case _: TermPattern.If                 => "If"
      case _: TermPattern.Block              => "Block"
      case _: TermPattern.Parenthesized      => "Parenthesized"

private[quasiquotes] object SelectedNamePatternMatcher:
  import NeutralSelectedNameMatchError.*

  private final case class MatchState(
      termBindings: Map[String, TermShape],
      selectedNameBindings: Map[String, SelectedMemberName],
      nextSelectOrdinal: Int
  )

  def matchTarget(
      pattern: ValidatedSelectedNamePattern,
      target: TermShape
  ): Either[NeutralSelectedNameMatchError, NeutralSelectedNameMatchResult] =
    for
      presentPattern <- Option(pattern).toRight(
        InvalidPattern("validated pattern must be present")
      )
      presentTarget <- Option(target).toRight(
        InvalidTarget("target must be present")
      )
      state <- loop(
        presentPattern.pattern,
        presentTarget,
        presentPattern.occurrences.map(value => value.selectOrdinal -> value).toMap,
        MatchState(Map.empty, Map.empty, 0)
      )
      _ <-
        if presentPattern.occurrences.size == 1 then
          Either.cond(
            state.selectedNameBindings.keySet ==
              Set(presentPattern.occurrence.name),
            (),
            InvalidSelectedNameOccurrence(
              "validated selected-name occurrence was not reached during matching"
            )
          )
        else
          ensureDeclaredSelectedNameBindings(
            presentPattern,
            state.selectedNameBindings
          )
    yield NeutralSelectedNameMatchResult(
      state.termBindings,
      state.selectedNameBindings
    )

  private[matching] def ensureDeclaredSelectedNameBindings(
      pattern: ValidatedSelectedNamePattern,
      bindings: Map[String, SelectedMemberName]
  ): Either[NeutralSelectedNameMatchError, Unit] =
    val declaredNames = pattern.occurrences.map(_.name).distinct
    declaredNames.find(name => !bindings.contains(name)) match
      case Some(name) => Left(MissingSelectedNameCapture(name))
      case None =>
        val undeclaredNames = bindings.keySet -- declaredNames
        Either.cond(
          undeclaredNames.isEmpty,
          (),
          InvalidSelectedNameOccurrence(
            s"matcher produced undeclared selected-name bindings ${undeclaredNames.toVector.sorted.mkString("[", ", ", "]")}"
          )
        )

  private def loop(
      pattern: TermPattern,
      target: TermShape,
      occurrencesByOrdinal: Map[Int, SelectedNamePatternOccurrence],
      state: MatchState
  ): Either[NeutralSelectedNameMatchError, MatchState] =
    if pattern == null then Left(InvalidPattern("pattern node must be present"))
    else if target == null then Left(InvalidTarget("target node must be present"))
    else
      (pattern, target) match
        case (TermPattern.Hole(name), shape) =>
          state.termBindings.get(name) match
            case None =>
              Right(
                state.copy(termBindings = state.termBindings.updated(name, shape))
              )
            case Some(previous) if previous == shape => Right(state)
            case Some(_) => Left(RepeatedTermCaptureMismatch(name))
        case (
              TermPattern.Identifier(expected),
              TermShape.Identifier(actual, isPlaceholder)
            ) if !isPlaceholder && expected == actual =>
          Right(state)
        case (TermPattern.Literal(expected), TermShape.Literal(actual))
            if expected == actual =>
          Right(state)
        case (
              TermPattern.Select(patternQualifier, patternName),
              TermShape.Select(targetQualifier, targetName)
            ) =>
          val ordinal = state.nextSelectOrdinal
          val afterSelect = state.copy(nextSelectOrdinal = ordinal + 1)
          Option(targetQualifier)
            .toRight(InvalidTarget("Select qualifier must be present"))
            .flatMap { qualifier =>
              occurrencesByOrdinal.get(ordinal) match
                case Some(occurrence) if patternName != occurrence.transport =>
                  Left(
                    InvalidSelectedNameOccurrence(
                      "pattern transport changed after validation"
                    )
                  )
                case Some(occurrence) =>
                  selectedInNeutralIntersection(targetName).flatMap { selectedName =>
                    afterSelect.selectedNameBindings.get(occurrence.name) match
                      case Some(previous) if previous != selectedName =>
                        Left(
                          RepeatedSelectedNameCaptureMismatch(occurrence.name)
                        )
                      case Some(_) =>
                        loop(
                          patternQualifier,
                          qualifier,
                          occurrencesByOrdinal,
                          afterSelect
                        )
                      case None =>
                        loop(
                          patternQualifier,
                          qualifier,
                          occurrencesByOrdinal,
                          afterSelect.copy(
                            selectedNameBindings =
                              afterSelect.selectedNameBindings.updated(
                                occurrence.name,
                                selectedName
                              )
                          )
                        )
                  }
                case None if patternName == targetName =>
                  loop(
                    patternQualifier,
                    qualifier,
                    occurrencesByOrdinal,
                    afterSelect
                  )
                case None =>
                  Left(
                    ShapeMismatch(
                      s"Select(name=$patternName)",
                      s"Select(name=$targetName)"
                    )
                  )
            }
        case (
              TermPattern.Apply(patternFunction, patternArguments),
              TermShape.Apply(targetFunction, targetArguments)
            ) =>
          for
            presentTargetFunction <- Option(targetFunction).toRight(
              InvalidTarget("Apply function must be present")
            )
            presentTargetArguments <- Option(targetArguments).toRight(
              InvalidTarget("Apply arguments must be present")
            )
            _ <- Either.cond(
              patternArguments.size == presentTargetArguments.size,
              (),
              ShapeMismatch(
                s"Apply(arity=${patternArguments.size})",
                s"Apply(arity=${presentTargetArguments.size})"
              )
            )
            afterFunction <- loop(
              patternFunction,
              presentTargetFunction,
              occurrencesByOrdinal,
              state
            )
            afterArguments <- patternArguments
              .zip(presentTargetArguments)
              .foldLeft[Either[NeutralSelectedNameMatchError, MatchState]](
                Right(afterFunction)
              ) { case (result, (nextPattern, nextTarget)) =>
                result.flatMap(
                  loop(nextPattern, nextTarget, occurrencesByOrdinal, _)
                )
              }
          yield afterArguments
        case _ =>
          Left(
            ShapeMismatch(patternKind(pattern), targetKind(target))
          )

  private def selectedInNeutralIntersection(
      decoded: String
  ): Either[NeutralSelectedNameMatchError, SelectedMemberName] =
    SelectedMemberName
      .from(decoded)
      .left
      .map(_ => SelectedNameLexicalUnsupported(decoded))
      .flatMap { selectedName =>
        Either.cond(
          DefinitionName.plain(selectedName.decoded).isRight,
          selectedName,
          SelectedNameLexicalUnsupported(decoded)
        )
      }

  private def patternKind(pattern: TermPattern): String =
    pattern match
      case null                              => "missing pattern"
      case _: TermPattern.Hole               => "Hole"
      case _: TermPattern.Identifier         => "Identifier"
      case _: TermPattern.BoundReference     => "BoundReference"
      case _: TermPattern.Lambda1            => "Lambda1"
      case _: TermPattern.Literal            => "Literal"
      case _: TermPattern.Select             => "Select"
      case _: TermPattern.Apply              => "Apply"
      case _: TermPattern.New                => "New"
      case _: TermPattern.Infix              => "Infix"
      case _: TermPattern.Unary              => "Unary"
      case _: TermPattern.InterpolatedString => "InterpolatedString"
      case _: TermPattern.Typed              => "Typed"
      case _: TermPattern.Tuple              => "Tuple"
      case _: TermPattern.If                 => "If"
      case _: TermPattern.Block              => "Block"
      case _: TermPattern.Parenthesized      => "Parenthesized"

  private def targetKind(target: TermShape): String =
    target match
      case null                            => "missing target"
      case _: TermShape.Identifier         => "Identifier"
      case _: TermShape.BoundReference     => "BoundReference"
      case _: TermShape.Lambda1            => "Lambda1"
      case _: TermShape.Literal            => "Literal"
      case _: TermShape.Select             => "Select"
      case _: TermShape.Apply              => "Apply"
      case _: TermShape.New                => "New"
      case _: TermShape.Infix              => "Infix"
      case _: TermShape.Unary              => "Unary"
      case _: TermShape.InterpolatedString => "InterpolatedString"
      case _: TermShape.Typed              => "Typed"
      case _: TermShape.Tuple              => "Tuple"
      case _: TermShape.If                 => "If"
      case _: TermShape.Block              => "Block"
      case _: TermShape.Parenthesized      => "Parenthesized"
      case _: TermShape.Unsupported        => "Unsupported"
