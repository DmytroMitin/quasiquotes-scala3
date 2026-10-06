package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.TermShape

import scala.compiletime.testing.typeCheckErrors

/** Test-only characterization of a possible private selected-name matching
  * relation. This is deliberately not a production matcher or public API.
  */
final class N060SelectedNameMatchingCaptureModelFitCharacterizationTest
    extends munit.FunSuite:
  private final case class SelectedNamePatternOccurrence(
      name: String,
      selectOrdinal: Int,
      transport: String
  )

  private final case class TypedCaptureResult(
      termBindings: Map[String, TermShape],
      selectedNameBindings: Map[String, SelectedMemberName]
  )

  private enum CharacterizationFailure derives CanEqual:
    case ShapeMismatch
    case InvalidSelectedNamePosition
    case DuplicateSelectedNameCapture
    case CaptureCategoryConflict(name: String)
    case SelectedNameOutsideNeutralIntersection(name: String)

  private final case class State(
      termBindings: Map[String, TermShape],
      selectedNameBindings: Map[String, SelectedMemberName],
      nextSelectOrdinal: Int
  )

  private val selectedTransport = "__n060_selected_name_transport"

  test("current MatchResult remains homogeneous and selected names require a distinct typed field"):
    val receiver = ident("service")
    val result = MatchResult[TermShape](Map("receiver" -> receiver))

    assertEquals(result.binding("receiver"), Some(receiver))
    assertEquals(result.binding("$receiver"), Some(receiver))
    assert(
      typeCheckErrors(
        """val result = quasiquotes.matching.MatchResult[quasiquotes.parser.TermShape](Map("member" -> quasiquotes.construct.SelectedMemberName.from("ordinary").toOption.get))"""
      ).nonEmpty
    )

  test("fixed Select names compare by exact decoded String equality and compose with qualifier capture"):
    val pattern = TermPattern.Select(TermPattern.Hole("receiver"), "ordinary")
    val matching = TermShape.Select(ident("service"), "ordinary")
    val differentCase = TermShape.Select(ident("service"), "Ordinary")
    val symbolic = TermShape.Select(ident("service"), "+")

    assertEquals(
      characterize(pattern, matching, None),
      Right(TypedCaptureResult(Map("receiver" -> ident("service")), Map.empty))
    )
    assertEquals(characterize(pattern, differentCase, None), Left(CharacterizationFailure.ShapeMismatch))
    assertEquals(characterize(pattern, symbolic, None), Left(CharacterizationFailure.ShapeMismatch))

  test("one private selected-name role composes with qualifier and argument Term captures"):
    val pattern = TermPattern.Apply(
      TermPattern.Select(TermPattern.Hole("receiver"), selectedTransport),
      List(TermPattern.Hole("argument"))
    )
    val target = TermShape.Apply(
      TermShape.Select(ident("service"), "ordinary"),
      List(TermShape.Literal("1"))
    )
    val occurrence = SelectedNamePatternOccurrence("member", 0, selectedTransport)

    assertEquals(
      characterize(pattern, target, Some(occurrence)),
      Right(
        TypedCaptureResult(
          Map(
            "receiver" -> ident("service"),
            "argument" -> TermShape.Literal("1")
          ),
          Map("member" -> selected("ordinary"))
        )
      )
    )

  test("Select preorder is root-first and fixed names coexist with one captured name"):
    val pattern = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), selectedTransport),
      "fixedOuter"
    )
    val target = TermShape.Select(
      TermShape.Select(ident("service"), "ordinary"),
      "fixedOuter"
    )
    val occurrence = SelectedNamePatternOccurrence("member", 1, selectedTransport)

    assertEquals(selectNamesPreorder(pattern), Vector("fixedOuter", selectedTransport))
    assertEquals(
      characterize(pattern, target, Some(occurrence)),
      Right(
        TypedCaptureResult(
          Map("receiver" -> ident("service")),
          Map("member" -> selected("ordinary"))
        )
      )
    )
    assertEquals(
      characterize(pattern, target, Some(occurrence.copy(selectOrdinal = 0))),
      Left(CharacterizationFailure.InvalidSelectedNamePosition)
    )

  test("the N-local first tranche retains the neutral plain-name intersection"):
    val pattern = TermPattern.Select(TermPattern.Hole("receiver"), selectedTransport)
    val occurrence = Some(SelectedNamePatternOccurrence("member", 0, selectedTransport))

    Vector("ordinary", "member2", "_privateLike").foreach { name =>
      val result = characterize(pattern, TermShape.Select(ident("service"), name), occurrence)
      assertEquals(result.map(_.selectedNameBindings("member")), Right(selected(name)))
    }

    Vector("+", "type", "safe spaced name", "_", "naïve", "$internal").foreach { name =>
      assertEquals(
        characterize(pattern, TermShape.Select(ident("service"), name), occurrence),
        Left(CharacterizationFailure.SelectedNameOutsideNeutralIntersection(name))
      )
    }

  test("the first tranche is exactly one occurrence and rejects Term/name category collision"):
    val duplicatePattern = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), selectedTransport),
      selectedTransport
    )
    val duplicateTarget = TermShape.Select(
      TermShape.Select(ident("service"), "ordinary"),
      "member2"
    )
    val occurrence = SelectedNamePatternOccurrence("member", 0, selectedTransport)

    assertEquals(
      characterize(duplicatePattern, duplicateTarget, Some(occurrence)),
      Left(CharacterizationFailure.DuplicateSelectedNameCapture)
    )

    val collidingPattern = TermPattern.Apply(
      TermPattern.Select(TermPattern.Hole("member"), selectedTransport),
      Nil
    )
    val collidingTarget = TermShape.Apply(
      TermShape.Select(ident("service"), "ordinary"),
      Nil
    )
    assertEquals(
      characterize(collidingPattern, collidingTarget, Some(occurrence)),
      Left(CharacterizationFailure.CaptureCategoryConflict("member"))
    )

  test("ordinary PatternSource metadata records Term holes only, not selected-name roles"):
    val mapped = PatternSource.synthesizeMapped("$receiver.fixed").toOption.get

    assertEquals(mapped.patternSource.holes, Vector("receiver"))
    assertEquals(mapped.occurrences.map(_.name), Vector("receiver"))
    assertEquals(mapped.patternSource.source.contains(selectedTransport), false)

  private def characterize(
      pattern: TermPattern,
      target: TermShape,
      selectedOccurrence: Option[SelectedNamePatternOccurrence]
  ): Either[CharacterizationFailure, TypedCaptureResult] =
    selectedOccurrence match
      case Some(occurrence) =>
        val exactTransportOccurrences = selectNamesPreorder(pattern).count(_ == occurrence.transport)
        if exactTransportOccurrences > 1 then Left(CharacterizationFailure.DuplicateSelectedNameCapture)
        else if ordinaryHoleNames(pattern).contains(occurrence.name) then
          Left(CharacterizationFailure.CaptureCategoryConflict(occurrence.name))
        else
          loop(pattern, target, selectedOccurrence, State(Map.empty, Map.empty, 0)).flatMap { state =>
            if state.selectedNameBindings.keySet == Set(occurrence.name) then
              Right(TypedCaptureResult(state.termBindings, state.selectedNameBindings))
            else Left(CharacterizationFailure.InvalidSelectedNamePosition)
          }
      case None =>
        loop(pattern, target, None, State(Map.empty, Map.empty, 0))
          .map(state => TypedCaptureResult(state.termBindings, state.selectedNameBindings))

  private def loop(
      pattern: TermPattern,
      target: TermShape,
      selectedOccurrence: Option[SelectedNamePatternOccurrence],
      state: State
  ): Either[CharacterizationFailure, State] =
    (pattern, target) match
      case (TermPattern.Hole(name), shape) =>
        state.termBindings.get(name) match
          case None => Right(state.copy(termBindings = state.termBindings.updated(name, shape)))
          case Some(previous) if previous == shape => Right(state)
          case Some(_) => Left(CharacterizationFailure.ShapeMismatch)
      case (TermPattern.Identifier(expected), TermShape.Identifier(actual, _)) if expected == actual =>
        Right(state)
      case (TermPattern.Literal(expected), TermShape.Literal(actual)) if expected == actual =>
        Right(state)
      case (TermPattern.Select(patternQualifier, patternName), TermShape.Select(targetQualifier, targetName)) =>
        val ordinal = state.nextSelectOrdinal
        val afterSelect = state.copy(nextSelectOrdinal = ordinal + 1)
        selectedOccurrence match
          case Some(occurrence) if occurrence.selectOrdinal == ordinal =>
            if patternName != occurrence.transport then
              Left(CharacterizationFailure.InvalidSelectedNamePosition)
            else
              selectedInNeutralIntersection(targetName).flatMap { selectedName =>
                loop(
                  patternQualifier,
                  targetQualifier,
                  selectedOccurrence,
                  afterSelect.copy(
                    selectedNameBindings = afterSelect.selectedNameBindings.updated(occurrence.name, selectedName)
                  )
                )
              }
          case _ if patternName == targetName =>
            loop(patternQualifier, targetQualifier, selectedOccurrence, afterSelect)
          case _ => Left(CharacterizationFailure.ShapeMismatch)
      case (TermPattern.Apply(patternFunction, patternArguments), TermShape.Apply(targetFunction, targetArguments))
          if patternArguments.size == targetArguments.size =>
        loop(patternFunction, targetFunction, selectedOccurrence, state).flatMap { afterFunction =>
          patternArguments.zip(targetArguments).foldLeft[Either[CharacterizationFailure, State]](
            Right(afterFunction)
          ) { case (result, (nextPattern, nextTarget)) =>
            result.flatMap(loop(nextPattern, nextTarget, selectedOccurrence, _))
          }
        }
      case _ => Left(CharacterizationFailure.ShapeMismatch)

  private def selectedInNeutralIntersection(
      decoded: String
  ): Either[CharacterizationFailure, SelectedMemberName] =
    SelectedMemberName
      .from(decoded)
      .left
      .map(_ => CharacterizationFailure.SelectedNameOutsideNeutralIntersection(decoded))
      .flatMap { selectedName =>
        if DefinitionName.plain(selectedName.decoded).isRight then Right(selectedName)
        else Left(CharacterizationFailure.SelectedNameOutsideNeutralIntersection(decoded))
      }

  private def selectNamesPreorder(pattern: TermPattern): Vector[String] =
    pattern match
      case TermPattern.Select(qualifier, name) => name +: selectNamesPreorder(qualifier)
      case TermPattern.Apply(function, arguments) =>
        selectNamesPreorder(function) ++ arguments.toVector.flatMap(selectNamesPreorder)
      case TermPattern.Infix(left, _, right) => selectNamesPreorder(left) ++ selectNamesPreorder(right)
      case TermPattern.Unary(_, operand) => selectNamesPreorder(operand)
      case TermPattern.InterpolatedString(_, _, arguments) => arguments.toVector.flatMap(selectNamesPreorder)
      case TermPattern.Typed(expression, _) => selectNamesPreorder(expression)
      case TermPattern.Tuple(elements) => elements.toVector.flatMap(selectNamesPreorder)
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        selectNamesPreorder(condition) ++ selectNamesPreorder(thenBranch) ++ selectNamesPreorder(elseBranch)
      case TermPattern.Block(statements, result) =>
        statements.toVector.flatMap {
          case pattern: TermPattern => selectNamesPreorder(pattern)
          case BlockPatternStatement.LocalVal(_, _, _, initializer) => selectNamesPreorder(initializer)
        } ++ selectNamesPreorder(result)
      case TermPattern.Parenthesized(expression) => selectNamesPreorder(expression)
      case TermPattern.Lambda1(_, _, _, body) => selectNamesPreorder(body)
      case TermPattern.New(_, arguments) => arguments.toVector.flatMap(selectNamesPreorder)
      case TermPattern.Hole(_) | TermPattern.Identifier(_) | TermPattern.BoundReference(_, _) |
          TermPattern.Literal(_) => Vector.empty

  private def ordinaryHoleNames(pattern: TermPattern): Set[String] =
    pattern match
      case TermPattern.Hole(name) => Set(name)
      case TermPattern.Select(qualifier, _) => ordinaryHoleNames(qualifier)
      case TermPattern.Apply(function, arguments) =>
        ordinaryHoleNames(function) ++ arguments.iterator.flatMap(ordinaryHoleNames).toSet
      case TermPattern.Infix(left, _, right) => ordinaryHoleNames(left) ++ ordinaryHoleNames(right)
      case TermPattern.Unary(_, operand) => ordinaryHoleNames(operand)
      case TermPattern.InterpolatedString(_, _, arguments) => arguments.iterator.flatMap(ordinaryHoleNames).toSet
      case TermPattern.Typed(expression, _) => ordinaryHoleNames(expression)
      case TermPattern.Tuple(elements) => elements.iterator.flatMap(ordinaryHoleNames).toSet
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        ordinaryHoleNames(condition) ++ ordinaryHoleNames(thenBranch) ++ ordinaryHoleNames(elseBranch)
      case TermPattern.Block(statements, result) =>
        statements.iterator.flatMap {
          case pattern: TermPattern => ordinaryHoleNames(pattern)
          case BlockPatternStatement.LocalVal(_, _, _, initializer) => ordinaryHoleNames(initializer)
        }.toSet ++ ordinaryHoleNames(result)
      case TermPattern.Parenthesized(expression) => ordinaryHoleNames(expression)
      case TermPattern.Lambda1(_, _, _, body) => ordinaryHoleNames(body)
      case TermPattern.New(_, arguments) => arguments.iterator.flatMap(ordinaryHoleNames).toSet
      case TermPattern.Identifier(_) | TermPattern.BoundReference(_, _) | TermPattern.Literal(_) => Set.empty

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
