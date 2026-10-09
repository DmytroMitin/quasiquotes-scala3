package quasiquotes.matching

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.TermShape

import scala.compiletime.testing.typeCheckErrors

/** Test-only characterization of multiple selected-name capture semantics.
  *
  * The local vector metadata and matcher deliberately leave the N061
  * production matcher unchanged.
  */
final class N064MultipleSelectedNameCaptureModelFitCharacterizationTest
    extends munit.FunSuite:
  private final case class Occurrence(
      name: String,
      selectOrdinal: Int,
      transport: String
  )

  private enum Failure derives CanEqual:
    case InvalidSelectedNameOccurrence
    case DuplicateSelectedNameOccurrenceAddress(selectOrdinal: Int)
    case SelectedNameTransportConflict(transport: String)
    case SelectedNameCaptureCategoryConflict(name: String)
    case RepeatedSelectedNameCaptureMismatch(name: String)
    case RepeatedTermCaptureMismatch(name: String)
    case MissingSelectedNameCapture(name: String)
    case SelectedNameLexicalUnsupported(name: String)
    case UnsupportedPatternShape
    case ShapeMismatch

  private enum SelectedKey derives CanEqual:
    case Fixed(name: String)
    case Captured(name: String)

  private enum SemanticKey derives CanEqual:
    case Hole(name: String)
    case Identifier(name: String)
    case Literal(value: String)
    case Select(qualifier: SemanticKey, name: SelectedKey)
    case Apply(function: SemanticKey, arguments: Vector[SemanticKey])

  private final case class Scan(
      selectNames: Vector[String],
      ordinaryHoleNames: Set[String]
  )

  private final case class MatchState(
      termBindings: Map[String, TermShape],
      selectedNameBindings: Map[String, SelectedMemberName],
      nextSelectOrdinal: Int
  )

  private final class Validated private (
      val pattern: TermPattern,
      val occurrences: Vector[Occurrence],
      private val semanticKey: SemanticKey
  ):
    override def equals(other: Any): Boolean =
      other match
        case that: Validated => semanticKey == that.semanticKey
        case _               => false

    override def hashCode: Int = semanticKey.hashCode

  private object Validated:
    import Failure.*

    def create(
        pattern: TermPattern,
        occurrences: Vector[Occurrence]
    ): Either[Failure, Validated] =
      for
        presentPattern <- Option(pattern).toRight(InvalidSelectedNameOccurrence)
        presentOccurrences <- Option(occurrences).toRight(InvalidSelectedNameOccurrence)
        _ <- Either.cond(
          presentOccurrences.size == 2,
          (),
          InvalidSelectedNameOccurrence
        )
        _ <- validateOccurrenceFields(presentOccurrences)
        scan <- scanRoot(presentPattern)
        _ <- validateAddresses(presentOccurrences, scan)
        byOrdinal = presentOccurrences.map(value => value.selectOrdinal -> value).toMap
        semanticKey = key(presentPattern, byOrdinal, 0)._1
      yield new Validated(
        presentPattern,
        presentOccurrences.sortBy(_.selectOrdinal),
        semanticKey
      )

    private def validateOccurrenceFields(
        occurrences: Vector[Occurrence]
    ): Either[Failure, Unit] =
      if occurrences.exists(value =>
          !isHoleName(value.name) ||
            value.selectOrdinal < 0 ||
            !isHoleName(value.transport)
        )
      then Left(InvalidSelectedNameOccurrence)
      else
        occurrences
          .groupBy(_.selectOrdinal)
          .collectFirst { case (ordinal, values) if values.size > 1 =>
            DuplicateSelectedNameOccurrenceAddress(ordinal)
          }
          .orElse(
            occurrences
              .groupBy(_.transport)
              .collectFirst { case (transport, values) if values.size > 1 =>
                SelectedNameTransportConflict(transport)
              }
          ) match
          case Some(error) => Left(error)
          case None        => Right(())

    private def validateAddresses(
        occurrences: Vector[Occurrence],
        scan: Scan
    ): Either[Failure, Unit] =
      occurrences.collectFirst(Function.unlift { occurrence =>
        val count = scan.selectNames.count(_ == occurrence.transport)
        if count > 1 then Some(SelectedNameTransportConflict(occurrence.transport))
        else if count == 0 then Some(InvalidSelectedNameOccurrence)
        else
          scan.selectNames.lift(occurrence.selectOrdinal) match
            case Some(value) if value == occurrence.transport => None
            case _                                            => Some(InvalidSelectedNameOccurrence)
      }) match
        case Some(error) => Left(error)
        case None =>
          occurrences.collectFirst {
            case occurrence if scan.ordinaryHoleNames(occurrence.name) =>
              SelectedNameCaptureCategoryConflict(occurrence.name)
          } match
            case Some(error) => Left(error)
            case None =>
              val capturedOrdinals = occurrences.map(_.selectOrdinal).toSet
              scan.selectNames.zipWithIndex.collectFirst {
                case (name, ordinal)
                    if !capturedOrdinals(ordinal) && !inNeutralIntersection(name) =>
                  SelectedNameLexicalUnsupported(name)
              }.toLeft(())

    private def scanRoot(pattern: TermPattern): Either[Failure, Scan] =
      pattern match
        case select: TermPattern.Select => scanSelect(select)
        case TermPattern.Apply(function: TermPattern.Select, arguments) =>
          Option(arguments).toRight(InvalidSelectedNameOccurrence).flatMap {
            case Nil => scanSelect(function)
            case argument :: Nil =>
              for
                functionScan <- scanSelect(function)
                argumentScan <- scanSimple(argument)
              yield combine(functionScan, argumentScan)
            case _ => Left(UnsupportedPatternShape)
          }
        case _ => Left(UnsupportedPatternShape)

    private def scanSelect(pattern: TermPattern.Select): Either[Failure, Scan] =
      for
        name <- Option(pattern.name).filter(_.nonEmpty).toRight(
          InvalidSelectedNameOccurrence
        )
        qualifier <- Option(pattern.qualifier).toRight(
          InvalidSelectedNameOccurrence
        )
        qualifierScan <- qualifier match
          case nested: TermPattern.Select => scanSelect(nested)
          case simple                     => scanSimple(simple)
      yield Scan(name +: qualifierScan.selectNames, qualifierScan.ordinaryHoleNames)

    private def scanSimple(pattern: TermPattern): Either[Failure, Scan] =
      Option(pattern).toRight(InvalidSelectedNameOccurrence).flatMap {
        case TermPattern.Hole(name) if isHoleName(name) =>
          Right(Scan(Vector.empty, Set(name)))
        case TermPattern.Identifier(name) if name != null && name.nonEmpty =>
          Right(Scan(Vector.empty, Set.empty))
        case TermPattern.Literal(value) if value != null =>
          Right(Scan(Vector.empty, Set.empty))
        case _ => Left(UnsupportedPatternShape)
      }

    private def combine(left: Scan, right: Scan): Scan =
      Scan(
        left.selectNames ++ right.selectNames,
        left.ordinaryHoleNames ++ right.ordinaryHoleNames
      )

    private def key(
        pattern: TermPattern,
        byOrdinal: Map[Int, Occurrence],
        nextSelectOrdinal: Int
    ): (SemanticKey, Int) =
      pattern match
        case TermPattern.Hole(name) =>
          (SemanticKey.Hole(name), nextSelectOrdinal)
        case TermPattern.Identifier(name) =>
          (SemanticKey.Identifier(name), nextSelectOrdinal)
        case TermPattern.Literal(value) =>
          (SemanticKey.Literal(value), nextSelectOrdinal)
        case TermPattern.Select(qualifier, name) =>
          val ordinal = nextSelectOrdinal
          val selectedKey = byOrdinal.get(ordinal) match
            case Some(occurrence) => SelectedKey.Captured(occurrence.name)
            case None             => SelectedKey.Fixed(name)
          val (qualifierKey, afterQualifier) =
            key(qualifier, byOrdinal, ordinal + 1)
          (SemanticKey.Select(qualifierKey, selectedKey), afterQualifier)
        case TermPattern.Apply(function, arguments) =>
          val (functionKey, afterFunction) =
            key(function, byOrdinal, nextSelectOrdinal)
          val (argumentKeys, afterArguments) = arguments.foldLeft(
            (Vector.empty[SemanticKey], afterFunction)
          ) { case ((keys, ordinal), argument) =>
            val (argumentKey, afterArgument) = key(argument, byOrdinal, ordinal)
            (keys :+ argumentKey, afterArgument)
          }
          (SemanticKey.Apply(functionKey, argumentKeys), afterArguments)
        case _ => throw new IllegalStateException("validated shape escaped envelope")

  private object Matcher:
    import Failure.*

    def matchTarget(
        pattern: Validated,
        target: TermShape
    ): Either[Failure, NeutralSelectedNameMatchResult] =
      val byOrdinal = pattern.occurrences.map(value => value.selectOrdinal -> value).toMap
      for
        state <- loop(
          pattern.pattern,
          target,
          byOrdinal,
          MatchState(Map.empty, Map.empty, 0)
        )
        _ <- pattern.occurrences
          .map(_.name)
          .distinct
          .foldLeft[Either[Failure, Unit]](Right(())) { case (result, name) =>
            result.flatMap(_ =>
              Either.cond(
                state.selectedNameBindings.contains(name),
                (),
                MissingSelectedNameCapture(name)
              )
            )
          }
      yield NeutralSelectedNameMatchResult(
        state.termBindings,
        state.selectedNameBindings
      )

    private def loop(
        pattern: TermPattern,
        target: TermShape,
        byOrdinal: Map[Int, Occurrence],
        state: MatchState
    ): Either[Failure, MatchState] =
      (pattern, target) match
        case (TermPattern.Hole(name), shape) =>
          state.termBindings.get(name) match
            case None =>
              Right(state.copy(termBindings = state.termBindings.updated(name, shape)))
            case Some(previous) if previous == shape => Right(state)
            case Some(_)                             => Left(RepeatedTermCaptureMismatch(name))
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
          byOrdinal.get(ordinal) match
            case Some(occurrence) =>
              if patternName != occurrence.transport then
                Left(InvalidSelectedNameOccurrence)
              else
                selectedInNeutralIntersection(targetName).flatMap { selectedName =>
                  afterSelect.selectedNameBindings.get(occurrence.name) match
                    case Some(previous) if previous != selectedName =>
                      Left(RepeatedSelectedNameCaptureMismatch(occurrence.name))
                    case _ =>
                      loop(
                        patternQualifier,
                        targetQualifier,
                        byOrdinal,
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
              loop(patternQualifier, targetQualifier, byOrdinal, afterSelect)
            case None => Left(ShapeMismatch)
        case (
              TermPattern.Apply(patternFunction, patternArguments),
              TermShape.Apply(targetFunction, targetArguments)
            ) if patternArguments.size == targetArguments.size =>
          loop(patternFunction, targetFunction, byOrdinal, state).flatMap {
            afterFunction =>
              patternArguments
                .zip(targetArguments)
                .foldLeft[Either[Failure, MatchState]](Right(afterFunction)) {
                  case (result, (nextPattern, nextTarget)) =>
                    result.flatMap(loop(nextPattern, nextTarget, byOrdinal, _))
                }
          }
        case _ => Left(ShapeMismatch)

  private val firstTransport = "__n064_first"
  private val secondTransport = "__n064_second"

  test("two distinct captures fit Vector metadata and the existing typed result"):
    val pattern = twoCaptureSelect(firstTransport, secondTransport)
    val validated = validate(
      pattern,
      Vector(
        Occurrence("first", 1, firstTransport),
        Occurrence("second", 0, secondTransport)
      )
    )
    val result = matched(validated, twoSelectedTarget("service", "alpha", "beta"))

    assertEquals(result.termBindings, Map("receiver" -> ident("service")))
    assertEquals(
      result.selectedNameBindings,
      Map("first" -> selected("alpha"), "second" -> selected("beta"))
    )
    assertEquals(
      orderedSelectedBindings(validated, result),
      Vector("second" -> selected("beta"), "first" -> selected("alpha"))
    )

  test("repeated same-logical-name captures store once then require exact equality"):
    val validated = validate(
      twoCaptureSelect(firstTransport, secondTransport),
      Vector(
        Occurrence("member", 0, secondTransport),
        Occurrence("member", 1, firstTransport)
      )
    )

    assertEquals(
      matched(validated, twoSelectedTarget("service", "ordinary", "ordinary"))
        .selectedNameBindings,
      Map("member" -> selected("ordinary"))
    )
    assertEquals(
      Matcher.matchTarget(
        validated,
        twoSelectedTarget("service", "firstName", "secondName")
      ),
      Left(Failure.RepeatedSelectedNameCaptureMismatch("member"))
    )

  test("validation requires unique addresses transports and capture categories"):
    val pattern = twoCaptureSelect(firstTransport, secondTransport)

    assertEquals(
      Validated.create(
        pattern,
        Vector(
          Occurrence("first", 0, secondTransport),
          Occurrence("second", 0, firstTransport)
        )
      ).left.toOption,
      Some(Failure.DuplicateSelectedNameOccurrenceAddress(0))
    )
    assertEquals(
      Validated.create(
        pattern,
        Vector(
          Occurrence("first", 1, firstTransport),
          Occurrence("second", 0, firstTransport)
        )
      ).left.toOption,
      Some(Failure.SelectedNameTransportConflict(firstTransport))
    )
    assertEquals(
      Validated.create(
        twoCaptureSelect(firstTransport, firstTransport),
        Vector(
          Occurrence("first", 0, firstTransport),
          Occurrence("second", 1, secondTransport)
        )
      ).left.toOption,
      Some(Failure.SelectedNameTransportConflict(firstTransport))
    )

    val categoryCollision = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("first"), firstTransport),
      secondTransport
    )
    assertEquals(
      Validated.create(
        categoryCollision,
        Vector(
          Occurrence("first", 1, firstTransport),
          Occurrence("second", 0, secondTransport)
        )
      ).left.toOption,
      Some(Failure.SelectedNameCaptureCategoryConflict("first"))
    )
    assertEquals(
      Validated.create(
        pattern,
        Vector(
          Occurrence("first", 1, "__missing"),
          Occurrence("second", 0, secondTransport)
        )
      ).left.toOption,
      Some(Failure.InvalidSelectedNameOccurrence)
    )

  test("fixed names coexist with two captures under exact root-first ordinals"):
    val pattern = TermPattern.Select(
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), firstTransport),
        "fixed"
      ),
      secondTransport
    )
    val validated = validate(
      pattern,
      Vector(
        Occurrence("second", 0, secondTransport),
        Occurrence("first", 2, firstTransport)
      )
    )
    val target = TermShape.Select(
      TermShape.Select(
        TermShape.Select(ident("service"), "alpha"),
        "fixed"
      ),
      "beta"
    )

    assertEquals(
      matched(validated, target).selectedNameBindings,
      Map("first" -> selected("alpha"), "second" -> selected("beta"))
    )
    val wrongFixed = TermShape.Select(
      TermShape.Select(
        TermShape.Select(ident("service"), "alpha"),
        "Fixed"
      ),
      "beta"
    )
    assertEquals(
      Matcher.matchTarget(validated, wrongFixed).left.toOption,
      Some(Failure.ShapeMismatch)
    )

  test("direct nullary and unary envelopes fit without arbitrary Apply support"):
    val select = twoCaptureSelect(firstTransport, secondTransport)
    val occurrences = Vector(
      Occurrence("second", 0, secondTransport),
      Occurrence("first", 1, firstTransport)
    )
    val direct = validate(select, occurrences)
    val nullary = validate(TermPattern.Apply(select, Nil), occurrences)
    val unary = validate(
      TermPattern.Apply(select, List(TermPattern.Hole("argument"))),
      occurrences
    )
    val selectedTarget = twoSelectedTarget("service", "alpha", "beta")

    assert(Matcher.matchTarget(direct, selectedTarget).isRight)
    assert(
      Matcher.matchTarget(nullary, TermShape.Apply(selectedTarget, Nil)).isRight
    )
    assertEquals(
      matched(
        unary,
        TermShape.Apply(selectedTarget, List(TermShape.Literal("1")))
      ).termBindings("argument"),
      TermShape.Literal("1")
    )

    Vector[TermPattern](
      TermPattern.Apply(
        select,
        List(TermPattern.Hole("a"), TermPattern.Hole("b"))
      ),
      TermPattern.Apply(
        TermPattern.Apply(select, Nil),
        List(TermPattern.Hole("argument"))
      )
    ).foreach { unsupported =>
      assertEquals(
        Validated.create(unsupported, occurrences).left.toOption,
        Some(Failure.UnsupportedPatternShape)
      )
    }

  test("each captured position retains the current neutral lexical boundary"):
    val validated = validate(
      twoCaptureSelect(firstTransport, secondTransport),
      Vector(
        Occurrence("second", 0, secondTransport),
        Occurrence("first", 1, firstTransport)
      )
    )

    Vector("ordinary", "member2", "_privateLike").foreach { name =>
      assert(
        Matcher
          .matchTarget(validated, twoSelectedTarget("service", name, name))
          .isRight
      )
    }

    Vector("+", "type", "safe spaced name", "_", "naïve", "$internal")
      .foreach { name =>
        Vector(
          twoSelectedTarget("service", name, "ordinary"),
          twoSelectedTarget("service", "ordinary", name)
        ).foreach { target =>
          assertEquals(
            Matcher.matchTarget(validated, target).left.toOption,
            Some(Failure.SelectedNameLexicalUnsupported(name))
          )
        }
      }

    val invalidFixed = TermPattern.Select(
      twoCaptureSelect(firstTransport, secondTransport),
      "+"
    )
    assertEquals(
      Validated.create(
        invalidFixed,
        Vector(
          Occurrence("second", 1, secondTransport),
          Occurrence("first", 2, firstTransport)
        )
      ).left.toOption,
      Some(Failure.SelectedNameLexicalUnsupported("+"))
    )

  test("semantic equality ignores transports but observes logical names and positions"):
    val left = validate(
      twoCaptureSelect("__left_first", "__left_second"),
      Vector(
        Occurrence("second", 0, "__left_second"),
        Occurrence("first", 1, "__left_first")
      )
    )
    val renamed = validate(
      twoCaptureSelect("__right_first", "__right_second"),
      Vector(
        Occurrence("first", 1, "__right_first"),
        Occurrence("second", 0, "__right_second")
      )
    )
    val changedLogicalName = validate(
      twoCaptureSelect("__third_first", "__third_second"),
      Vector(
        Occurrence("second", 0, "__third_second"),
        Occurrence("different", 1, "__third_first")
      )
    )
    val swappedPositions = validate(
      twoCaptureSelect("__fourth_first", "__fourth_second"),
      Vector(
        Occurrence("first", 0, "__fourth_second"),
        Occurrence("second", 1, "__fourth_first")
      )
    )

    assertEquals(left, renamed)
    assertEquals(left.hashCode, renamed.hashCode)
    assertNotEquals(left, changedLogicalName)
    assertNotEquals(left, swappedPositions)

  test("programmatic multi-capture stays independent of source and public result APIs"):
    val mapped = PatternSource.synthesizeMapped("$receiver.fixed").toOption.get
    val publicResult = MatchResult[TermShape](Map("receiver" -> ident("service")))
    val privateResult = matched(
      validate(
        twoCaptureSelect(firstTransport, secondTransport),
        Vector(
          Occurrence("second", 0, secondTransport),
          Occurrence("first", 1, firstTransport)
        )
      ),
      twoSelectedTarget("service", "alpha", "beta")
    )

    assertEquals(mapped.patternSource.holes, Vector("receiver"))
    assertEquals(publicResult.binding("$receiver"), Some(ident("service")))
    assertEquals(privateResult.selectedNameBindings.keySet, Set("first", "second"))
    assert(
      typeCheckErrors(
        """val result = quasiquotes.matching.MatchResult[quasiquotes.parser.TermShape](Map("member" -> quasiquotes.construct.SelectedMemberName.from("ordinary").toOption.get))"""
      ).nonEmpty
    )

  private def validate(
      pattern: TermPattern,
      occurrences: Vector[Occurrence]
  ): Validated =
    Validated.create(pattern, occurrences).fold(error => fail(error.toString), identity)

  private def matched(
      pattern: Validated,
      target: TermShape
  ): NeutralSelectedNameMatchResult =
    Matcher.matchTarget(pattern, target).fold(error => fail(error.toString), identity)

  private def orderedSelectedBindings(
      pattern: Validated,
      result: NeutralSelectedNameMatchResult
  ): Vector[(String, SelectedMemberName)] =
    pattern.occurrences.map(occurrence =>
      occurrence.name -> result.selectedNameBindings(occurrence.name)
    )

  private def twoCaptureSelect(
      first: String,
      second: String
  ): TermPattern.Select =
    TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), first),
      second
    )

  private def twoSelectedTarget(
      receiver: String,
      first: String,
      second: String
  ): TermShape.Select =
    TermShape.Select(
      TermShape.Select(ident(receiver), first),
      second
    )

  private def selectedInNeutralIntersection(
      decoded: String
  ): Either[Failure, SelectedMemberName] =
    SelectedMemberName
      .from(decoded)
      .left
      .map(_ => Failure.SelectedNameLexicalUnsupported(decoded))
      .flatMap { selectedName =>
        Either.cond(
          DefinitionName.plain(selectedName.decoded).isRight,
          selectedName,
          Failure.SelectedNameLexicalUnsupported(decoded)
        )
      }

  private def inNeutralIntersection(decoded: String): Boolean =
    selectedInNeutralIntersection(decoded).isRight

  private def isHoleName(name: String): Boolean =
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

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName.from(name).fold(error => fail(error.message), identity)
