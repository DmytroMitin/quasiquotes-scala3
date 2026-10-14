package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName

object Q070ReflectedBridgeTarget:
  object directReceiver:
    object inner:
      val outer: Int = 42

  object unaryReceiver:
    object inner:
      def outer(value: Int): Int = value + 1

  object repeatedReceiver:
    object same:
      val same: Int = 42

  object unequalReceiver:
    object inner:
      val outer: Int = 42

  object wrongArityReceiver:
    object inner:
      def outer(first: Int, second: Int): Int = first + second

  object invalidNameReceiver:
    object inner:
      val naïve: Int = 42

/** Test-only Q070 source-role plan and reflected-target bridge. */
object Q070ReflectedBridgeProbe:
  private enum Layout:
    case Direct, Unary

  private final case class Plan(
      source: String,
      parts: List[String],
      layout: Layout,
      pattern: TermPattern,
      holeNames: Vector[String],
      selectedOccurrences: Vector[SelectedNamePatternOccurrence],
      transportsExcludedFromOrdinaryIndex: Boolean
  )

  private final case class ReflectedMatch[T](
      termBindings: Map[String, T],
      selectedBindings: Map[String, SelectedMemberName]
  )

  inline def distinctDirectObservation: String = ${ distinctDirectObservationImpl }
  inline def distinctUnaryObservation: String = ${ distinctUnaryObservationImpl }
  inline def repeatedObservation: String = ${ repeatedObservationImpl }
  inline def negativeObservation: String = ${ negativeObservationImpl }

  def sourcePlanObservation: String =
    val parts = StringContext("", ".", ".", "(", ")").parts.toList
    val plan = standardPlan("$receiver.$first.$second($argument)", parts, Layout.Unary)
      .fold(error => throw new AssertionError(error), identity)
    val sourceOrder = plan.holeNames.mkString(",")
    val selectedOrder = plan.selectedOccurrences
      .map(value => s"${value.name}:${value.selectOrdinal}")
      .mkString(",")
    val distinctTransports = plan.selectedOccurrences.map(_.transport).distinct.size == 2
    s"$sourceOrder|$selectedOrder|$distinctTransports|${plan.transportsExcludedFromOrdinaryIndex}"

  def lexicalObservation: String =
    Vector("+", "type", "safe spaced name")
      .flatMap(name => Vector(SelectedMemberName.from(name).isRight, DefinitionName.plain(name).isLeft))
      .mkString(":")

  private def distinctDirectObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val plan = standardPlan(
      "$receiver.$first.$second",
      StringContext("", ".", ".", "").parts.toList,
      Layout.Direct
    ).fold(error => report.errorAndAbort(error), identity)
    val target = stripInlined('{ Q070ReflectedBridgeTarget.directReceiver.inner.outer }.asTerm)
    val originalReceiver = target match
      case Select(Select(receiver, _), _) => receiver
      case other => report.errorAndAbort(s"Q070 direct fixture is not a nested Select: ${other.show}")

    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName)
    ] = TermPatternProductExtractorFactory.productExtractor { candidate =>
      matchTarget(plan, candidate).flatMap { result =>
        for
          receiver <- result.termBindings.get("receiver")
          first <- result.selectedBindings.get("first")
          second <- result.selectedBindings.get("second")
        yield (receiver, first, second)
      }
    }

    val (receiver, first, second) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q070 direct reflected bridge did not match")
    )
    Expr(s"${first.decoded}:${second.decoded}:${receiver eq originalReceiver}")

  private def distinctUnaryObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val plan = standardPlan(
      "$receiver.$first.$second($argument)",
      StringContext("", ".", ".", "(", ")").parts.toList,
      Layout.Unary
    ).fold(error => report.errorAndAbort(error), identity)
    val target = stripInlined('{ Q070ReflectedBridgeTarget.unaryReceiver.inner.outer(41) }.asTerm)
    val (originalReceiver, originalArgument) = target match
      case Apply(Select(Select(receiver, _), _), argument :: Nil) => (receiver, argument)
      case other => report.errorAndAbort(s"Q070 unary fixture has the wrong shape: ${other.show}")

    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)
    ] = TermPatternProductExtractorFactory.productExtractor { candidate =>
      matchTarget(plan, candidate).flatMap { result =>
        for
          receiver <- result.termBindings.get("receiver")
          first <- result.selectedBindings.get("first")
          second <- result.selectedBindings.get("second")
          argument <- result.termBindings.get("argument")
        yield (receiver, first, second, argument)
      }
    }

    val (receiver, first, second, argument) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q070 unary reflected bridge did not match")
    )
    Expr(
      s"${first.decoded}:${second.decoded}:${receiver eq originalReceiver}:${argument eq originalArgument}"
    )

  private def repeatedObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val plan = standardPlan(
      "$receiver.$same.$same",
      StringContext("", ".", ".", "").parts.toList,
      Layout.Direct
    ).fold(error => report.errorAndAbort(error), identity)
    val target = stripInlined('{ Q070ReflectedBridgeTarget.repeatedReceiver.same.same }.asTerm)
    val originalReceiver = target match
      case Select(Select(receiver, _), _) => receiver
      case other => report.errorAndAbort(s"Q070 repeated fixture has the wrong shape: ${other.show}")

    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName)
    ] = TermPatternProductExtractorFactory.productExtractor { candidate =>
      matchTarget(plan, candidate).flatMap { result =>
        for
          receiver <- result.termBindings.get("receiver")
          same <- result.selectedBindings.get("same")
          if result.selectedBindings.size == 1
        yield (receiver, same)
      }
    }

    val (receiver, same) = extractor.unapply(target).getOrElse(
      report.errorAndAbort("Q070 repeated reflected bridge did not match")
    )
    Expr(s"${same.decoded}:${receiver eq originalReceiver}:${plan.selectedOccurrences.size == 2}")

  private def negativeObservationImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val distinct = standardPlan(
      "$receiver.$first.$second",
      StringContext("", ".", ".", "").parts.toList,
      Layout.Direct
    ).fold(error => report.errorAndAbort(error), identity)
    val unary = standardPlan(
      "$receiver.$first.$second($argument)",
      StringContext("", ".", ".", "(", ")").parts.toList,
      Layout.Unary
    ).fold(error => report.errorAndAbort(error), identity)
    val repeated = standardPlan(
      "$receiver.$same.$same",
      StringContext("", ".", ".", "").parts.toList,
      Layout.Direct
    ).fold(error => report.errorAndAbort(error), identity)

    val wrongTopology = stripInlined('{ Q070ReflectedBridgeTarget.directReceiver }.asTerm)
    val unequal = stripInlined('{ Q070ReflectedBridgeTarget.unequalReceiver.inner.outer }.asTerm)
    val wrongArity = stripInlined(
      '{ Q070ReflectedBridgeTarget.wrongArityReceiver.inner.outer(1, 2) }.asTerm
    )
    val invalidName = stripInlined(
      '{ Q070ReflectedBridgeTarget.invalidNameReceiver.inner.naïve }.asTerm
    )

    val normalized = SelectedNameSourceNormalizer
      .normalize(
        "$receiver.$first.$second",
        Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
      )
      .fold(error => report.errorAndAbort(error.message), identity)
    val swapped = ValidatedSelectedNamePattern.createTwo(
      distinct.pattern,
      normalized.selectedOccurrences(0).copy(selectOrdinal = 0),
      normalized.selectedOccurrences(1).copy(selectOrdinal = 1)
    ).isLeft
    val stale = ValidatedSelectedNamePattern.createTwo(
      distinct.pattern,
      normalized.selectedOccurrences(0).copy(selectOrdinal = 7),
      normalized.selectedOccurrences(1)
    ).isLeft
    val missing = SelectedNameSourceNormalizer.normalize(
      "$receiver.$first.$second",
      Vector(SelectedNameSourcePosition(1, 1))
    ).isLeft
    val missingReceiver = standardPlan(
      ".$first.$second",
      StringContext("", ".", ".", "").parts.toList,
      Layout.Direct
    ).isLeft
    val duplicate = SelectedNameSourceNormalizer.normalize(
      "$receiver.$first.$second",
      Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(1, 0))
    ).isLeft
    val collision = SelectedNameSourceNormalizer.normalize(
      "__qqhole_second__selected_0.$first.$second",
      Vector(SelectedNameSourcePosition(0, 1), SelectedNameSourcePosition(1, 0))
    ).toOption.exists(plan =>
      plan.selectedOccurrences.forall(_.transport != "__qqhole_second__selected_0")
    )
    val categoryConflict = SelectedNameSourceNormalizer.normalize(
      "$first.$first.$second",
      Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
    ).isLeft
    val rankAndP3Rejected =
      standardPlan(
        "$receiver.$first.$second(..$argument)",
        List("", ".", ".", "(..", ")"),
        Layout.Unary
      ).isLeft &&
        standardPlan(
          "{ def local(value: Int): Int = value; local($argument) }",
          List("{ def local(value: Int): Int = value; local(", ") }"),
          Layout.Unary
        ).isLeft
    val unsupportedParts = standardPlan(
      "$receiver + $first + $second",
      List("", " + ", " + ", ""),
      Layout.Direct
    ).isLeft

    val results = Vector(
      matchTarget(distinct, wrongTopology).isEmpty,
      swapped,
      stale,
      missing,
      missingReceiver,
      duplicate,
      matchTarget(repeated, unequal).isEmpty,
      collision,
      categoryConflict,
      matchTarget(unary, wrongArity).isEmpty,
      matchTarget(distinct, null.asInstanceOf[q.reflect.Term]).isEmpty,
      matchTarget(distinct, invalidName).isEmpty,
      rankAndP3Rejected,
      unsupportedParts
    )
    Expr(results.mkString(":"))

  private def standardPlan(
      source: String,
      parts: List[String],
      layout: Layout
  ): Either[String, Plan] =
    val expectedParts = layout match
      case Layout.Direct => List("", ".", ".", "")
      case Layout.Unary  => List("", ".", ".", "(", ")")
    if parts != expectedParts then Left("unsupported Q070 source parts")
    else
      for
        normalized <- SelectedNameSourceNormalizer
          .normalize(
            source,
            Vector(SelectedNameSourcePosition(1, 1), SelectedNameSourcePosition(2, 0))
          )
          .left
          .map(_.message)
        first = normalized.selectedOccurrences(0)
        second = normalized.selectedOccurrences(1)
        receiver <- normalized.occurrences.headOption.map(_.name).toRight("missing receiver")
        effectiveSource <- layout match
          case Layout.Direct =>
            Right(s"$$$receiver.${first.transport}.${second.transport}")
          case Layout.Unary =>
            normalized.occurrences
              .lift(3)
              .map(_.name)
              .toRight("missing unary argument")
              .map(argument =>
                s"$$$receiver.${first.transport}.${second.transport}($$$argument)"
              )
        pattern <- QuasiPattern.term(effectiveSource).left.map(_.message).map(_.pattern)
        _ <- ValidatedSelectedNamePattern
          .createTwo(pattern, first, second)
          .left
          .map(_.message)
      yield Plan(
        source,
        parts,
        layout,
        pattern,
        normalized.occurrences.map(_.name),
        normalized.selectedOccurrences,
        normalized.selectedOccurrences.forall(value =>
          normalized.ordinaryHoleIndex.semanticNameFor(value.transport).isEmpty
        )
      )

  private def matchTarget(using q: Quotes)(
      plan: Plan,
      target: q.reflect.Term
  ): Option[ReflectedMatch[q.reflect.Term]] =
    import q.reflect.*

    if target == null then None
    else
      val prepared = stripInlined(target)
      val selectedNames = (plan.layout, prepared) match
        case (Layout.Direct, Select(Select(_, inner), outer)) => Some(Vector(outer, inner))
        case (
              Layout.Unary,
              Apply(Select(Select(_, inner), outer), _ :: Nil)
            ) => Some(Vector(outer, inner))
        case _ => None

      for
        names <- selectedNames
        selected <- selectedBindings(plan.selectedOccurrences, names)
        concrete <- replaceSelectedNames(plan.pattern, names)
        matched <- TermMatcher.matchTermRaw(concrete, prepared).toOption
      yield ReflectedMatch(matched.bindings, selected)

  private def selectedBindings(
      occurrences: Vector[SelectedNamePatternOccurrence],
      rootFirstNames: Vector[String]
  ): Option[Map[String, SelectedMemberName]] =
    occurrences.foldLeft(Option(Map.empty[String, SelectedMemberName])) {
      case (state, occurrence) =>
        for
          bindings <- state
          decoded <- rootFirstNames.lift(occurrence.selectOrdinal)
          selected <- SelectedMemberName.from(decoded).toOption
          updated <- bindings.get(occurrence.name) match
            case None => Some(bindings.updated(occurrence.name, selected))
            case Some(previous) if previous == selected => Some(bindings)
            case Some(_) => None
        yield updated
    }

  private def replaceSelectedNames(
      pattern: TermPattern,
      rootFirstNames: Vector[String]
  ): Option[TermPattern] =
    def loop(current: TermPattern, nextOrdinal: Int): Option[(TermPattern, Int)] =
      current match
        case TermPattern.Select(qualifier, _) =>
          for
            name <- rootFirstNames.lift(nextOrdinal)
            replaced <- loop(qualifier, nextOrdinal + 1)
          yield (TermPattern.Select(replaced._1, name), replaced._2)
        case TermPattern.Apply(function, arguments) =>
          loop(function, nextOrdinal).map { case (replaced, afterFunction) =>
            (TermPattern.Apply(replaced, arguments), afterFunction)
          }
        case other => Some(other -> nextOrdinal)

    loop(pattern, 0).collect {
      case (replaced, consumed) if consumed == rootFirstNames.size => replaced
    }

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
