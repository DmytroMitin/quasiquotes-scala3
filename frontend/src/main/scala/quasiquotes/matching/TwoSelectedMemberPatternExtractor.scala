package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

private[matching] enum TwoSelectedMemberPatternLayout:
  case Direct, Unary

private[matching] final case class TwoSelectedMemberPatternPlan(
    layout: TwoSelectedMemberPatternLayout,
    pattern: TermPattern,
    receiverName: String,
    argumentName: Option[String]
)

/** Standard-qq-only planner for the C077 distinct-binder two-name slice. */
private[matching] object TwoSelectedMemberPatternSource:
  private val SelectedPositions = Vector(
    SelectedNameSourcePosition(sourceOccurrenceIndex = 1, selectOrdinal = 1),
    SelectedNameSourcePosition(sourceOccurrenceIndex = 2, selectOrdinal = 0)
  )

  def classify(
      parts: List[String],
      source: => String
  ): Either[String, Option[TwoSelectedMemberPatternPlan]] =
    layout(parts) match
      case None => Right(None)
      case Some(selectedLayout) => plan(source, selectedLayout).map(Some.apply)

  private def layout(parts: List[String]): Option[TwoSelectedMemberPatternLayout] =
    parts match
      case List("", ".", ".", "") => Some(TwoSelectedMemberPatternLayout.Direct)
      case List("", ".", ".", "(", ")") => Some(TwoSelectedMemberPatternLayout.Unary)
      case _ => None

  private def plan(
      source: String,
      layout: TwoSelectedMemberPatternLayout
  ): Either[String, TwoSelectedMemberPatternPlan] =
    for
      normalized <- SelectedNameSourceNormalizer
        .normalize(source, SelectedPositions)
        .left
        .map(_.message)
      expectedCount = layout match
        case TwoSelectedMemberPatternLayout.Direct => 3
        case TwoSelectedMemberPatternLayout.Unary  => 4
      _ <- Either.cond(
        normalized.occurrences.size == expectedCount,
        (),
        s"two-selected-name ${layout.toString.toLowerCase} source has ${normalized.occurrences.size} capture occurrences instead of $expectedCount"
      )
      selected = normalized.selectedOccurrences
      _ <- Either.cond(
        selected.size == 2 &&
          selected.map(_.transport).distinct.size == 2 &&
          selected.forall(value =>
            normalized.ordinaryHoleIndex.semanticNameFor(value.transport).isEmpty
          ),
        (),
        "two-selected-name source roles do not have two independent selected transports"
      )
      receiver <- normalized.occurrences.headOption
        .map(_.name)
        .toRight("two-selected-name source is missing its receiver capture")
      argument = layout match
        case TwoSelectedMemberPatternLayout.Direct => Right(None)
        case TwoSelectedMemberPatternLayout.Unary =>
          normalized.occurrences
            .lift(3)
            .map(value => Some(value.name))
            .toRight("two-selected-name unary source is missing its argument capture")
      argumentName <- argument
      effectiveSource = layout match
        case TwoSelectedMemberPatternLayout.Direct =>
          s"$$$receiver.${selected(0).transport}.${selected(1).transport}"
        case TwoSelectedMemberPatternLayout.Unary =>
          s"$$$receiver.${selected(0).transport}.${selected(1).transport}($$${argumentName.get})"
      pattern <- QuasiPattern.term(effectiveSource).left.map(_.message).map(_.pattern)
      _ <- ValidatedSelectedNamePattern
        .createTwo(pattern, selected(0), selected(1))
        .left
        .map(_.message)
    yield TwoSelectedMemberPatternPlan(layout, pattern, receiver, argumentName)

private[matching] object TwoSelectedMemberPatternBridge:
  final case class Result[T](
      termBindings: Map[String, T],
      first: SelectedMemberName,
      second: SelectedMemberName
  )

  def matchTarget(using q: Quotes)(
      plan: TwoSelectedMemberPatternPlan,
      target: q.reflect.Term
  ): Option[Result[q.reflect.Term]] =
    import q.reflect.*

    if target == null then None
    else
      val prepared = stripInlined(target)
      val rootFirstNames = (plan.layout, prepared) match
        case (
              TwoSelectedMemberPatternLayout.Direct,
              Select(Select(_, inner), outer)
            ) => Some(Vector(outer, inner))
        case (
              TwoSelectedMemberPatternLayout.Unary,
              Apply(Select(Select(_, inner), outer), _ :: Nil)
            ) => Some(Vector(outer, inner))
        case _ => None

      for
        names <- rootFirstNames
        outer <- names.headOption
        inner <- names.lift(1)
        first <- SelectedMemberName.from(inner).toOption
        second <- SelectedMemberName.from(outer).toOption
        concrete <- replaceSelectedNames(plan.pattern, names)
        matched <- TermMatcher.matchTermRaw(concrete, prepared).toOption
      yield Result(matched.bindings, first, second)

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

private[matching] object TwoSelectedMemberPatternExtractorFactory:
  def direct(using q: Quotes)(
      plan: TwoSelectedMemberPatternPlan
  ): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, SelectedMemberName)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      TwoSelectedMemberPatternBridge.matchTarget(plan, target).flatMap { result =>
        result.termBindings
          .get(plan.receiverName)
          .map(receiver => (receiver, result.first, result.second))
      }
    )

  def unary(using q: Quotes)(
      plan: TwoSelectedMemberPatternPlan
  ): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, SelectedMemberName, q.reflect.Term)
  ] =
    TermPatternProductExtractorFactory.productExtractor(target =>
      TwoSelectedMemberPatternBridge.matchTarget(plan, target).flatMap { result =>
        for
          receiver <- result.termBindings.get(plan.receiverName)
          argumentName <- plan.argumentName
          argument <- result.termBindings.get(argumentName)
        yield (receiver, result.first, result.second, argument)
      }
    )
