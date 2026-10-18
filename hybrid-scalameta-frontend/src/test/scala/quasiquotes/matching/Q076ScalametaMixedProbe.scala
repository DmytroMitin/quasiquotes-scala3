package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName
import quasiquotes.scalameta.TermFrontend

object Q076ScalametaMixedTarget:
  object receiver:
    object first:
      def second(): Int = 0
      def second(value: Int): Int = value
      def second(first: Int, second: Int, third: Int): Int = first + second + third
      def curried(first: Int)(second: Int): Int = first + second

  object names:
    object inner:
      def `+`(first: Int, second: Int): Int = first + second
      def `type`(value: Int): Int = value
      def `safe spaced name`(value: Int): Int = value
      def `__qqhole_second__selected_0`(value: Int): Int = value
      def naïve(value: Int): Int = value

/** Q076 test-only Scalameta-primary composition of the accepted two-name
  * source roles with the accepted direct-Apply rank-2 matcher.
  */
private[matching] object Q076ScalametaMixedModel:
  val CandidateParts: List[String] = List("", ".", ".", "(..", ")")
  val LogicalSource: String = "$receiver.$first.$second(..$arguments)"
  val RankErasedSource: String = "$receiver.$first.$second($arguments)"

  final case class Plan(
      pattern: TermPattern,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence],
      receiverHoleName: String,
      sequenceHoleName: String,
      rankedHoleNames: Vector[String],
      sequenceIndex: Int,
      normalized: NormalizedSelectedNameSource,
      engine: TermFrontend.Engine,
      primaryFailure: Option[TermFrontend.Failure]
  )

  def plan(): Either[String, Plan] =
    for
      ranked <- RankedPatternSource.classify(CandidateParts)
      sequenceIndex <- ranked.sequenceIndex.toRight("candidate lost its rank-2 marker")
      _ <- Either.cond(
        ranked.holeNames.size == 4 && sequenceIndex == 3,
        (),
        s"unexpected ranked metadata: ${ranked.holeNames} / ${ranked.sequenceIndex}"
      )
      normalized <- SelectedNameSourceNormalizer
        .normalize(
          RankErasedSource,
          Vector(
            SelectedNameSourcePosition(sourceOccurrenceIndex = 1, selectOrdinal = 1),
            SelectedNameSourcePosition(sourceOccurrenceIndex = 2, selectOrdinal = 0)
          )
        )
        .left
        .map(_.message)
      selected = normalized.selectedOccurrences
      _ <- Either.cond(
        selected.size == 2 && selected.map(_.transport).distinct.size == 2,
        (),
        "candidate did not produce two distinct selected-name transports"
      )
      receiverHole = ranked.holeNames(0)
      sequenceHole = ranked.holeNames(sequenceIndex)
      effectiveSource =
        s"$$$receiverHole.${selected(0).transport}.${selected(1).transport}($$$sequenceHole)"
      compiled <- TermFrontend.compile(effectiveSource).left.map(_.message)
      _ <- Either.cond(
        compiled.engine == TermFrontend.Engine.Scalameta && compiled.primaryFailure.isEmpty,
        (),
        s"candidate was not Scalameta-primary: ${compiled.engine} / ${compiled.primaryFailure}"
      )
      _ <- RankedPatternSource.validateSequencePosition(compiled.pattern, sequenceHole)
      _ <- ValidatedSelectedNamePattern
        .createTwo(compiled.pattern, selected(0), selected(1))
        .left
        .map(_.message)
    yield Plan(
      compiled.pattern,
      selected,
      receiverHole,
      sequenceHole,
      ranked.holeNames,
      sequenceIndex,
      normalized,
      compiled.engine,
      compiled.primaryFailure
    )

  def matchTarget(using q: Quotes)(
      plan: Plan,
      target: q.reflect.Term
  ): Option[(q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])] =
    import q.reflect.*

    if target == null then None
    else
      val prepared = stripInlined(target)
      val rootFirstNames = TargetTermView.fromTerm(prepared).toOption.collect {
        case TargetTermView.Apply(
              TargetTermView.Select(
                TargetTermView.Select(_, inner, _),
                outer,
                _
              ),
              _,
              _
            ) => Vector(outer, inner)
      }

      for
        names <- rootFirstNames
        outer <- names.headOption
        inner <- names.lift(1)
        first <- SelectedMemberName.from(inner).toOption
        second <- SelectedMemberName.from(outer).toOption
        concrete <- replaceSelectedNames(plan.pattern, names)
        matched <- TermMatcher
          .matchTermRanked(concrete, plan.sequenceHoleName, prepared)
          .toOption
        receiver <- matched.scalarBindings.get(plan.receiverHoleName)
        arguments <- matched.sequenceBindings.get(plan.sequenceHoleName)
      yield (receiver, first, second, arguments)

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

object Q076ScalametaMixedProbe:
  inline def observations: List[String] = ${ observationsImpl }
  inline def lexicalObservations: List[String] = ${ lexicalObservationsImpl }
  inline def negativeObservations: List[String] = ${ negativeObservationsImpl }

  private def observationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q076ScalametaMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val extractor: TermPatternProductExtractor[
      q.reflect.Term,
      (q.reflect.Term, SelectedMemberName, SelectedMemberName, Seq[q.reflect.Term])
    ] = TermPatternProductExtractorFactory.productExtractor(target =>
      Q076ScalametaMixedModel.matchTarget(plan, target)
    )

    val fixtures = List(
      "zero" -> '{ Q076ScalametaMixedTarget.receiver.first.second() }.asTerm,
      "one" -> '{ Q076ScalametaMixedTarget.receiver.first.second(1) }.asTerm,
      "many" -> '{ Q076ScalametaMixedTarget.receiver.first.second(1, 2, 3) }.asTerm
    )

    val observations = fixtures.map { case (label, rawTarget) =>
      val target = stripInlined(rawTarget)
      val (originalReceiver, originalArguments) = target match
        case Apply(Select(Select(receiver, _), _), arguments) => (receiver, arguments)
        case other => report.errorAndAbort(s"Q076 fixture has wrong shape: ${other.show}")
      val (receiver, first, second, arguments) = extractor.unapply(target).getOrElse(
        report.errorAndAbort(s"Q076 Scalameta model failed for $label")
      )
      val exactIdentity =
        (receiver eq originalReceiver) &&
          arguments.size == originalArguments.size &&
          arguments.zip(originalArguments).forall { case (actual, expected) => actual eq expected }
      s"$label:${first.decoded}:${second.decoded}:${arguments.size}:$exactIdentity"
    }

    Expr.ofList(observations.map(Expr(_)))

  private def lexicalObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q076ScalametaMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val fixtures = List(
      "symbolic" -> '{ Q076ScalametaMixedTarget.names.inner.`+`(1, 2) }.asTerm,
      "keyword" -> '{ Q076ScalametaMixedTarget.names.inner.`type`(1) }.asTerm,
      "spaced" -> '{ Q076ScalametaMixedTarget.names.inner.`safe spaced name`(1) }.asTerm,
      "generated" -> '{ Q076ScalametaMixedTarget.names.inner.`__qqhole_second__selected_0`(1) }.asTerm,
      "unicode" -> '{ Q076ScalametaMixedTarget.names.inner.naïve(1) }.asTerm
    )
    val values = fixtures.map { case (label, rawTarget) =>
      val matched = Q076ScalametaMixedModel.matchTarget(plan, stripInlined(rawTarget))
      matched match
        case Some((_, first, second, _)) => s"$label:${first.decoded}:${second.decoded}"
        case None => s"$label:fallthrough"
    }
    Expr.ofList(values.map(Expr(_)))

  private def negativeObservationsImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val plan = Q076ScalametaMixedModel.plan().fold(report.errorAndAbort(_), identity)
    val nonApply = '{ Q076ScalametaMixedTarget.receiver.first }.asTerm
    val curried = '{ Q076ScalametaMixedTarget.receiver.first.curried(1)(2) }.asTerm
    val values = List(
      "non-apply:" + Q076ScalametaMixedModel.matchTarget(plan, stripInlined(nonApply)).isEmpty,
      "curried:" + Q076ScalametaMixedModel.matchTarget(plan, stripInlined(curried)).isEmpty,
      "null:" + Q076ScalametaMixedModel
        .matchTarget(plan, null.asInstanceOf[q.reflect.Term])
        .isEmpty
    )
    Expr.ofList(values.map(Expr(_)))

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
