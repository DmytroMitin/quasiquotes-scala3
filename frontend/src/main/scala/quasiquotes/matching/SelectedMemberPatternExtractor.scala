package quasiquotes.matching

import scala.compiletime.erasedValue
import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

private[quasiquotes] enum SelectedMemberPatternLayout:
  case Direct, Nullary, Unary

private[quasiquotes] final case class SelectedMemberPatternPlan(
    layout: SelectedMemberPatternLayout,
    effectiveSource: String,
    holeNames: Vector[String],
    selectedNamePlaceholder: String,
    sequenceIndex: Option[Int]
):
  def sequenceHoleName: Option[String] = sequenceIndex.map(holeNames)

private[quasiquotes] object SelectedMemberPatternSource:
  private val PlaceholderBase = "__qq_selected_name_placeholder"

  def classify(parts: List[String]): Either[String, Option[SelectedMemberPatternPlan]] =
    classify(parts, allowRanked = false)

  def classifyIncludingRanked(
      parts: List[String]
  ): Either[String, Option[SelectedMemberPatternPlan]] =
    classify(parts, allowRanked = true)

  private def classify(
      parts: List[String],
      allowRanked: Boolean
  ): Either[String, Option[SelectedMemberPatternPlan]] =
    if !isDirectRootSelectedCandidate(parts) then Right(None)
    else
      RankedPatternSource.classify(parts).flatMap { ranked =>
        if ranked.sequenceIndex.nonEmpty then
          if allowRanked then Right(Some(rankedPlan(parts, ranked)))
          else
            Left(
              "selected-name capture cannot be combined with rank-2 capture in the current qq slice"
            )
        else
          selectedLayout(parts) match
            case None =>
              Left(
                "selected-name capture is supported only as $receiver.$selectedName, " +
                  "$receiver.$selectedName(), or $receiver.$selectedName($argument)"
              )
            case Some(layout) => Right(Some(plan(parts, ranked.holeNames, layout)))
      }

  private def isDirectRootSelectedCandidate(parts: List[String]): Boolean =
    parts match
      case "" :: "." :: _ => true
      case _ => false

  private def selectedLayout(parts: List[String]): Option[SelectedMemberPatternLayout] =
    parts match
      case List("", ".", "") => Some(SelectedMemberPatternLayout.Direct)
      case List("", ".", "()") => Some(SelectedMemberPatternLayout.Nullary)
      case List("", ".", "(", ")") => Some(SelectedMemberPatternLayout.Unary)
      case _ => None

  private def plan(
      parts: List[String],
      holeNames: Vector[String],
      layout: SelectedMemberPatternLayout
  ): SelectedMemberPatternPlan =
    val placeholder = collisionSafePlaceholder(parts, holeNames)
    val receiver = "$" + holeNames(0)
    val source = layout match
      case SelectedMemberPatternLayout.Direct => s"$receiver.$placeholder"
      case SelectedMemberPatternLayout.Nullary => s"$receiver.$placeholder()"
      case SelectedMemberPatternLayout.Unary =>
        val argument = "$" + holeNames(2)
        s"$receiver.$placeholder($argument)"

    SelectedMemberPatternPlan(layout, source, holeNames, placeholder, None)

  private def rankedPlan(
      parts: List[String],
      ranked: RankedTemplate
  ): SelectedMemberPatternPlan =
    val placeholder = collisionSafePlaceholder(parts, ranked.holeNames)
    val source = ranked.effectiveParts.zipWithIndex.foldLeft(new StringBuilder) {
      case (builder, (part, index)) =>
        builder.append(part)
        ranked.holeNames.lift(index).foreach { holeName =>
          if index == 1 then builder.append(placeholder)
          else builder.append('$').append(holeName)
        }
        builder
    }.toString

    SelectedMemberPatternPlan(
      SelectedMemberPatternLayout.Unary,
      source,
      ranked.holeNames,
      placeholder,
      ranked.sequenceIndex
    )

  private def collisionSafePlaceholder(
      parts: List[String],
      holeNames: Vector[String]
  ): String =
    Iterator
      .from(0)
      .map(index =>
        if index == 0 then PlaceholderBase
        else PlaceholderBase + "_" + index
      )
      .find(candidate =>
        !holeNames.contains(candidate) && !parts.exists(part =>
          part != null && part.contains(candidate)
        )
      )
      .get

/** Matcher bridge for a selector which already compiled an ordinary placeholder
  * pattern and retained the direct-root selected-name role as a sidecar.
  */
private[quasiquotes] object SelectedMemberPatternBridge:
  def validatePlan(
      plan: SelectedMemberPatternPlan,
      pattern: TermPattern
  ): Either[String, Unit] =
    plan.sequenceIndex match
      case Some(sequenceIndex) => validateRankedPlan(plan, pattern, sequenceIndex)
      case None => validateSelectedOnlyPlan(plan, pattern)

  private def validateSelectedOnlyPlan(
      plan: SelectedMemberPatternPlan,
      pattern: TermPattern
  ): Either[String, Unit] =
    val receiverName = plan.holeNames.headOption
    val argumentName = plan.holeNames.lift(2)
    val valid = (plan.layout, pattern) match
      case (
            SelectedMemberPatternLayout.Direct,
            TermPattern.Select(TermPattern.Hole(receiver), selected)
          ) =>
        receiverName.contains(receiver) && selected == plan.selectedNamePlaceholder
      case (
            SelectedMemberPatternLayout.Nullary,
            TermPattern.Apply(
              TermPattern.Select(TermPattern.Hole(receiver), selected),
              Nil
            )
          ) =>
        receiverName.contains(receiver) && selected == plan.selectedNamePlaceholder
      case (
            SelectedMemberPatternLayout.Unary,
            TermPattern.Apply(
              TermPattern.Select(TermPattern.Hole(receiver), selected),
              TermPattern.Hole(argument) :: Nil
            )
          ) =>
        receiverName.contains(receiver) &&
          argumentName.contains(argument) &&
          selected == plan.selectedNamePlaceholder
      case _ => false

    Either.cond(
      valid,
      (),
      "compiled selected-name pattern topology does not match " +
        plan.layout.toString.toLowerCase +
        " source role"
    )

  private def validateRankedPlan(
      plan: SelectedMemberPatternPlan,
      pattern: TermPattern,
      sequenceIndex: Int
  ): Either[String, Unit] =
    val expectedArguments = plan.holeNames.drop(2).map(TermPattern.Hole.apply).toList
    val valid = pattern match
      case TermPattern.Apply(
            TermPattern.Select(TermPattern.Hole(receiver), selected),
            arguments
          ) =>
        plan.holeNames.headOption.contains(receiver) &&
          selected == plan.selectedNamePlaceholder &&
          sequenceIndex >= 2 &&
          sequenceIndex < plan.holeNames.size &&
          arguments == expectedArguments
      case _ => false

    Either.cond(
      valid,
      (),
      "compiled mixed selected-name pattern must be a direct root Apply with " +
        "the rank-2 capture as one direct ordinary Apply argument"
    )

  def matchSelected(using q: Quotes)(
      pattern: TermPattern,
      selectedNamePlaceholder: String,
      target: q.reflect.Term
  ): Option[(MatchResult[q.reflect.Term], SelectedMemberName)] =
    if target == null then None
    else
      for
        targetView <- TargetTermView.fromTerm(target).toOption
        decoded <- rootSelectedName(pattern, selectedNamePlaceholder, targetView)
        selected <- SelectedMemberName.from(decoded).toOption
        concretePattern <- replaceRootSelectedName(pattern, selectedNamePlaceholder, decoded)
        result <- TermMatcher.matchTermRaw(concretePattern, target).toOption
      yield (result, selected)

  def matchSelectedRanked(using q: Quotes)(
      pattern: TermPattern,
      selectedNamePlaceholder: String,
      sequenceHoleName: String,
      target: q.reflect.Term
  ): Option[SelectedRankedTermMatch[q.reflect.Term]] =
    if target == null then None
    else
      for
        targetView <- TargetTermView.fromTerm(target).toOption
        decoded <- rootSelectedName(pattern, selectedNamePlaceholder, targetView)
        selected <- SelectedMemberName.from(decoded).toOption
        concretePattern <- replaceRootSelectedName(pattern, selectedNamePlaceholder, decoded)
        result <- TermMatcher.matchTermRanked(concretePattern, sequenceHoleName, target).toOption
      yield SelectedRankedTermMatch(
        result.scalarBindings,
        result.sequenceBindings,
        selected
      )

  private def rootSelectedName[T](
      pattern: TermPattern,
      selectedNamePlaceholder: String,
      target: TargetTermView[T]
  ): Option[String] =
    (pattern, target) match
      case (
            TermPattern.Select(_, patternName),
            TargetTermView.Select(_, targetName, _)
          ) if patternName == selectedNamePlaceholder => Some(targetName)
      case (
            TermPattern.Apply(TermPattern.Select(_, patternName), _),
            TargetTermView.Apply(TargetTermView.Select(_, targetName, _), _, _)
          ) if patternName == selectedNamePlaceholder => Some(targetName)
      case _ => None

  private def replaceRootSelectedName(
      pattern: TermPattern,
      selectedNamePlaceholder: String,
      decoded: String
  ): Option[TermPattern] =
    pattern match
      case TermPattern.Select(qualifier, patternName)
          if patternName == selectedNamePlaceholder =>
        Some(TermPattern.Select(qualifier, decoded))
      case TermPattern.Apply(
            TermPattern.Select(qualifier, patternName),
            arguments
          ) if patternName == selectedNamePlaceholder =>
        Some(TermPattern.Apply(TermPattern.Select(qualifier, decoded), arguments))
      case _ => None

private[matching] final case class SelectedRankedTermMatch[T](
    scalarBindings: Map[String, T],
    sequenceBindings: Map[String, Seq[T]],
    selectedName: SelectedMemberName
)

private[quasiquotes] sealed trait ProductCaptureKind
private[quasiquotes] sealed trait ProductTermCapture extends ProductCaptureKind
private[quasiquotes] sealed trait ProductSelectedNameCapture extends ProductCaptureKind
private[quasiquotes] sealed trait ProductSequenceTermCapture extends ProductCaptureKind

private[quasiquotes] type ProductCaptureTypes[T, Kinds <: Tuple] <: Tuple = Kinds match
  case EmptyTuple => EmptyTuple
  case ProductTermCapture *: tail => T *: ProductCaptureTypes[T, tail]
  case ProductSelectedNameCapture *: tail => SelectedMemberName *: ProductCaptureTypes[T, tail]
  case ProductSequenceTermCapture *: tail => Seq[T] *: ProductCaptureTypes[T, tail]

private[quasiquotes] object TermPatternProductExtractorFactory:
  transparent inline def mixed[Kinds <: Tuple](using q: Quotes)(
      pattern: TermPattern,
      holeNames: Vector[String],
      selectedNamePlaceholder: String,
      sequenceHoleName: String
  ): TermPatternProductExtractor[
    q.reflect.Term,
    ProductCaptureTypes[q.reflect.Term, Kinds]
  ] =
    productExtractor(target =>
      SelectedMemberPatternBridge
        .matchSelectedRanked(
          pattern,
          selectedNamePlaceholder,
          sequenceHoleName,
          target
        )
        .map(result => captureTuple[q.reflect.Term, Kinds](result, holeNames, 0))
    )

  private[quasiquotes] def productExtractor[T, Captures <: Tuple](
      extract: T => Option[Captures]
  ): TermPatternProductExtractor[T, Captures] =
    new TermPatternProductExtractor(extract)

  def direct(using q: Quotes)(
      pattern: TermPattern,
      holeNames: Vector[String],
      selectedNamePlaceholder: String
  ): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName)
  ] =
    new TermPatternProductExtractor(target =>
      SelectedMemberPatternBridge
        .matchSelected(pattern, selectedNamePlaceholder, target)
        .flatMap { case (result, selectedName) =>
          result.bindings.get(holeNames(0)).map(receiver => (receiver, selectedName))
        }
    )

  def nullary(using q: Quotes)(
      pattern: TermPattern,
      holeNames: Vector[String],
      selectedNamePlaceholder: String
  ): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName)
  ] = direct(pattern, holeNames, selectedNamePlaceholder)

  def unary(using q: Quotes)(
      pattern: TermPattern,
      holeNames: Vector[String],
      selectedNamePlaceholder: String
  ): TermPatternProductExtractor[
    q.reflect.Term,
    (q.reflect.Term, SelectedMemberName, q.reflect.Term)
  ] =
    new TermPatternProductExtractor(target =>
      SelectedMemberPatternBridge
        .matchSelected(pattern, selectedNamePlaceholder, target)
        .flatMap { case (result, selectedName) =>
          for
            receiver <- result.bindings.get(holeNames(0))
            argument <- result.bindings.get(holeNames(2))
          yield (receiver, selectedName, argument)
        }
    )

  private inline def captureTuple[T, Kinds <: Tuple](
      result: SelectedRankedTermMatch[T],
      holeNames: Vector[String],
      index: Int
  ): ProductCaptureTypes[T, Kinds] =
    inline erasedValue[Kinds] match
      case _: EmptyTuple => EmptyTuple
      case _: (ProductTermCapture *: tail) =>
        result.scalarBindings(holeNames(index)) *:
          captureTuple[T, tail](result, holeNames, index + 1)
      case _: (ProductSelectedNameCapture *: tail) =>
        result.selectedName *: captureTuple[T, tail](result, holeNames, index + 1)
      case _: (ProductSequenceTermCapture *: tail) =>
        result.sequenceBindings(holeNames(index)) *:
          captureTuple[T, tail](result, holeNames, index + 1)
