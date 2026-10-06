package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

private[matching] enum SelectedMemberPatternLayout:
  case Direct, Nullary, Unary

private[matching] final case class SelectedMemberCompiledPattern(
    layout: SelectedMemberPatternLayout,
    pattern: TermPattern,
    holeNames: Vector[String],
    selectedNamePlaceholder: String
)

private[matching] object SelectedMemberPatternSource:
  private val PlaceholderBase = "__qq_selected_name_placeholder"

  def classify(parts: List[String]): Either[String, Option[SelectedMemberCompiledPattern]] =
    if !isDirectRootSelectedCandidate(parts) then Right(None)
    else
      RankedPatternSource.classify(parts).flatMap { ranked =>
        if ranked.sequenceIndex.nonEmpty then
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
            case Some(layout) => compile(parts, ranked.holeNames, layout).map(Some(_))
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

  private def compile(
      parts: List[String],
      holeNames: Vector[String],
      layout: SelectedMemberPatternLayout
  ): Either[String, SelectedMemberCompiledPattern] =
    val placeholder = collisionSafePlaceholder(parts, holeNames)
    val receiver = "$" + holeNames(0)
    val source = layout match
      case SelectedMemberPatternLayout.Direct => s"$receiver.$placeholder"
      case SelectedMemberPatternLayout.Nullary => s"$receiver.$placeholder()"
      case SelectedMemberPatternLayout.Unary =>
        val argument = "$" + holeNames(2)
        s"$receiver.$placeholder($argument)"

    QuasiPattern.term(source).left.map(_.message).map(pattern =>
      SelectedMemberCompiledPattern(layout, pattern.pattern, holeNames, placeholder)
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

private[matching] object TermPatternProductExtractorFactory:
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
