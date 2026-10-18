package quasiquotes.matching

import quasiquotes.scalameta.TermFrontend

/** Typed-Scalameta planner for the Q075 distinct-binder two-name slice. */
private[matching] object ScalametaTwoSelectedMemberPatternSource:
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
      effectiveSource <- layout match
        case TwoSelectedMemberPatternLayout.Direct =>
          Right(s"$$$receiver.${selected(0).transport}.${selected(1).transport}")
        case TwoSelectedMemberPatternLayout.Unary =>
          argumentName
            .map(name =>
              s"$$$receiver.${selected(0).transport}.${selected(1).transport}($$$name)"
            )
            .toRight("two-selected-name unary source lost its argument capture")
      compiled <- TermFrontend.compile(effectiveSource).left.map(_.message)
      _ <- Either.cond(
        compiled.engine == TermFrontend.Engine.Scalameta && compiled.primaryFailure.isEmpty,
        (),
        "two-selected-name source did not compile through the Scalameta-primary pattern frontend"
      )
      _ <- ValidatedSelectedNamePattern
        .createTwo(compiled.pattern, selected(0), selected(1))
        .left
        .map(_.message)
    yield TwoSelectedMemberPatternPlan(layout, compiled.pattern, receiver, argumentName)
