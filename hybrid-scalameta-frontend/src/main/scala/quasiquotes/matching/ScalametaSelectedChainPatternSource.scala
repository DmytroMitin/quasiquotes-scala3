package quasiquotes.matching

import quasiquotes.scalameta.TermFrontend

/** Original source roles are shared; hybrid IR must independently be Scalameta-primary. */
private[matching] object ScalametaSelectedChainPatternSource:
  def classify(
      parts: List[String],
      source: => String
  ): Either[String, Option[SelectedChainPatternSource.Plan]] =
    SelectedChainPatternSource.classify(parts, source).flatMap {
      case None => Right(None)
      case Some(plan) =>
        for
          compiled <- TermFrontend.compile(plan.effectiveSource).left.map(_.message)
          _ <- Either.cond(
            compiled.engine == TermFrontend.Engine.Scalameta && compiled.primaryFailure.isEmpty,
            (), "selected-chain source did not compile through the Scalameta-primary pattern frontend"
          )
          _ <- SelectedChainPatternSource.validate(
            compiled.pattern, plan.receiverName, plan.selectedOccurrences
          )
        yield Some(plan.copy(pattern = compiled.pattern))
    }
