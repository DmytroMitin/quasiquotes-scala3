package quasiquotes.matching

import scala.quoted.*

import quasiquotes.scalameta.{ScalametaQuasiPattern, TermFrontend}
import quasiquotes.source.ReflectedPositionProvenance

private[quasiquotes] object ScalametaQuasiPatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        return '{ ScalametaQuasiPattern.scalarExtractor($context)(using $callerQuotes) }

    ScalametaSelectedChainPatternSource.classify(
      parts,
      ReflectedPositionProvenance.sourceCode(context.asTerm.pos).getOrElse(
        abort("selected-chain source-role planning requires recoverable original interpolation source", context)
      )
    ) match
      case Left(detail) => abort(detail, context)
      case Right(Some(plan)) => return QuasiPatternMacro.selectedChainExtractor(plan, callerQuotes)
      case Right(None) => ()

    ScalametaTwoSelectedMemberPatternSource.classify(
      parts,
      ReflectedPositionProvenance.sourceCode(context.asTerm.pos).getOrElse(
        abort(
          "two-selected-name source-role planning requires recoverable original interpolation source",
          context
        )
      )
    ) match
      case Left(detail) => abort(detail, context)
      case Right(Some(plan)) =>
        return twoSelectedMemberExtractor(plan, callerQuotes)
      case Right(None) => ()

    SelectedMemberPatternSource.classifyIncludingRanked(parts) match
      case Left(detail) => abort(detail, context)
      case Right(Some(plan)) =>
        val compiled = TermFrontend.compile(plan.effectiveSource).fold(
          failure => abort(failure.message, context),
          identity
        )
        SelectedMemberPatternBridge
          .validatePlan(plan, compiled.pattern)
          .fold(detail => abort(detail, context), identity)
        return selectedMemberExtractor(plan, compiled.pattern, callerQuotes)
      case Right(None) => ()

    RankedTermPatternBridge.classify(parts) match
      case Left(detail) => abort(detail, context)
      case Right(layout) =>
        layout.sequenceIndex match
          case None =>
            '{ ScalametaQuasiPattern.scalarExtractor($context)(using $callerQuotes) }
          case Some(sequenceIndex) =>
            val sequenceHoleName = layout.sequenceHoleName.getOrElse(
              quotes.reflect.report.errorAndAbort(
                "Ranked Scalameta qq layout lost its sequence capture",
                context
              )
            )
            val compiled = TermFrontend.compile(layout.effectiveSource).fold(
              failure => abort(failure.message, context),
              identity
            )
            RankedTermPatternBridge
              .validateSequencePosition(compiled.pattern, sequenceHoleName)
              .fold(detail => abort(detail, context), identity)

            val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
              case AppliedType(constructor, _) => constructor
              case other =>
                quotes.reflect.report.errorAndAbort(
                  s"Unable to resolve Scala tuple constructor: ${other.show}",
                  context
                )
            val kinds = layout.holeNames.indices.foldRight(TypeRepr.of[EmptyTuple]) {
              case (index, tail) =>
                val head =
                  if index == sequenceIndex then TypeRepr.of[SequenceTermCapture]
                  else TypeRepr.of[ScalarTermCapture]
                AppliedType(tupleCons, List(head, tail))
            }
            val pattern = RankedTermPatternBridge.patternExpr(compiled.pattern)
            val holeNames = RankedTermPatternBridge.holeNamesExpr(layout.holeNames)
            val sequenceName = Expr(sequenceHoleName)
            kinds.asType match
              case '[captureKinds] =>
                val directSingletonNewSequence =
                  layout.holeNames.size == 1 &&
                    RankedTermPatternBridge.containsDirectNewSequenceHole(
                      compiled.pattern,
                      sequenceHoleName
                    )
                if directSingletonNewSequence then
                  '{
                    RankedTermPatternBridge.singleSequenceExtractor(
                      $pattern,
                      $holeNames,
                      $sequenceName
                    )(using $callerQuotes)
                  }
                else
                  '{
                    RankedTermPatternBridge.extractor[captureKinds & Tuple](
                      $pattern,
                      $holeNames,
                      $sequenceName
                    )(using $callerQuotes)
                  }

  private def twoSelectedMemberExtractor(
      plan: TwoSelectedMemberPatternPlan,
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val pattern = RankedTermPatternBridge.patternExpr(plan.pattern)
    val receiverName = Expr(plan.receiverName)

    plan.layout match
      case TwoSelectedMemberPatternLayout.Direct =>
        '{
          TwoSelectedMemberPatternExtractorFactory.direct(using $callerQuotes)(
            TwoSelectedMemberPatternPlan(
              TwoSelectedMemberPatternLayout.Direct,
              $pattern,
              $receiverName,
              None
            )
          )
        }
      case TwoSelectedMemberPatternLayout.Unary =>
        val argumentName = Expr(plan.argumentName.getOrElse(
          quotes.reflect.report.errorAndAbort(
            "Two-selected-name Scalameta unary plan lost its argument capture"
          )
        ))
        '{
          TwoSelectedMemberPatternExtractorFactory.unary(using $callerQuotes)(
            TwoSelectedMemberPatternPlan(
              TwoSelectedMemberPatternLayout.Unary,
              $pattern,
              $receiverName,
              Some($argumentName)
            )
          )
        }

  private def selectedMemberExtractor(
      plan: SelectedMemberPatternPlan,
      compiledPattern: TermPattern,
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val pattern = RankedTermPatternBridge.patternExpr(compiledPattern)
    val holeNames = RankedTermPatternBridge.holeNamesExpr(plan.holeNames)
    val placeholder = Expr(plan.selectedNamePlaceholder)

    plan.sequenceIndex match
      case Some(sequenceIndex) =>
        val sequenceHoleName = plan.sequenceHoleName.getOrElse(
          quotes.reflect.report.errorAndAbort(
            "Mixed selected-name Scalameta qq layout lost its sequence capture"
          )
        )
        val tupleCons = quotes.reflect.TypeRepr.of[Any *: EmptyTuple] match
          case quotes.reflect.AppliedType(constructor, _) => constructor
          case other =>
            quotes.reflect.report.errorAndAbort(
              s"Unable to resolve Scala tuple constructor: ${other.show}"
            )
        val kinds = plan.holeNames.indices.foldRight(quotes.reflect.TypeRepr.of[EmptyTuple]) {
          case (index, tail) =>
            val head =
              if index == 1 then quotes.reflect.TypeRepr.of[ProductSelectedNameCapture]
              else if index == sequenceIndex then
                quotes.reflect.TypeRepr.of[ProductSequenceTermCapture]
              else quotes.reflect.TypeRepr.of[ProductTermCapture]
            quotes.reflect.AppliedType(tupleCons, List(head, tail))
        }
        return kinds.asType match
          case '[captureKinds] =>
            '{
              TermPatternProductExtractorFactory.mixed[captureKinds & Tuple](using $callerQuotes)(
                $pattern,
                $holeNames,
                $placeholder,
                ${ Expr(sequenceHoleName) }
              )
            }
      case None => ()

    plan.layout match
      case SelectedMemberPatternLayout.Direct =>
        '{
          TermPatternProductExtractorFactory.direct(using $callerQuotes)(
            $pattern,
            $holeNames,
            $placeholder
          )
        }
      case SelectedMemberPatternLayout.Nullary =>
        '{
          TermPatternProductExtractorFactory.nullary(using $callerQuotes)(
            $pattern,
            $holeNames,
            $placeholder
          )
        }
      case SelectedMemberPatternLayout.Unary =>
        '{
          TermPatternProductExtractorFactory.unary(using $callerQuotes)(
            $pattern,
            $holeNames,
            $placeholder
          )
        }

  private def abort(detail: String, context: Expr[StringContext])(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Scalameta qq term-pattern template: $detail",
      context
    )
