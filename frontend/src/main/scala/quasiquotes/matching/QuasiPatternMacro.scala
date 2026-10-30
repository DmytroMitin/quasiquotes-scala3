package quasiquotes.matching

import scala.quoted.*

import quasiquotes.source.ReflectedPositionProvenance

private[matching] object QuasiPatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        return '{ QuasiPattern.scalarExtractor($context)(using $callerQuotes) }

    // Classify parts before reading positions: unrelated generated NoSpan
    // contexts retain the legacy scalar/selected/ranked admission rules.
    SelectedChainPatternSource.classify(
      parts,
      ReflectedPositionProvenance.sourceCode(context.asTerm.pos).getOrElse(
        report.errorAndAbort(
          "Invalid qq term-pattern template: selected-chain source-role planning " +
            "requires recoverable original interpolation source",
          context
        )
      )
    ) match
      case Left(detail) => report.errorAndAbort(s"Invalid qq term-pattern template: $detail", context)
      case Right(Some(plan)) => return selectedChainExtractor(plan, callerQuotes)
      case Right(None) => ()

    TwoSelectedMemberPatternSource.classify(
      parts,
      ReflectedPositionProvenance.sourceCode(context.asTerm.pos).getOrElse(
        quotes.reflect.report.errorAndAbort(
          "Invalid qq term-pattern template: two-selected-name source-role planning " +
            "requires recoverable original interpolation source",
          context
        )
      )
    ) match
      case Left(detail) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid qq term-pattern template: $detail",
          context
        )
      case Right(Some(plan)) =>
        return twoSelectedMemberExtractor(plan, callerQuotes)
      case Right(None) => ()

    SelectedMemberPatternSource.classifyIncludingRanked(parts) match
      case Left(detail) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid qq term-pattern template: $detail",
          context
        )
      case Right(Some(plan)) =>
        val pattern = QuasiPattern.term(plan.effectiveSource).fold(
          failure =>
            quotes.reflect.report.errorAndAbort(
              "Invalid qq term-pattern template: " + failure.message,
              context
            ),
          _.pattern
        )
        SelectedMemberPatternBridge
          .validatePlan(plan, pattern)
          .fold(
            detail =>
              quotes.reflect.report.errorAndAbort(
                "Invalid qq term-pattern template: " + detail,
                context
              ),
            identity
          )
        return selectedMemberExtractor(plan, pattern, callerQuotes)
      case Right(None) => ()

    RankedPatternSource.classify(parts) match
      case Left(detail) =>
        quotes.reflect.report.errorAndAbort(
          s"Invalid qq term-pattern template: $detail",
          context
        )
      case Right(template) =>
        template.sequenceIndex match
          case None => '{ QuasiPattern.scalarExtractor($context)(using $callerQuotes) }
          case Some(sequenceIndex) =>
            RankedPatternSource.compile(parts, sequenceIndex) match
              case Left(detail) =>
                quotes.reflect.report.errorAndAbort(
                  s"Invalid qq term-pattern template: $detail",
                  context
                )
              case Right(compiled) =>
                val tupleCons = TypeRepr.of[Any *: EmptyTuple] match
                  case AppliedType(constructor, _) => constructor
                  case other =>
                    quotes.reflect.report.errorAndAbort(
                      s"Unable to resolve Scala tuple constructor: ${other.show}",
                      context
                    )
                val kinds = template.holeNames.indices.foldRight(TypeRepr.of[EmptyTuple]) {
                  case (index, tail) =>
                    val head =
                      if index == sequenceIndex then TypeRepr.of[SequenceTermCapture]
                      else TypeRepr.of[ScalarTermCapture]
                    AppliedType(tupleCons, List(head, tail))
                }
                kinds.asType match
                  case '[captureKinds] =>
                    val directSingletonNewSequence =
                      template.holeNames.size == 1 && containsDirectNewSequenceHole(
                        compiled.pattern,
                        compiled.sequenceHoleName
                      )
                    if directSingletonNewSequence then
                      '{
                        RankedTermPatternExtractorFactory
                          .singleSequenceExtractor($context, ${ Expr(sequenceIndex) })(using $callerQuotes)
                      }
                    else
                      '{
                        RankedTermPatternExtractorFactory
                          .extractor[captureKinds & Tuple]($context, ${ Expr(sequenceIndex) })(using $callerQuotes)
                      }

  private[matching] def selectedChainExtractor(
      plan: SelectedChainPatternSource.Plan,
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val cons = TypeRepr.of[Any *: EmptyTuple] match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"Unable to resolve Scala tuple constructor: ${other.show}")
    val roles = TypeRepr.of[ProductTermCapture] ::
      List.fill(plan.selectedOccurrences.size)(TypeRepr.of[ProductSelectedNameCapture])
    val kinds = roles.foldRight(TypeRepr.of[EmptyTuple]) { (head, tail) =>
      AppliedType(cons, List(head, tail))
    }
    val pattern = RankedTermPatternBridge.patternExpr(plan.pattern)
    kinds.asType match
      case '[captureKinds] =>
        '{ SelectedChainTupleFactory.extractor[captureKinds & Tuple](using $callerQuotes)(
          $pattern, ${Expr(plan.receiverName)}, ${Expr(plan.selectedOccurrences.size)}
        ) }

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
            "Two-selected-name unary plan lost its argument capture"
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
            "Mixed selected-name qq layout lost its sequence capture"
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
        kinds.asType match
          case '[captureKinds] =>
            '{
              TermPatternProductExtractorFactory.mixed[captureKinds & Tuple](using $callerQuotes)(
                $pattern,
                $holeNames,
                $placeholder,
                ${ Expr(sequenceHoleName) }
              )
            }
      case None =>
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

  private def containsDirectNewSequenceHole(pattern: TermPattern, sequenceName: String): Boolean =
    pattern match
      case TermPattern.Lambda1(_, _, _, body) => containsDirectNewSequenceHole(body, sequenceName)
      case TermPattern.Select(qualifier, _) => containsDirectNewSequenceHole(qualifier, sequenceName)
      case TermPattern.Apply(function, arguments) =>
        containsDirectNewSequenceHole(function, sequenceName) ||
          arguments.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.New(_, arguments) =>
        arguments.exists {
          case TermPattern.Hole(name) => name == sequenceName
          case argument => containsDirectNewSequenceHole(argument, sequenceName)
        }
      case TermPattern.Infix(left, _, right) =>
        containsDirectNewSequenceHole(left, sequenceName) ||
          containsDirectNewSequenceHole(right, sequenceName)
      case TermPattern.Unary(_, operand) => containsDirectNewSequenceHole(operand, sequenceName)
      case TermPattern.InterpolatedString(_, _, arguments) =>
        arguments.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.Typed(expression, _) => containsDirectNewSequenceHole(expression, sequenceName)
      case TermPattern.Tuple(elements) => elements.exists(containsDirectNewSequenceHole(_, sequenceName))
      case TermPattern.If(condition, thenBranch, elseBranch) =>
        containsDirectNewSequenceHole(condition, sequenceName) ||
          containsDirectNewSequenceHole(thenBranch, sequenceName) ||
          containsDirectNewSequenceHole(elseBranch, sequenceName)
      case TermPattern.Block(prefix, result) =>
        prefix.exists {
          case term: TermPattern => containsDirectNewSequenceHole(term, sequenceName)
          case BlockPatternStatement.LocalVal(_, _, _, initializer) =>
            containsDirectNewSequenceHole(initializer, sequenceName)
          case BlockPatternStatement.LocalDef(_, _, _, _, _, _, body) =>
            containsDirectNewSequenceHole(body, sequenceName)
        } || containsDirectNewSequenceHole(result, sequenceName)
      case TermPattern.Parenthesized(inner) => containsDirectNewSequenceHole(inner, sequenceName)
      case _ => false
