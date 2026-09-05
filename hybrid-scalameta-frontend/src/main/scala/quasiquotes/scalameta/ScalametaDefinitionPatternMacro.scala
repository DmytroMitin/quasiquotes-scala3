package quasiquotes.scalameta

import scala.quoted.*

import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend

private[scalameta] object ScalametaDefinitionPatternMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val staticParts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    staticParts match
      case None =>
        '{ ScalametaQuasiPattern.singleParameterExtractor($context)(using $callerQuotes) }
      case Some(parts) =>
        ScalametaDefinitionFrontend.classifyPatternParts(parts) match
          case Right(ScalametaDefinitionFrontend.PatternKind.SingleParameter) =>
            '{ ScalametaQuasiPattern.singleParameterExtractor($context)(using $callerQuotes) }
          case Right(ScalametaDefinitionFrontend.PatternKind.ExactTwo) =>
            '{ ScalametaQuasiPattern.exactTwoExtractor($context)(using $callerQuotes) }
          case Right(ScalametaDefinitionFrontend.PatternKind.RankedParameterSequence) =>
            '{ ScalametaQuasiPattern.rankedParameterSequenceExtractor($context)(using $callerQuotes) }
          case Right(ScalametaDefinitionFrontend.PatternKind.RankedParameterClauseSequence) =>
            '{ ScalametaQuasiPattern.rankedParameterClauseSequenceExtractor($context)(using $callerQuotes) }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedNameRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedNameRankedParameterClauseSequenceCapturedResultExtractor($context)(using
                  $callerQuotes
                )
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
                  $context
                )(using $callerQuotes)
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedNameNamedUsingParameterSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedNameNamedUsingParameterSequenceCapturedResultExtractor($context)(using
                  $callerQuotes
                )
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedModifiersNameNamedUsingParameterSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedModifiersNameNamedUsingParameterSequenceCapturedResultExtractor($context)(
                  using $callerQuotes
                )
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedModifiersNameScala2ImplicitParameterSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedModifiersNameScala2ImplicitParameterSequenceCapturedResultExtractor(
                  $context
                )(using $callerQuotes)
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedModifiersNameRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedModifiersNameRankedParameterClauseSequenceCapturedResultExtractor($context)(
                  using $callerQuotes
                )
            }
          case Right(
                ScalametaDefinitionFrontend.PatternKind.CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              ScalametaQuasiPattern
                .capturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
                  $context
                )(using $callerQuotes)
            }
          case Left(failure) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Scalameta dqq definition-pattern template: ${failure.message}",
              context
            )
