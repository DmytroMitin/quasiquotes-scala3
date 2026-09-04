package quasiquotes.matching

import scala.quoted.*

private[matching] object DefinitionPatternMacro:
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
        '{ DefinitionPattern.singleParameterExtractor($context)(using $callerQuotes) }
      case Some(parts) =>
        DefinitionPattern.classifyStaticParts(parts) match
          case Right(DefinitionPattern.StaticPatternKind.SingleParameter) =>
            '{ DefinitionPattern.singleParameterExtractor($context)(using $callerQuotes) }
          case Right(DefinitionPattern.StaticPatternKind.ExactTwo) =>
            '{ DefinitionPattern.twoParameterExtractor($context)(using $callerQuotes) }
          case Right(DefinitionPattern.StaticPatternKind.RankedParameterSequence) =>
            '{ DefinitionPattern.rankedParameterSequenceExtractor($context)(using $callerQuotes) }
          case Right(DefinitionPattern.StaticPatternKind.RankedParameterClauseSequence) =>
            '{ DefinitionPattern.rankedParameterClauseSequenceExtractor($context)(using $callerQuotes) }
          case Right(
                DefinitionPattern.StaticPatternKind.CapturedNameRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              DefinitionPattern
                .capturedNameRankedParameterClauseSequenceCapturedResultExtractor($context)(using
                  $callerQuotes
                )
            }
          case Right(
                DefinitionPattern.StaticPatternKind.CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
              ) =>
            '{
              DefinitionPattern
                .capturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
                  $context
                )(using $callerQuotes)
            }
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid dqq definition-pattern template: $detail",
              context
            )
