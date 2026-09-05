package quasiquotes.hybrid.q033

import scala.quoted.*

import quasiquotes.q033.Q033MixedClauseCandidateFactory

object Q033ScalametaDefinitionPatternMacros:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) =>
        Q033ScalametaMixedClauseSyntax.validatePatternParts(values) match
          case Right(_) =>
            '{ Q033MixedClauseCandidateFactory.capturedModifiers(using $callerQuotes) }
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Q033 typed-Scalameta dqq Definition template: $detail.",
              context
            )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q033 typed-Scalameta mixed-clause Definition template must be statically known.",
          context
        )

object Q033ScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q033ScalametaDefinitionPatternMacros.extractor('context, 'q) }
