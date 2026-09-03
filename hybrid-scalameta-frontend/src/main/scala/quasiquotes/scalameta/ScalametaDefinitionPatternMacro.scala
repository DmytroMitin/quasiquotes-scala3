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
          case Right(1) =>
            '{ ScalametaQuasiPattern.singleParameterExtractor($context)(using $callerQuotes) }
          case Right(2) =>
            '{ ScalametaQuasiPattern.exactTwoExtractor($context)(using $callerQuotes) }
          case Right(other) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Scalameta dqq definition-pattern template: unsupported Definition arity $other.",
              context
            )
          case Left(failure) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Scalameta dqq definition-pattern template: ${failure.message}",
              context
            )
