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
          case Right(1) =>
            '{ DefinitionPattern.singleParameterExtractor($context)(using $callerQuotes) }
          case Right(2) =>
            '{ DefinitionPattern.twoParameterExtractor($context)(using $callerQuotes) }
          case Right(other) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid dqq definition-pattern template: unsupported Definition arity $other.",
              context
            )
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid dqq definition-pattern template: $detail",
              context
            )
