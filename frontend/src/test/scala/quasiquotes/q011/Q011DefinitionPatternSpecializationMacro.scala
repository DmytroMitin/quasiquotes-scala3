package quasiquotes.q011

import scala.quoted.*

import quasiquotes.matching.DefinitionPattern

private[q011] object Q011DefinitionPatternSpecializationMacro:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        partExpressions.toList.map(_.valueOrAbort)
      case _ =>
        quotes.reflect.report.errorAndAbort(
          "Q011 Strategy A requires a statically known StringContext.",
          context
        )

    if parts == List("def identity(value: Int): Int = ", "") then
      '{ DefinitionPattern.dqq($context)(using $callerQuotes) }
    else if parts == List("def first(left: Int, right: String): Int = ", "") then
      '{ Q011TwoParameterDefinitionPattern.exactFirst }
    else if parts == List("def second(left: Int, right: String): String = ", "") then
      '{ Q011TwoParameterDefinitionPattern.exactSecond }
    else
      quotes.reflect.report.errorAndAbort(
        "Q011 Strategy A admits only its exact static one- and two-parameter templates.",
        context
      )
