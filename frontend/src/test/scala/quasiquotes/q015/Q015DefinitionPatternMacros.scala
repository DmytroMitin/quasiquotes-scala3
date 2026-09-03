package quasiquotes.q015

import scala.quoted.*

object Q015DefinitionPatternMacros:
  def strategyA(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes],
      frontend: String
  )(using Quotes): Expr[Any] =
    validate(context, frontend)
    '{ Q015StrategyAFactory.extractor(using $callerQuotes) }

  def strategyB(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes],
      frontend: String
  )(using Quotes): Expr[Any] =
    validate(context, frontend)
    '{ Q015StrategyBFactory.extractor(using $callerQuotes) }

  def strategyC(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes],
      frontend: String
  )(using Quotes): Expr[Any] =
    validate(context, frontend)
    '{ Q015StrategyCFactory.extractor(using $callerQuotes) }

  def strategyD(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes],
      frontend: String
  )(using Quotes): Expr[Any] =
    validate(context, frontend)
    '{ Q015StrategyDFactory.extractor(using $callerQuotes) }

  private def validate(
      context: Expr[StringContext],
      frontend: String
  )(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(List("def collect(..", "): Int = ", "")) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(
          diagnostic(frontend, values),
          context
        )
      case None =>
        quotes.reflect.report.errorAndAbort(
          s"Q015 $frontend ranked Definition template must be statically known.",
          context
        )

  private def diagnostic(frontend: String, parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    val detail =
      if literal.contains("...") then "rank-3 parameter-clause capture is outside Q015"
      else if parts.count(_.contains("..")) > 1 then "two rank-2 captures are outside Q015"
      else if literal.contains("..") && !parts.headOption.exists(_.endsWith("collect(..")) then
        "rank-2 capture must occupy the immediate parameter list"
      else if parts.size != 3 then "exactly one parameter-sequence and one complete-body capture are required"
      else "unsupported structural topology or malformed rank marker"
    s"Invalid Q015 $frontend dqq Definition template: $detail."

object Q015StrategyAStandardDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyA('context, 'q, "standard") }

object Q015StrategyBStandardDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyB('context, 'q, "standard") }

object Q015StrategyCStandardDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyC('context, 'q, "standard") }

object Q015StrategyDStandardDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyD('context, 'q, "standard") }
