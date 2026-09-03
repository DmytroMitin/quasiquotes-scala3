package quasiquotes.hybrid.q015

import scala.quoted.*

import quasiquotes.q015.Q015DefinitionPatternMacros

object Q015StrategyAScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyA('context, 'q, "typed-Scalameta") }

object Q015StrategyBScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q015DefinitionPatternMacros.strategyB('context, 'q, "typed-Scalameta") }
