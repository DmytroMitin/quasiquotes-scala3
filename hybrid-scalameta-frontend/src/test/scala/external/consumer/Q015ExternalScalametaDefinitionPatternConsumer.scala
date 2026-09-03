package external.consumer

import scala.quoted.*

object Q015ExternalScalametaDefinitionPatternConsumer:
  def strategyB(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.hybrid.q015.Q015StrategyBScalametaDefinitionPattern.dqq

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()
