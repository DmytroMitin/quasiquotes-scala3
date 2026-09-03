package external.consumer

import scala.quoted.*

object Q015ExternalDefinitionPatternConsumer:
  def strategyB(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()

  def strategyC(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q015.Q015StrategyCStandardDefinitionPattern.dqq

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()

  def strategyD(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q015.Q015StrategyDStandardDefinitionPattern.dqq

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()
