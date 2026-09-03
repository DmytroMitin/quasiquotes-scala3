package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object RankedScalametaDefinitionPatternExternalConsumer:
  def direct(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (Seq[q.reflect.ValDef], q.reflect.Term)
    ] = dqq(StringContext("def collect(..", "): Int = ", ""))(using q)

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()

  def umbrella(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.Quasiquotes.dqq

    target match
      case dqq"def collect(..$params): Int = $body" =>
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.Term = body
      case _ => ()
