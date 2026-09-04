package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q018ExternalDefinitionParamssConsumer:
  def direct(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.matching.DefinitionPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
    ] = dqq(StringContext("def collect(...", "): Int = ", ""))(using q)

    target match
      case dqq"def collect(...$paramss): Int = $body" =>
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.Term = body
      case _ => ()

  def umbrella(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.Quasiquotes.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
    ] = dqq(StringContext("def collect(...", "): Int = ", ""))(using q)

    target match
      case dqq"def collect(...$paramss): Int = $body" =>
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.Term = body
      case _ => ()
