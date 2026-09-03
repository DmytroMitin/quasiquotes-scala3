package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q017ExternalDefinitionParamssConsumer:
  def candidateA(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q017.Q017CandidateAStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
    ] = dqq(StringContext("def collect(...", "): Int = ", ""))(using q)

    target match
      case dqq"def collect(...$paramss): Int = $body" =>
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.Term = body
      case _ => ()

  def candidateB(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q017.Q017CandidateBStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (Seq[q.reflect.TermParamClause], q.reflect.Term)
    ] = dqq(StringContext("def collect(...", "): Int = ", ""))(using q)

    target match
      case dqq"def collect(...$paramss): Int = $body" =>
        val _: Seq[q.reflect.TermParamClause] = paramss
        val _: q.reflect.Term = body
      case _ => ()
