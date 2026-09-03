package external.consumer

import scala.quoted.*

object Q017ExternalScalametaDefinitionParamssConsumer:
  def candidateA(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq

    target match
      case dqq"def collect(...$paramss): Int = $body" =>
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.Term = body
      case _ => ()

  def candidateB(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.hybrid.q017.Q017CandidateBScalametaPattern.dqq

    target match
      case dqq"def collect(...$clauses): Int = $body" =>
        val _: Seq[q.reflect.TermParamClause] = clauses
        val _: q.reflect.Term = body
      case _ => ()
