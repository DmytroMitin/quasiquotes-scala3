package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor
import quasiquotes.q027.{
  DefinitionParameterClause,
  Q027CandidateFactory,
  Q027ClauseMode
}

object Q027ExternalDefinitionClauseConsumer:
  def nested(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[Seq[q.reflect.ValDef]]]
  ] = Q027CandidateFactory.nested(using q)

  def native(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[q.reflect.TermParamClause]]
  ] = Q027CandidateFactory.native(using q)

  def structured(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[DefinitionParameterClause[Q027ClauseMode, q.reflect.ValDef]]]
  ] = Q027CandidateFactory.structured(using q)

  def namedUsing(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[q.reflect.ValDef]]
  ] = Q027CandidateFactory.namedUsing(using q)

  def ordinaryThenNamedUsing(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], Seq[q.reflect.ValDef])
  ] = Q027CandidateFactory.ordinaryThenNamedUsing(using q)
