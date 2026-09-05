package external.consumer

import scala.quoted.*

import quasiquotes.matching.{
  DefinitionModifiers,
  Q030GenericNamedUsingCandidateFactory,
  RankedDefinitionPatternExtractor
}

object Q030ExternalGenericNamedUsingDefinitionClauseConsumer:
  def capturedModifiers(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.TypeDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)

  def semanticEmpty(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.TypeDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] = Q030GenericNamedUsingCandidateFactory.semanticEmpty(using q)
