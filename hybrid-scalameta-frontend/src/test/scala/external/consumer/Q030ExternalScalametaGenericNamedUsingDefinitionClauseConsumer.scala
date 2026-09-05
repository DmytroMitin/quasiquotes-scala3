package external.consumer

import scala.quoted.*

import quasiquotes.hybrid.q030.{
  Q030ScalametaDefinitionSummary,
  Q030ScalametaDefinitionSyntax
}
import quasiquotes.matching.{
  DefinitionModifiers,
  Q030GenericNamedUsingCandidateFactory,
  RankedDefinitionPatternExtractor
}

object Q030ExternalScalametaGenericNamedUsingDefinitionClauseConsumer:
  val source: Either[String, Q030ScalametaDefinitionSummary] =
    Q030ScalametaDefinitionSyntax.inspect(
      "def generic[A](using ordering: Ordering[A]): Int = 1"
    )

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
