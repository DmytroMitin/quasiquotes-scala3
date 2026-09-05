package external.consumer

import scala.quoted.*

import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
import DefinitionPattern.dqq

object Q035ExternalSemanticEmptyMixedOrdinaryNamedUsingConsumer:
  def exactType(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)

  def patternSite(using q: Quotes)(target: q.reflect.DefDef): Unit =
    target match
      case dqq"def $name(..$params)(using ..$usingParams): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.ValDef] = params
        val _: Seq[q.reflect.ValDef] = usingParams
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
