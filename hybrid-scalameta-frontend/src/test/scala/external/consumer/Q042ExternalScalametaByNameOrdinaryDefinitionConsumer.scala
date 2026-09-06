package external.consumer

import scala.quoted.*

import quasiquotes.hybrid.q042.Q042ScalametaDefinitionPattern.dqq
import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q042ExternalScalametaByNameOrdinaryDefinitionConsumer:
  def exactType(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)

  def patternSite(using q: Quotes)(target: q.reflect.DefDef): Unit =
    target match
      case dqq"$mods def $name(..$params): $result = $body" =>
        val _: DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term] = mods
        val _: String = name
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
