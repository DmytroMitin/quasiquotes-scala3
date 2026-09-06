package external.consumer

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
import DefinitionPattern.dqq

object Q037ExternalMixedOrdinaryScala2ImplicitConsumer:
  def exactType(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)

  def patternSite(using q: Quotes)(target: q.reflect.DefDef): Unit =
    target match
      case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body" =>
        val _: DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term] = mods
        val _: String = name
        val _: Seq[q.reflect.ValDef] = params
        val _: Seq[q.reflect.ValDef] = implicitParams
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
