package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor
import quasiquotes.q023.DefinitionModifiers

object Q023ExternalDefinitionModifierCaptureConsumer:
  def flags(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q023.Q023FlagsStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (
        q.reflect.Flags,
        String,
        Seq[q.reflect.TypeDef],
        Seq[Seq[q.reflect.ValDef]],
        q.reflect.TypeRepr,
        q.reflect.Term
      )
    ] = dqq(StringContext("", " def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" =>
        val _: q.reflect.Flags = mods
        val _: String = name
        val _: Seq[q.reflect.TypeDef] = tparams
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def structured(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q023.Q023StructuredStandardPattern.dqq

    target match
      case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" =>
        val _: DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term] = mods
      case _ => ()

  def symbol(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q023.Q023SymbolStandardPattern.dqq

    target match
      case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" =>
        val _: q.reflect.Symbol = mods
      case _ => ()
