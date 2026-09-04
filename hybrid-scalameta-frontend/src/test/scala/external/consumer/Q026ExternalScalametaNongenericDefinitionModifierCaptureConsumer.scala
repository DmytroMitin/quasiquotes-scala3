package external.consumer

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q026ExternalScalametaNongenericDefinitionModifierCaptureConsumer:
  def direct(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (
        DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
        String,
        Seq[Seq[q.reflect.ValDef]],
        q.reflect.TypeRepr,
        q.reflect.Term
      )
    ] = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)

    target match
      case dqq"$mods def $name(...$paramss): $result = $body" =>
        val _: DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term] = mods
        val _: String = name
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def umbrella(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.Quasiquotes.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (
        DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
        String,
        Seq[Seq[q.reflect.ValDef]],
        q.reflect.TypeRepr,
        q.reflect.Term
      )
    ] = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)

    target match
      case dqq"$mods def $name(...$paramss): $result = $body" =>
        val _: DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term] = mods
        val _: String = name
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
