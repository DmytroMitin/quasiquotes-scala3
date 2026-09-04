package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q022ExternalScalametaDefinitionTypeParameterCaptureConsumer:
  def direct(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] =
      dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.TypeDef] = tparams
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def umbrella(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.scalameta.Quasiquotes.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] =
      dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.TypeDef] = tparams
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
