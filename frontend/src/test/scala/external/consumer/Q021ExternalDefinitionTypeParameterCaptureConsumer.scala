package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q021ExternalDefinitionTypeParameterCaptureConsumer:
  def typeDefs(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q021.Q021TypeDefStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.TypeDef] = tparams
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def symbols(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q021.Q021SymbolStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.Symbol], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
        val _: Seq[q.reflect.Symbol] = tparams
      case _ => ()

  def nameBounds(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.q021.Q021NameBoundsStandardPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[(String, q.reflect.TypeBounds)], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
        val _: Seq[(String, q.reflect.TypeBounds)] = tparams
      case _ => ()
