package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q019ExternalScalametaDefinitionCaptureConsumer:
  def semantic(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "(...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name(...$paramss): $result = $body" =>
        val _: String = name
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def tree(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.hybrid.q019.Q019TreeScalametaPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)
    ] = dqq(StringContext("def ", "(...", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name(...$paramss): $result = $body" =>
        val _: q.reflect.Symbol = name
        val _: Seq[Seq[q.reflect.ValDef]] = paramss
        val _: q.reflect.TypeTree = result
        val _: q.reflect.Term = body
      case _ => ()
