package external.consumer

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

object Q032ExternalSemanticEmptyScala2ImplicitDefinitionClauseConsumer:
  def direct(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.matching.DefinitionPattern.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name(implicit ..$params): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()

  def umbrella(using q: Quotes)(target: q.reflect.DefDef): Unit =
    import quasiquotes.Quasiquotes.dqq

    val _: RankedDefinitionPatternExtractor[
      q.reflect.DefDef,
      (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
    ] = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)

    target match
      case dqq"def $name(implicit ..$params): $result = $body" =>
        val _: String = name
        val _: Seq[q.reflect.ValDef] = params
        val _: q.reflect.TypeRepr = result
        val _: q.reflect.Term = body
      case _ => ()
