package quasiquotes.q038

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q038DefaultedOrdinaryCandidateFactory:
  def capturedModifiers(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      admitted(target).map { (parameters, result, body) =>
        val modifiers = new DefinitionModifiers(
          target.symbol.flags,
          target.symbol.privateWithin,
          target.symbol.protectedWithin,
          target.symbol.annotations
        )
        (modifiers, target.name, parameters, result, body)
      }
    )

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
    import q.reflect.*

    if target == null ||
        target.symbol == Symbol.noSymbol ||
        !target.symbol.isDefDef ||
        target.symbol.isClassConstructor ||
        target.symbol.flags.is(Flags.ExtensionMethod) ||
        target.symbol.flags.is(Flags.FieldAccessor) ||
        target.symbol.flags.is(Flags.ParamAccessor) ||
        target.symbol.flags.is(Flags.CaseAccessor) ||
        target.symbol.flags.is(Flags.Given)
    then None
    else
      target.paramss match
        case List(clause: TermParamClause)
            if !clause.isImplicit && !clause.isGiven && !clause.isErased && clause.params.nonEmpty =>
          val parameters = clause.params
          val symbols = parameters.map(_.symbol)
          val ordinarySourceParameters = parameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              parameter.symbol.annotations.isEmpty
          )
          val exactTopology =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              target.symbol.paramSymss == List(symbols)
          val hasDefault = parameters.exists(_.symbol.flags.is(Flags.HasDefault))

          Option
            .when(ordinarySourceParameters && exactTopology && hasDefault)(target.returnTpt.tpe)
            .flatMap(result => target.rhs.map(body => (parameters, result, body)))
        case _ => None

object Q038DefinitionPatternMacros:
  def standard(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validateStaticParts(context)
    '{ Q038DefaultedOrdinaryCandidateFactory.capturedModifiers(using $callerQuotes) }

  private def validateStaticParts(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if isExact(values) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(diagnostic(values), context)
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q038 standard defaulted-ordinary Definition template must be statically known.",
          context
        )

  private def isExact(parts: List[String]): Boolean =
    parts match
      case List(beforeModifiers, beforeName, beforeParameters, beforeResult, beforeBody, suffix) =>
        beforeModifiers.trim.isEmpty &&
          beforeName.matches("(?s)\\s+def\\s+") &&
          beforeParameters.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    val detail =
      if literal.contains("...") then "rank-3 capture is outside Q038"
      else if parts.size != 6 then "exactly five ordered captures are required"
      else "only `$mods def $name(..$params): $result = $body` is selected"
    s"Invalid Q038 standard dqq Definition template: $detail."

object Q038DefaultedOrdinaryDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q038DefinitionPatternMacros.standard('context, 'q) }
