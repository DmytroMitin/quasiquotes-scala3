package quasiquotes.q036

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q036MixedClauseCandidateFactory:
  def capturedModifiers(using q: Quotes): RankedDefinitionPatternExtractor[
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
    new RankedDefinitionPatternExtractor(target =>
      admitted(target).map { (ordinary, contextual, result, body) =>
        val modifiers = new DefinitionModifiers(
          target.symbol.flags,
          target.symbol.privateWithin,
          target.symbol.protectedWithin,
          target.symbol.annotations
        )
        (modifiers, target.name, ordinary, contextual, result, body)
      }
    )

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.ValDef], List[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
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
        case List(ordinary: TermParamClause, contextual: TermParamClause)
            if !ordinary.isImplicit && !ordinary.isGiven && !ordinary.isErased &&
              contextual.isImplicit && !contextual.isGiven && !contextual.isErased &&
              contextual.params.nonEmpty =>
          val ordinaryParameters = ordinary.params
          val contextualParameters = contextual.params
          val ordinarySymbols = ordinaryParameters.map(_.symbol)
          val contextualSymbols = contextualParameters.map(_.symbol)
          val allSymbols = ordinarySymbols ++ contextualSymbols
          val ordinaryAdmitted = ordinaryParameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          )
          val contextualAdmitted = contextualParameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          )
          val symbolTopologyAdmitted =
            allSymbols.forall(_ != Symbol.noSymbol) &&
              allSymbols.distinct.size == allSymbols.size &&
              target.symbol.paramSymss == List(ordinarySymbols, contextualSymbols)

          Option
            .when(ordinaryAdmitted && contextualAdmitted && symbolTopologyAdmitted)(
              target.returnTpt.tpe
            )
            .flatMap(result => target.rhs.map(body =>
              (ordinaryParameters, contextualParameters, result, body)
            ))
        case _ => None

object Q036DefinitionPatternMacros:
  def standard(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validateStaticParts(context, "standard")
    '{ Q036MixedClauseCandidateFactory.capturedModifiers(using $callerQuotes) }

  private def validateStaticParts(
      context: Expr[StringContext],
      frontend: String
  )(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if isExact(values) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(diagnostic(frontend, values), context)
      case None =>
        quotes.reflect.report.errorAndAbort(
          s"Q036 $frontend mixed-clause Definition template must be statically known.",
          context
        )

  private def isExact(parts: List[String]): Boolean =
    parts match
      case List(beforeModifiers, beforeName, beforeOrdinary, beforeImplicit, beforeResult, beforeBody, suffix) =>
        beforeModifiers.trim.isEmpty &&
          beforeName.matches("(?s)\\s+def\\s+") &&
          beforeOrdinary.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
          beforeImplicit.matches("(?s)\\s*\\)\\s*\\(\\s*implicit\\s+\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def diagnostic(frontend: String, parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    val detail =
      if literal.contains("...") then "rank-3 capture is outside Q036"
      else if parts.size != 7 then "exactly six ordered captures are required"
      else if !literal.contains("(implicit") then "the second clause must be an explicit Scala-2 implicit clause"
      else "only `$mods def $name(..$params)(implicit ..$implicitParams): $result = $body` is selected"
    s"Invalid Q036 $frontend dqq Definition template: $detail."

object Q036StandardDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q036DefinitionPatternMacros.standard('context, 'q) }
