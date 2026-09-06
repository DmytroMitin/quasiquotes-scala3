package quasiquotes.q042

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q042ByNameOrdinaryCandidateFactory:
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

  private[quasiquotes] def valDefByNameElementType(using q: Quotes)(
      parameter: q.reflect.ValDef
  ): Option[q.reflect.TypeRepr] =
    import q.reflect.*
    Option(parameter).flatMap(_.tpt.tpe match
      case ByNameType(element) => Some(element)
      case _ => None
    )

  private[quasiquotes] def symbolByNameElementType(using q: Quotes)(
      parameter: q.reflect.ValDef
  ): Option[q.reflect.TypeRepr] =
    import q.reflect.*
    Option(parameter).flatMap(_.symbol.termRef.widen match
      case ByNameType(element) => Some(element)
      case _ => None
    )

  private[quasiquotes] def methodByNameElementType(using q: Quotes)(
      target: q.reflect.DefDef,
      parameterIndex: Int
  ): Option[q.reflect.TypeRepr] =
    import q.reflect.*
    Option(target).flatMap(_.symbol.termRef.widen match
      case method: MethodType =>
        method.paramTypes.lift(parameterIndex).flatMap {
          case ByNameType(element) => Some(element)
          case _ => None
        }
      case _ => None
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
          val byNameIndices = parameters.zipWithIndex.collect {
            case (parameter, index)
                if agreesOnByNameElement(target, parameter, index) => index
          }
          val coherentModes = parameters.zipWithIndex.forall { (parameter, index) =>
            (valDefByNameElementType(parameter), methodByNameElementType(target, index)) match
              case (None, None) => true
              case (Some(_), Some(_)) => agreesOnByNameElement(target, parameter, index)
              case _ => false
          }
          val admittedParameters = parameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Param) &&
              parameter.symbol.annotations.isEmpty &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          )
          val exactTopology =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              target.symbol.paramSymss == List(symbols)

          Option
            .when(byNameIndices.nonEmpty && coherentModes && admittedParameters && exactTopology)(
              target.returnTpt.tpe
            )
            .flatMap(result => target.rhs.map(body => (parameters, result, body)))
        case _ => None

  private def agreesOnByNameElement(using q: Quotes)(
      target: q.reflect.DefDef,
      parameter: q.reflect.ValDef,
      index: Int
  ): Boolean =
    (valDefByNameElementType(parameter), methodByNameElementType(target, index)) match
      case (Some(treeElement), Some(methodElement)) =>
        treeElement =:= methodElement && parameter.symbol.termRef.widen =:= treeElement
      case _ => false

object Q042DefinitionPatternMacros:
  def standard(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validateStaticParts(context)
    '{ Q042ByNameOrdinaryCandidateFactory.capturedModifiers(using $callerQuotes) }

  private def validateStaticParts(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if isExact(values) => ()
      case Some(values) => quotes.reflect.report.errorAndAbort(diagnostic(values), context)
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q042 standard by-name ordinary Definition template must be statically known.",
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
      if literal.contains("...") then "rank-3 capture is outside the Q042 test helper"
      else if parts.size != 6 then "exactly five ordered captures are required"
      else "only `$mods def $name(..$params): $result = $body` is selected"
    s"Invalid Q042 standard dqq Definition template: $detail."

object Q042ByNameOrdinaryDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q042DefinitionPatternMacros.standard('context, 'q) }
