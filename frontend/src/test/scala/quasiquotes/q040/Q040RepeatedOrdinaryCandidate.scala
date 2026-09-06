package quasiquotes.q040

import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

object Q040RepeatedOrdinaryCandidateFactory:
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

  private[q040] def repeatedElementType(using q: Quotes)(
      parameter: q.reflect.ValDef
  ): Option[q.reflect.TypeRepr] =
    import q.reflect.*

    Option(parameter).flatMap(_.tpt.tpe match
      case AnnotatedType(AppliedType(_, List(element)), annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot =>
        Some(element)
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass =>
        Some(element)
      case _ => None
    )

  private[q040] def methodRepeatedElementType(using q: Quotes)(
      target: q.reflect.DefDef,
      parameterIndex: Int
  ): Option[q.reflect.TypeRepr] =
    import q.reflect.*

    Option(target).flatMap(_.symbol.termRef.widen match
      case method: MethodType =>
        method.paramTypes.lift(parameterIndex).flatMap {
          case AppliedType(constructor, List(element))
              if constructor.typeSymbol == defn.RepeatedParamClass =>
            Some(element)
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
          def agreesOnRepeatedElement(parameter: ValDef, index: Int): Boolean =
            (repeatedElementType(parameter), methodRepeatedElementType(target, index)) match
              case (Some(treeElement), Some(methodElement)) => treeElement =:= methodElement
              case _ => false
          val repeatedIndices = parameters.zipWithIndex.collect {
            case (parameter, index) if agreesOnRepeatedElement(parameter, index) =>
              index
          }
          val admittedParameters = parameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              parameter.symbol.annotations.isEmpty &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault) &&
              (parameter.tpt.tpe match
                case _: ByNameType => false
                case _ => true)
          )
          val exactTopology =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              target.symbol.paramSymss == List(symbols)
          val exactRepeatedShape = repeatedIndices == List(parameters.size - 1)

          Option
            .when(admittedParameters && exactTopology && exactRepeatedShape)(target.returnTpt.tpe)
            .flatMap(result => target.rhs.map(body => (parameters, result, body)))
        case _ => None

object Q040DefinitionPatternMacros:
  def standard(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validateStaticParts(context)
    '{ Q040RepeatedOrdinaryCandidateFactory.capturedModifiers(using $callerQuotes) }

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
          "Q040 standard repeated-ordinary Definition template must be statically known.",
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
      if literal.contains("...") then "rank-3 capture is outside the Q040 test helper"
      else if parts.size != 6 then "exactly five ordered captures are required"
      else "only `$mods def $name(..$params): $result = $body` is selected"
    s"Invalid Q040 standard dqq Definition template: $detail."

object Q040RepeatedOrdinaryDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q040DefinitionPatternMacros.standard('context, 'q) }
