package quasiquotes.q017

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

object Q017DefinitionParamssMatcher:
  def candidateA(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(Seq[Seq[q.reflect.ValDef]], q.reflect.Term)] =
    candidateB(target).map((clauses, body) => (clauses.map(_.params), body))

  def candidateB(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(Seq[q.reflect.TermParamClause], q.reflect.Term)] =
    import q.reflect.*

    if target == null ||
        target.name != "collect" ||
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
      val rawClauses = target.paramss
      val termClauses = rawClauses.collect { case clause: TermParamClause => clause }
      val parameterSymbols = termClauses.map(_.params.map(_.symbol).toList)
      val allParameters = termClauses.flatMap(_.params)
      val allSymbols = allParameters.map(_.symbol)
      val admitted =
        termClauses.size == rawClauses.size &&
          allSymbols.forall(_ != Symbol.noSymbol) &&
          allSymbols.distinct.size == allSymbols.size &&
          allParameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ) &&
          target.symbol.paramSymss == parameterSymbols

      Option
        .when(
          admitted &&
            TargetTypeReprInspector
              .inspect(target.returnTpt.tpe)
              .contains(TypeNormalForm.STypeIdent("Int"))
        )(()).flatMap(_ => target.rhs.map(body => (termClauses, body)))

object Q017CandidateAFactory:
  def extractor(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      Q017DefinitionParamssMatcher.candidateA(target)
    )

object Q017CandidateBFactory:
  def extractor(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.TermParamClause], q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      Q017DefinitionParamssMatcher.candidateB(target)
    )

object Q017DefinitionParamssPatternMacro:
  def candidateA(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q017CandidateAFactory.extractor(using $callerQuotes) }

  def candidateB(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q017CandidateBFactory.extractor(using $callerQuotes) }

  private def validate(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if exactRank3(values) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(diagnostic(values), context)
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q017 standard rank-3 Definition template must be statically known.",
          context
        )

  private def exactRank3(parts: List[String]): Boolean =
    parts match
      case List(prefix, between, suffix) =>
        prefix.matches("(?s)\\s*def\\s+collect\\s*\\(\\s*\\.\\.\\.\\s*") &&
          between.matches("(?s)\\s*\\)\\s*:\\s*Int\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    val detail =
      if literal.contains("..") && !literal.contains("...") then
        "rank-2 capture is not rank-3 parameter-clause capture"
      else if parts.count(_.contains("...")) > 1 then
        "exactly one rank-3 capture is required"
      else if literal.contains("...") then
        "rank-3 capture must occupy the complete parameter-clause region"
      else
        "one rank-3 parameter-clause capture and one complete-body capture are required"
    s"Invalid Q017 standard dqq Definition template: $detail."

object Q017CandidateAStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q017DefinitionParamssPatternMacro.candidateA('context, 'q) }

object Q017CandidateBStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q017DefinitionParamssPatternMacro.candidateB('context, 'q) }
