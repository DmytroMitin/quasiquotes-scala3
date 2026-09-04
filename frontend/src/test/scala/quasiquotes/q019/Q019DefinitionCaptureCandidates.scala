package quasiquotes.q019

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

private[q019] object Q019DefinitionCaptureMatcher:
  def semantic(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    admitted(target).map { (clauses, body) =>
      (target.name, clauses.map(_.params), target.returnTpt.tpe, body)
    }

  def tree(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)] =
    admitted(target).map { (clauses, body) =>
      (target.symbol, clauses.map(_.params), target.returnTpt, body)
    }

  def stringTree(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)] =
    admitted(target).map { (clauses, body) =>
      (target.name, clauses.map(_.params), target.returnTpt, body)
    }

  def symbolTypeRepr(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    admitted(target).map { (clauses, body) =>
      (target.symbol, clauses.map(_.params), target.returnTpt.tpe, body)
    }

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TermParamClause], q.reflect.Term)] =
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
      val rawClauses = target.paramss
      val clauses = rawClauses.collect { case clause: TermParamClause => clause }
      val parameters = clauses.flatMap(_.params)
      val symbols = parameters.map(_.symbol)
      val nestedSymbols = clauses.map(_.params.map(_.symbol).toList)
      val admittedClauses =
        clauses.size == rawClauses.size &&
          clauses.forall(clause =>
            !clause.isImplicit && !clause.isGiven && !clause.isErased
          )
      val admittedParameters =
        symbols.forall(_ != Symbol.noSymbol) &&
          symbols.distinct.size == symbols.size &&
          parameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              !parameter.symbol.flags.is(Flags.HasDefault) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given)
          ) &&
          target.symbol.paramSymss == nestedSymbols

      Option
        .when(admittedClauses && admittedParameters)(())
        .flatMap(_ => target.rhs.map(body => (clauses, body)))

object Q019CandidateFactory:
  def semantic(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q019DefinitionCaptureMatcher.semantic(_))

  def tree(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q019DefinitionCaptureMatcher.tree(_))

  def stringTree(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q019DefinitionCaptureMatcher.stringTree(_))

  def symbolTypeRepr(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] = new RankedDefinitionPatternExtractor(Q019DefinitionCaptureMatcher.symbolTypeRepr(_))

object Q019DefinitionCapturePatternMacro:
  def semantic(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q019CandidateFactory.semantic(using $callerQuotes) }

  def tree(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q019CandidateFactory.tree(using $callerQuotes) }

  private def validate(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if exactLayout(values) => ()
      case Some(values) =>
        quotes.reflect.report.errorAndAbort(diagnostic(values), context)
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q019 standard Definition capture template must be statically known.",
          context
        )

  private def exactLayout(parts: List[String]): Boolean =
    parts match
      case List(prefix, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeParamss.matches("(?s)\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    val detail =
      if literal.contains("..") && !literal.contains("...") then
        "rank-2 capture is not rank-3 parameter-clause capture"
      else if parts.count(_.contains("...")) > 1 then
        "exactly one rank-3 capture is required"
      else
        "name, complete rank-3 paramss, result, and complete body captures are required"
    s"Invalid Q019 standard dqq Definition template: $detail."

object Q019SemanticStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q019DefinitionCapturePatternMacro.semantic('context, 'q) }

object Q019TreeStandardPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q019DefinitionCapturePatternMacro.tree('context, 'q) }
