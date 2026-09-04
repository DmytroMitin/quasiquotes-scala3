package quasiquotes.matching

import scala.quoted.Quotes

import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

/** A typed product extractor for ranked captures from a Definition pattern. */
final class RankedDefinitionPatternExtractor[Target, Captures <: Tuple](
    extract: Target => Option[Captures]
):
  def unapply(value: Target): Option[Captures] = extract(value)

private[quasiquotes] object RankedDefinitionPatternExtractorFactory:
  def exactCollect(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractExactCollect(target)
    )

  def exactCollectParamss(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractExactCollectParamss(target)
    )

  def capturedNameParamssResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedNameParamssResult(target)
    )

private[matching] object RankedDefinitionPatternMatcher:
  def extractExactCollect(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(Seq[q.reflect.ValDef], q.reflect.Term)] =
    extractExactCollectClauses(target).flatMap { (clauses, body) =>
      clauses match
        case clause :: Nil => Some((clause.params, body))
        case _ => None
    }

  def extractExactCollectParamss(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(Seq[Seq[q.reflect.ValDef]], q.reflect.Term)] =
    extractExactCollectClauses(target).map { (clauses, body) =>
      (clauses.map(_.params), body)
    }

  def extractCapturedNameParamssResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
    extractAdmittedDefinition(target).map { (clauses, result, body) =>
      (target.name, clauses.map(_.params), result, body)
    }

  private def extractExactCollectClauses(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TermParamClause], q.reflect.Term)] =
    extractAdmittedDefinition(target).flatMap { (clauses, result, body) =>
      Option.when(
        target.name == "collect" &&
          TargetTypeReprInspector
            .inspect(result)
            .contains(TypeNormalForm.STypeIdent("Int"))
      )((clauses, body))
    }

  private def extractAdmittedDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TermParamClause], q.reflect.TypeRepr, q.reflect.Term)] =
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
        .when(admittedClauses && admittedParameters)(target.returnTpt.tpe)
        .flatMap(result => target.rhs.map(body => (clauses, result, body)))
