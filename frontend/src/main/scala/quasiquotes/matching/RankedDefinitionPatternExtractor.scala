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

  private def extractExactCollectClauses(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TermParamClause], q.reflect.Term)] =
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
        .when(
          admittedClauses &&
            admittedParameters &&
            TargetTypeReprInspector
              .inspect(target.returnTpt.tpe)
              .contains(TypeNormalForm.STypeIdent("Int"))
        )(())
        .flatMap(_ => target.rhs.map(body => (clauses, body)))
