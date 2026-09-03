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

private[matching] object RankedDefinitionPatternMatcher:
  def extractExactCollect(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(Seq[q.reflect.ValDef], q.reflect.Term)] =
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
      target.paramss match
        case List(clause: TermParamClause)
            if !clause.isImplicit && !clause.isGiven && !clause.isErased =>
          val parameters = clause.params
          val symbols = parameters.map(_.symbol)
          val admittedParameters =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  !parameter.symbol.flags.is(Flags.HasDefault) &&
                  !parameter.symbol.flags.is(Flags.Erased)
              ) &&
              target.symbol.paramSymss == List(symbols)

          Option
            .when(
              admittedParameters &&
                TargetTypeReprInspector
                  .inspect(target.returnTpt.tpe)
                  .contains(TypeNormalForm.STypeIdent("Int"))
            )(())
            .flatMap(_ => target.rhs.map(body => (parameters, body)))
        case _ => None
