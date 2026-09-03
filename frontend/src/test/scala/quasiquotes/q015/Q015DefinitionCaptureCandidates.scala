package quasiquotes.q015

import scala.quoted.*

import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

/** Test-only analogue of the Q002 ranked product extractor. */
final class RankedDefinitionPatternExtractor[Target, Captures <: Tuple](
    extract: Target => Option[Captures]
):
  def unapply(value: Target): Option[Captures] = extract(value)

/** Test-only analogue for measuring a generic replacement of the accepted carrier. */
final class GeneralizedDefinitionPatternExtractor[Target, Captures <: Tuple](
    extract: Target => Option[Captures]
):
  def unapply(value: Target): Option[Captures] = extract(value)

/** Test-only abstract-member analogue for the refined-carrier strategy. */
trait RefinedDefinitionPatternExtractor:
  type Target
  type Captures <: Tuple
  def unapply(value: Target): Option[Captures]

object Q015DefinitionSequenceMatcher:
  def extract(using q: Quotes)(
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
          Option.when(
            admittedParameters &&
              TargetTypeReprInspector
                .inspect(target.returnTpt.tpe)
                .contains(TypeNormalForm.STypeIdent("Int"))
          )(()).flatMap(_ => target.rhs.map(body => (parameters, body)))
        case _ => None

object Q015StrategyAFactory:
  transparent inline def extractor(using q: Quotes) =
    new:
      def unapply(
          target: q.reflect.DefDef
      ): Option[(Seq[q.reflect.ValDef], q.reflect.Term)] =
        Q015DefinitionSequenceMatcher.extract(target)

object Q015StrategyBFactory:
  def extractor(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      Q015DefinitionSequenceMatcher.extract(target)
    )

object Q015StrategyCFactory:
  def extractor(using q: Quotes): GeneralizedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], q.reflect.Term)
  ] =
    new GeneralizedDefinitionPatternExtractor(target =>
      Q015DefinitionSequenceMatcher.extract(target)
    )

object Q015StrategyDFactory:
  def extractor(using q: Quotes): RefinedDefinitionPatternExtractor {
    type Target = q.reflect.DefDef
    type Captures = (Seq[q.reflect.ValDef], q.reflect.Term)
  } =
    new RefinedDefinitionPatternExtractor:
      type Target = q.reflect.DefDef
      type Captures = (Seq[q.reflect.ValDef], q.reflect.Term)
      def unapply(value: Target): Option[Captures] =
        Q015DefinitionSequenceMatcher.extract(value)
