package quasiquotes.q027

import scala.quoted.*

import quasiquotes.matching.RankedDefinitionPatternExtractor

enum Q027ClauseMode:
  case Ordinary, Contextual, Scala2Implicit, Erased

final class DefinitionParameterClause[Mode, Parameter](
    val mode: Mode,
    val parameters: Seq[Parameter]
)

object Q027CandidateFactory:
  def nested(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[Seq[q.reflect.ValDef]]]
  ] = new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
    clauses(target).map(value => Tuple1(value.map(_.params)))
  )

  def native(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[q.reflect.TermParamClause]]
  ] = new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
    clauses(target).map(Tuple1(_))
  )

  def structured(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[DefinitionParameterClause[Q027ClauseMode, q.reflect.ValDef]]]
  ] =
    new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
      clauses(target).map(value =>
        Tuple1(value.map(clause =>
          new DefinitionParameterClause(nativeMode(clause), clause.params)
        ))
      )
    )

  def namedUsing(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    Tuple1[Seq[q.reflect.ValDef]]
  ] =
    new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
      clauses(target).flatMap {
        case clause :: Nil
            if target.paramss.size == 1 &&
              clause.isGiven &&
              clause.params.nonEmpty &&
              clause.params.forall(parameter =>
                parameter.symbol.flags.is(q.reflect.Flags.Given) &&
                  !parameter.symbol.flags.is(q.reflect.Flags.Synthetic)
              ) =>
          Some(Tuple1(clause.params))
        case _ => None
      }
    )

  def ordinaryThenNamedUsing(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], Seq[q.reflect.ValDef])
  ] =
    new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
      clauses(target).flatMap {
        case ordinary :: contextual :: Nil
            if target.paramss.size == 2 &&
              nativeMode(ordinary) == Q027ClauseMode.Ordinary &&
              contextual.isGiven &&
              contextual.params.nonEmpty &&
              contextual.params.forall(parameter =>
                parameter.symbol.flags.is(q.reflect.Flags.Given) &&
                  !parameter.symbol.flags.is(q.reflect.Flags.Synthetic)
              ) =>
          Some((ordinary.params, contextual.params))
        case _ => None
      }
    )

  def modeFromParameters(using q: Quotes)(
      parameters: Seq[q.reflect.ValDef]
  ): Q027ClauseMode =
    import q.reflect.Flags
    if parameters.exists(_.symbol.flags.is(Flags.Erased)) then Q027ClauseMode.Erased
    else if parameters.exists(_.symbol.flags.is(Flags.Given)) then Q027ClauseMode.Contextual
    else if parameters.exists(_.symbol.flags.is(Flags.Implicit)) then Q027ClauseMode.Scala2Implicit
    else Q027ClauseMode.Ordinary

  def nativeMode(using q: Quotes)(
      clause: q.reflect.TermParamClause
  ): Q027ClauseMode =
    import q.reflect.Flags
    if clause.params.exists(_.symbol.flags.is(Flags.Erased)) then Q027ClauseMode.Erased
    else if clause.isGiven then Q027ClauseMode.Contextual
    else if clause.isImplicit then Q027ClauseMode.Scala2Implicit
    else Q027ClauseMode.Ordinary

  private def clauses(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[List[q.reflect.TermParamClause]] =
    import q.reflect.*

    if target == null ||
        target.symbol == Symbol.noSymbol ||
        !target.symbol.isDefDef ||
        target.rhs.isEmpty
    then None
    else
      val (typeSymbols, rawTerms) = target.paramss match
        case (types: TypeParamClause) :: tail =>
          (types.params.map(_.symbol), tail)
        case clauses => (Nil, clauses)
      val terms = rawTerms.collect { case clause: TermParamClause => clause }
      val parameters = terms.flatMap(_.params)
      val symbols = parameters.map(_.symbol)
      val expectedParamSymss =
        Option.when(typeSymbols.nonEmpty)(typeSymbols).toList ++
          terms.map(_.params.map(_.symbol))

      Option.when(
        terms.size == rawTerms.size &&
          symbols.forall(_ != Symbol.noSymbol) &&
          symbols.distinct.size == symbols.size &&
          parameters.forall(parameter =>
            parameter.symbol.owner == target.symbol &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ) &&
          target.symbol.paramSymss == expectedParamSymss
      )(terms)
