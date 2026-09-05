package quasiquotes.matching

import scala.quoted.*

object Q030GenericNamedUsingCandidateFactory:
  def capturedModifiers(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.TypeDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
      admitted(target).map { (tparams, parameters, result, body) =>
        val modifiers = new DefinitionModifiers(
          target.symbol.flags,
          target.symbol.privateWithin,
          target.symbol.protectedWithin,
          target.symbol.annotations
        )
        (modifiers, target.name, tparams, parameters, result, body)
      }
    )

  def semanticEmpty(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.TypeDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor((target: q.reflect.DefDef) =>
      admitted(target)
        .filter(_ => DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol))
        .map { (tparams, parameters, result, body) =>
          (target.name, tparams, parameters, result, body)
        }
    )

  private def admitted(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.TypeDef], List[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
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
        case List(typeClause: TypeParamClause, clause: TermParamClause)
            if typeClause.params.nonEmpty &&
              clause.isGiven &&
              !clause.isImplicit &&
              !clause.isErased &&
              clause.params.nonEmpty =>
          val tparams = typeClause.params
          val parameters = clause.params
          val typeSymbols = tparams.map(_.symbol)
          val parameterSymbols = parameters.map(_.symbol)
          val admittedTypeParameters =
            typeSymbols.forall(_ != Symbol.noSymbol) &&
              typeSymbols.distinct.size == typeSymbols.size &&
              tparams.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.annotations.isEmpty &&
                  !parameter.symbol.flags.is(Flags.Covariant) &&
                  !parameter.symbol.flags.is(Flags.Contravariant) &&
                  (parameter.rhs match
                    case _: TypeBoundsTree => true
                    case _ => false)
              )
          val admittedParameters =
            parameterSymbols.forall(_ != Symbol.noSymbol) &&
              parameterSymbols.distinct.size == parameterSymbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              target.symbol.paramSymss == List(typeSymbols, parameterSymbols)

          Option
            .when(admittedTypeParameters && admittedParameters)(target.returnTpt.tpe)
            .flatMap(result => target.rhs.map(body => (tparams, parameters, result, body)))
        case _ => None
