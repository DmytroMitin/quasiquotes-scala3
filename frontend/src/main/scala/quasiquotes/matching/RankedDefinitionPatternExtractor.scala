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

  def capturedModifiersNameParamssResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameParamssResult(target)
    )

  def capturedModifiersNameNamedUsingParamsResult(using q: Quotes): RankedDefinitionPatternExtractor[
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
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameNamedUsingParamsResult(target)
    )

  def capturedNameNamedUsingParamsResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedNameNamedUsingParamsResult(target)
    )

  def capturedModifiersNameScala2ImplicitParamsResult(using q: Quotes): RankedDefinitionPatternExtractor[
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
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameScala2ImplicitParamsResult(target)
    )

  def capturedNameScala2ImplicitParamsResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedNameScala2ImplicitParamsResult(target)
    )

  def capturedNameTypeParamsParamssResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedNameTypeParamsParamssResult(target)
    )

  def capturedModifiersNameTypeParamsParamssResult(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameTypeParamsParamssResult(target)
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

  def extractCapturedModifiersNameParamssResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedDefinitionCore(target).map { (clauses, result, body) =>
      val modifiers = new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
      (modifiers, target.name, clauses.map(_.params), result, body)
    }

  def extractCapturedModifiersNameNamedUsingParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedNamedUsingDefinition(target).map { (parameters, result, body) =>
      val modifiers = new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
      (modifiers, target.name, parameters, result, body)
    }

  def extractCapturedNameNamedUsingParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
    extractAdmittedNamedUsingDefinition(target)
      .filter(_ => DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol))
      .map { (parameters, result, body) =>
        (target.name, parameters, result, body)
      }

  def extractCapturedModifiersNameScala2ImplicitParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedScala2ImplicitDefinition(target).map { (parameters, result, body) =>
      val modifiers = new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
      (modifiers, target.name, parameters, result, body)
    }

  def extractCapturedNameScala2ImplicitParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
    extractAdmittedScala2ImplicitDefinition(target)
      .filter(_ => DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol))
      .map { (parameters, result, body) =>
        (target.name, parameters, result, body)
      }

  def extractCapturedNameTypeParamsParamssResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedGenericDefinition(target).map { (tparams, clauses, result, body) =>
      (target.name, tparams, clauses.map(_.params), result, body)
    }

  def extractCapturedModifiersNameTypeParamsParamssResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedGenericDefinitionCore(target).map { (tparams, clauses, result, body) =>
      val modifiers = new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
      (modifiers, target.name, tparams, clauses.map(_.params), result, body)
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
    extractAdmittedDefinitionCore(target).filter(_ =>
      DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol)
    )

  private def extractAdmittedNamedUsingDefinition(using q: Quotes)(
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
            if clause.isGiven && !clause.isImplicit && !clause.isErased && clause.params.nonEmpty =>
          val parameters = clause.params
          val symbols = parameters.map(_.symbol)
          val admittedParameters =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              target.symbol.paramSymss == List(symbols)

          Option
            .when(admittedParameters)(target.returnTpt.tpe)
            .flatMap(result => target.rhs.map(body => (parameters, result, body)))
        case _ => None

  private def extractAdmittedScala2ImplicitDefinition(using q: Quotes)(
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
            if clause.isImplicit && !clause.isGiven && !clause.isErased && clause.params.nonEmpty =>
          val parameters = clause.params
          val symbols = parameters.map(_.symbol)
          val admittedParameters =
            symbols.forall(_ != Symbol.noSymbol) &&
              symbols.distinct.size == symbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              target.symbol.paramSymss == List(symbols)

          Option
            .when(admittedParameters)(target.returnTpt.tpe)
            .flatMap(result => target.rhs.map(body => (parameters, result, body)))
        case _ => None

  private def extractAdmittedDefinitionCore(using q: Quotes)(
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

  private def extractAdmittedGenericDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      List[q.reflect.TypeDef],
      List[q.reflect.TermParamClause],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedGenericDefinitionCore(target).filter(_ =>
      DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol)
    )

  private def extractAdmittedGenericDefinitionCore(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      List[q.reflect.TypeDef],
      List[q.reflect.TermParamClause],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
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
        case (typeClause: TypeParamClause) :: rawTermClauses
            if typeClause.params.nonEmpty =>
          val tparams = typeClause.params
          val clauses = rawTermClauses.collect { case clause: TermParamClause => clause }
          val typeSymbols = tparams.map(_.symbol)
          val parameters = clauses.flatMap(_.params)
          val parameterSymbols = parameters.map(_.symbol)
          val nestedSymbols = typeSymbols :: clauses.map(_.params.map(_.symbol))
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
          val admittedClauses =
            clauses.size == rawTermClauses.size &&
              clauses.forall(clause =>
                !clause.isImplicit && !clause.isGiven && !clause.isErased
              )
          val admittedParameters =
            parameterSymbols.forall(_ != Symbol.noSymbol) &&
              parameterSymbols.distinct.size == parameterSymbols.size &&
              parameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  !parameter.symbol.flags.is(Flags.HasDefault) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given)
              ) &&
              target.symbol.paramSymss == nestedSymbols

          Option
            .when(admittedTypeParameters && admittedClauses && admittedParameters)(
              target.returnTpt.tpe
            )
            .flatMap(result => target.rhs.map(body => (tparams, clauses, result, body)))
        case _ => None
