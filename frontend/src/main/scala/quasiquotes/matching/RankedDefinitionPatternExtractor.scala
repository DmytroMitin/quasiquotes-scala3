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

  def capturedModifiersNameOrdinaryParamsResult(using q: Quotes): RankedDefinitionPatternExtractor[
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
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameOrdinaryParamsResult(target)
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

  def capturedModifiersNameMixedOrdinaryNamedUsingParamsResult(using
      q: Quotes
  ): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameMixedOrdinaryNamedUsingParamsResult(
        target
      )
    )

  def capturedNameMixedOrdinaryNamedUsingParamsResult(using
      q: Quotes
  ): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedNameMixedOrdinaryNamedUsingParamsResult(
        target
      )
    )

  def capturedModifiersNameMixedOrdinaryScala2ImplicitParamsResult(using
      q: Quotes
  ): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    new RankedDefinitionPatternExtractor(target =>
      RankedDefinitionPatternMatcher.extractCapturedModifiersNameMixedOrdinaryScala2ImplicitParamsResult(
        target
      )
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

  def extractCapturedModifiersNameOrdinaryParamsResult(using q: Quotes)(
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
    extractAdmittedOrdinaryRank2Definition(target).map { (parameters, result, body) =>
      val modifiers = new DefinitionModifiers(
        target.symbol.flags,
        target.symbol.privateWithin,
        target.symbol.protectedWithin,
        target.symbol.annotations
      )
      (modifiers, target.name, parameters, result, body)
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

  def extractCapturedModifiersNameMixedOrdinaryNamedUsingParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedMixedOrdinaryNamedUsingDefinition(target).map {
      (ordinaryParameters, usingParameters, result, body) =>
        val modifiers = new DefinitionModifiers(
          target.symbol.flags,
          target.symbol.privateWithin,
          target.symbol.protectedWithin,
          target.symbol.annotations
        )
        (modifiers, target.name, ordinaryParameters, usingParameters, result, body)
    }

  def extractCapturedNameMixedOrdinaryNamedUsingParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedMixedOrdinaryNamedUsingDefinition(target)
      .filter(_ => DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol))
      .map { (ordinaryParameters, usingParameters, result, body) =>
        (target.name, ordinaryParameters, usingParameters, result, body)
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

  def extractCapturedModifiersNameMixedOrdinaryScala2ImplicitParamsResult(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    extractAdmittedMixedOrdinaryScala2ImplicitDefinition(target).map {
      (ordinaryParameters, implicitParameters, result, body) =>
        val modifiers = new DefinitionModifiers(
          target.symbol.flags,
          target.symbol.privateWithin,
          target.symbol.protectedWithin,
          target.symbol.annotations
        )
        (modifiers, target.name, ordinaryParameters, implicitParameters, result, body)
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

    if !isAdmittedNormalMethod(target) then None
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

  private def extractAdmittedOrdinaryRank2Definition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
    import q.reflect.*

    if !isAdmittedNormalMethod(target) then None
    else
      target.paramss match
        case List(clause: TermParamClause)
            if !clause.isImplicit && !clause.isGiven && !clause.isErased =>
          target.symbol.termRef.widen match
            case method: MethodType if method.paramTypes.size == clause.params.size =>
              val parameters = clause.params
              val symbols = parameters.map(_.symbol)
              val admittedParameters =
                symbols.forall(_ != Symbol.noSymbol) &&
                  symbols.distinct.size == symbols.size &&
                  parameters.forall(parameter =>
                    parameter.symbol.owner == target.symbol &&
                      parameter.symbol.flags.is(Flags.Param) &&
                      parameter.symbol.annotations.isEmpty &&
                      parameter.rhs.isEmpty &&
                      !parameter.symbol.flags.is(Flags.Implicit) &&
                      !parameter.symbol.flags.is(Flags.Given) &&
                      !parameter.symbol.flags.is(Flags.Synthetic) &&
                      !parameter.symbol.flags.is(Flags.Erased) &&
                      !parameter.symbol.flags.is(Flags.HasDefault)
                  ) &&
                  parameters.zip(method.paramTypes).zipWithIndex.forall {
                    case ((parameter, methodType), index) =>
                      isCoherentOrdinaryRank2Parameter(
                        parameter,
                        methodType,
                        index == parameters.size - 1
                      )
                  } &&
                  target.symbol.paramSymss == List(symbols)

              Option
                .when(admittedParameters)(target.returnTpt.tpe)
                .flatMap(result => target.rhs.map(body => (parameters, result, body)))
            case _ => None
        case _ => None

  private def isCoherentOrdinaryRank2Parameter(using q: Quotes)(
      parameter: q.reflect.ValDef,
      methodType: q.reflect.TypeRepr,
      isFinal: Boolean
  ): Boolean =
    import q.reflect.*

    def byNameElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case ByNameType(element) => Some(element)
      case _ => None

    def treeRepeatedElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AnnotatedType(AppliedType(_, List(element)), annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot => Some(element)
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    def methodRepeatedElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    val treeType = parameter.tpt.tpe
    val treeByName = byNameElement(treeType)
    val methodByName = byNameElement(methodType)
    val treeRepeated = treeRepeatedElement(treeType)
    val methodRepeated = methodRepeatedElement(methodType)

    (treeByName, methodByName, treeRepeated, methodRepeated) match
      case (Some(treeElement), Some(methodElement), None, None) =>
        treeElement =:= methodElement && parameter.symbol.termRef.widen =:= treeElement
      case (None, None, Some(treeElement), Some(methodElement)) =>
        isFinal && treeElement =:= methodElement
      case (None, None, None, None) =>
        treeType =:= methodType && parameter.symbol.termRef.widen =:= treeType
      case _ => false

  private def extractAdmittedScala2ImplicitDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[(List[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)] =
    import q.reflect.*

    if !isAdmittedNormalMethod(target) then None
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

  private def extractAdmittedMixedOrdinaryNamedUsingDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      List[q.reflect.ValDef],
      List[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    import q.reflect.*

    if !isAdmittedNormalMethod(target) then None
    else
      target.paramss match
        case List(ordinaryClause: TermParamClause, usingClause: TermParamClause)
            if !ordinaryClause.isImplicit &&
              !ordinaryClause.isGiven &&
              !ordinaryClause.isErased &&
              usingClause.isGiven &&
              !usingClause.isImplicit &&
              !usingClause.isErased &&
              usingClause.params.nonEmpty =>
          val ordinaryParameters = ordinaryClause.params
          val usingParameters = usingClause.params
          val ordinarySymbols = ordinaryParameters.map(_.symbol)
          val usingSymbols = usingParameters.map(_.symbol)
          val allSymbols = ordinarySymbols ++ usingSymbols
          val admittedParameters =
            allSymbols.forall(_ != Symbol.noSymbol) &&
              allSymbols.distinct.size == allSymbols.size &&
              ordinaryParameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              usingParameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              target.symbol.paramSymss == List(ordinarySymbols, usingSymbols)

          Option
            .when(admittedParameters)(target.returnTpt.tpe)
            .flatMap(result =>
              target.rhs.map(body => (ordinaryParameters, usingParameters, result, body))
            )
        case _ => None

  private def extractAdmittedMixedOrdinaryScala2ImplicitDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    (
      List[q.reflect.ValDef],
      List[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    import q.reflect.*

    if !isAdmittedNormalMethod(target) then None
    else
      target.paramss match
        case List(ordinaryClause: TermParamClause, implicitClause: TermParamClause)
            if !ordinaryClause.isImplicit &&
              !ordinaryClause.isGiven &&
              !ordinaryClause.isErased &&
              implicitClause.isImplicit &&
              !implicitClause.isGiven &&
              !implicitClause.isErased &&
              implicitClause.params.nonEmpty =>
          val ordinaryParameters = ordinaryClause.params
          val implicitParameters = implicitClause.params
          val ordinarySymbols = ordinaryParameters.map(_.symbol)
          val implicitSymbols = implicitParameters.map(_.symbol)
          val allSymbols = ordinarySymbols ++ implicitSymbols
          val admittedParameters =
            allSymbols.forall(_ != Symbol.noSymbol) &&
              allSymbols.distinct.size == allSymbols.size &&
              ordinaryParameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.annotations.isEmpty &&
                  isStrictNonRepeatedParameter(parameter) &&
                  !parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              implicitParameters.forall(parameter =>
                parameter.symbol.owner == target.symbol &&
                  parameter.symbol.annotations.isEmpty &&
                  isStrictNonRepeatedParameter(parameter) &&
                  parameter.symbol.flags.is(Flags.Implicit) &&
                  !parameter.symbol.flags.is(Flags.Given) &&
                  !parameter.symbol.flags.is(Flags.Synthetic) &&
                  !parameter.symbol.flags.is(Flags.Erased) &&
                  !parameter.symbol.flags.is(Flags.HasDefault)
              ) &&
              target.symbol.paramSymss == List(ordinarySymbols, implicitSymbols)

          Option
            .when(admittedParameters)(target.returnTpt.tpe)
            .flatMap(result =>
              target.rhs.map(body =>
                (ordinaryParameters, implicitParameters, result, body)
              )
            )
        case _ => None

  private def isStrictNonRepeatedParameter(using q: Quotes)(
      parameter: q.reflect.ValDef
  ): Boolean =
    import q.reflect.*

    parameter.tpt.tpe match
      case _: ByNameType => false
      case AnnotatedType(AppliedType(_, List(_)), annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot => false
      case AppliedType(constructor, List(_))
          if constructor.typeSymbol == defn.RepeatedParamClass => false
      case _ => true

  private def isAdmittedNormalMethod(using q: Quotes)(target: q.reflect.DefDef): Boolean =
    import q.reflect.*

    target != null &&
      target.symbol != Symbol.noSymbol &&
      target.symbol.isDefDef &&
      !target.symbol.isClassConstructor &&
      !target.symbol.flags.is(Flags.ExtensionMethod) &&
      !target.symbol.flags.is(Flags.FieldAccessor) &&
      !target.symbol.flags.is(Flags.ParamAccessor) &&
      !target.symbol.flags.is(Flags.CaseAccessor) &&
      !target.symbol.flags.is(Flags.Given)

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
