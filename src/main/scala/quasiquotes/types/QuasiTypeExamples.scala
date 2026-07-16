package quasiquotes.types

import scala.quoted.*

object QuasiTypeExamples:
  inline def supportedConstructionSummary: List[String] =
    ${ supportedConstructionSummaryImpl }

  inline def matches(patternSource: String, targetSource: String): Boolean =
    ${ matchesImpl('patternSource, 'targetSource) }

  inline def matchSummary(patternSource: String, targetSource: String): String =
    ${ matchSummaryImpl('patternSource, 'targetSource) }

  inline def structuralNormalFormSummary(source: String): String =
    ${ structuralNormalFormSummaryImpl('source) }

  inline def structuralMatches(patternSource: String, targetSource: String): Boolean =
    ${ structuralMatchesImpl('patternSource, 'targetSource) }

  inline def equalityComparisonSummary(patternSource: String, targetSource: String): String =
    ${ equalityComparisonSummaryImpl('patternSource, 'targetSource) }

  inline def matchingSubstrateSummary(patternSource: String): String =
    ${ matchingSubstrateSummaryImpl('patternSource) }

  inline def targetNormalFormSummary(source: String): String =
    ${ targetNormalFormSummaryImpl('source) }

  inline def targetInspectionComparisonSummary(patternSource: String, targetSource: String): String =
    ${ targetInspectionComparisonSummaryImpl('patternSource, 'targetSource) }

  inline def typePatternMatchSummary(patternSource: String, targetSource: String): String =
    ${ typePatternMatchSummaryImpl('patternSource, 'targetSource) }

  inline def tqqTypePatternMatchSummary(patternSource: String, targetSource: String): String =
    ${ tqqTypePatternMatchSummaryImpl('patternSource, 'targetSource) }

  inline def tqqEquivalenceSummary(patternSource: String, targetSource: String): String =
    ${ tqqEquivalenceSummaryImpl('patternSource, 'targetSource) }

  inline def patternAliasEquivalenceSummary(patternSource: String, targetSource: String): String =
    ${ patternAliasEquivalenceSummaryImpl('patternSource, 'targetSource) }

  inline def typeConstructionDualitySummary(patternSource: String, targetSource: String): String =
    ${ typeConstructionDualitySummaryImpl('patternSource, 'targetSource) }

  inline def constructedTypeReprRoundtripSummary(templateSource: String, bindingName: String, bindingSource: String): String =
    ${ constructedTypeReprRoundtripSummaryImpl('templateSource, 'bindingName, 'bindingSource) }

  inline def constructedTypeReprRoundtripSummary(
      templateSource: String,
      firstBindingName: String,
      firstBindingSource: String,
      secondBindingName: String,
      secondBindingSource: String
  ): String =
    ${ constructedTypeReprRoundtripSummaryImpl('templateSource, 'firstBindingName, 'firstBindingSource, 'secondBindingName, 'secondBindingSource) }

  inline def constructedTypeReprLoweringMessage(templateSource: String, bindingName: String, bindingSource: String): String =
    ${ constructedTypeReprLoweringMessageImpl('templateSource, 'bindingName, 'bindingSource) }

  inline def normalFormLoweringMessage(source: String): String =
    ${ normalFormLoweringMessageImpl('source) }

  inline def rawIdentifierLoweringMessage(name: String): String =
    ${ rawIdentifierLoweringMessageImpl('name) }

  inline def rawAppliedLoweringMessage(constructorName: String, argumentName: String): String =
    ${ rawAppliedLoweringMessageImpl('constructorName, 'argumentName) }

  inline def rawTupleArityLoweringMessage: String =
    ${ rawTupleArityLoweringMessageImpl }

  inline def rawFunctionArityLoweringMessage: String =
    ${ rawFunctionArityLoweringMessageImpl }

  inline def typeConstructionLoweringDualitySummary(patternSource: String, targetSource: String): String =
    ${ typeConstructionLoweringDualitySummaryImpl('patternSource, 'targetSource) }

  inline def typePatternBindingSummary(patternSource: String, targetSource: String, bindingName: String): String =
    ${ typePatternBindingSummaryImpl('patternSource, 'targetSource, 'bindingName) }

  inline def unsupportedMessage(source: String): String =
    ${ unsupportedMessageImpl('source) }

  private def supportedConstructionSummaryImpl(using Quotes): Expr[List[String]] =
    import quotes.reflect.*
    val sources = List("Int", "String", "Boolean", "List[Int]", "Option[String]", "(Int, String)", "Int => String")
    val rendered = sources.map { source =>
      QuasiTypeRepr.fromSource(source) match
        case Right(quasiType) => s"$source -> ${quasiType.renderedTypeRepr}"
        case Left(error) => s"$source -> ERROR: ${error.message}"
    }
    Expr.ofList(rendered.map(Expr(_)))

  private def matchesImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[Boolean] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val result = QuasiTypePattern.matchesSource(patternText, targetText)
    Expr(result.getOrElse(false))

  private def matchSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val operator = if QuasiTypePattern.matchesSource(patternText, targetText).getOrElse(false) then "==" else "!="
    Expr(s"$patternText $operator $targetText")

  private def structuralNormalFormSummaryImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    Expr(TypeNormalForm.fromSource(sourceText).fold(_.message, _.render))

  private def structuralMatchesImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[Boolean] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    Expr(TypeNormalForm.equalSources(patternText, targetText).getOrElse(false))

  private def equalityComparisonSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val exact = QuasiTypePattern.matchesSourceByRenderedTypeRepr(patternText, targetText).getOrElse(false)
    val structural = TypeNormalForm.equalSources(patternText, targetText).getOrElse(false)
    Expr(s"exact=$exact structural=$structural")

  private def matchingSubstrateSummaryImpl(patternSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val pattern = QuasiTypePattern.reprOrThrow(patternText)
    Expr(pattern.matchingSubstrateSummary)

  private def targetNormalFormSummaryImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    val inspected =
      for
        quasiType <- QuasiTypeRepr.fromSource(sourceText)
        targetRepr <- TypeReprLowerer.lower(quasiType.shape)
        targetNormalForm <- TargetTypeReprInspector.inspect(targetRepr)
      yield targetNormalForm
    Expr(inspected.fold(_.message, _.render))

  private def targetInspectionComparisonSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val inspected =
      for
        sourceNormalForm <- TypeNormalForm.fromSource(patternText)
        targetQuasiType <- QuasiTypeRepr.fromSource(targetText)
        targetRepr <- TypeReprLowerer.lower(targetQuasiType.shape)
        targetNormalForm <- TargetTypeReprInspector.inspect(targetRepr)
      yield (sourceNormalForm, targetNormalForm)
    Expr(inspected.fold(_.message, (sourceNormalForm, targetNormalForm) =>
      s"source=${sourceNormalForm.render} target=${targetNormalForm.render} matched=${sourceNormalForm == targetNormalForm}"
    ))

  private def typePatternMatchSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val summary =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        result <- pattern.matchSource(targetText)
      yield result match
        case Some(matchResult) if matchResult.bindings.nonEmpty => s"matched=true bindings=${matchResult.bindingsSummary}"
        case Some(_) => "matched=true bindings="
        case None => "matched=false"
    Expr(summary.fold(_.message, identity))

  private def tqqTypePatternMatchSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val summary =
      for
        pattern <- QuasiTypequotes.tqq(patternText)
        result <- pattern.matchSource(targetText)
      yield result match
        case Some(matchResult) if matchResult.bindings.nonEmpty => s"matched=true bindings=${matchResult.bindingsSummary}"
        case Some(_) => "matched=true bindings="
        case None => "matched=false"
    Expr(summary.fold(_.message, identity))

  private def tqqEquivalenceSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val explicit =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        result <- pattern.matchSource(targetText)
      yield result.map(_.bindingsSummary).getOrElse("no-match")
    val wrapped =
      for
        pattern <- QuasiTypequotes.tqq(patternText)
        result <- pattern.matchSource(targetText)
      yield result.map(_.bindingsSummary).getOrElse("no-match")
    Expr(s"explicit=${explicit.fold(_.message, identity)} tqq=${wrapped.fold(_.message, identity)}")

  private def patternAliasEquivalenceSummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    def bindingsSummary(result: Either[TypeQuasiquoteError, QuasiTypePattern]): String =
      (for
        pattern <- result
        matched <- pattern.matchSource(targetText)
      yield matched.map(_.bindingsSummary).getOrElse("no-match")).fold(_.message, identity)
    val canonical = bindingsSummary(QuasiTypePattern.pattern(patternText))
    val compatibility = bindingsSummary(QuasiTypePattern.repr(patternText))
    Expr(s"pattern=$canonical repr=$compatibility")

  private def typeConstructionDualitySummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val summary =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        result <- pattern.matchSource(targetText)
        matchResult <- result.toRight(TypeQuasiquoteError("type pattern did not match target"))
        constructed <- QuasiTypeConstruct.fromTemplate(patternText, matchResult.bindings)
      yield constructed.source
    Expr(summary.fold(_.message, identity))

  private def constructedTypeReprRoundtripSummaryImpl(templateSource: Expr[String], bindingName: Expr[String], bindingSource: Expr[String])(using Quotes): Expr[String] =
    val templateText = templateSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val bindingSourceText = bindingSource.valueOrAbort
    val summary =
      for
        binding <- TypeNormalForm.fromSource(bindingSourceText)
        constructed <- QuasiTypequotes.tqr(templateText, bindingText -> binding)
        lowered <- constructed.toTypeRepr
        inspected <- TargetTypeReprInspector.inspect(lowered)
      yield s"constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"
    Expr(summary.fold(_.message, identity))

  private def constructedTypeReprRoundtripSummaryImpl(
      templateSource: Expr[String],
      firstBindingName: Expr[String],
      firstBindingSource: Expr[String],
      secondBindingName: Expr[String],
      secondBindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    val templateText = templateSource.valueOrAbort
    val firstBindingText = firstBindingName.valueOrAbort
    val firstBindingSourceText = firstBindingSource.valueOrAbort
    val secondBindingText = secondBindingName.valueOrAbort
    val secondBindingSourceText = secondBindingSource.valueOrAbort
    val summary =
      for
        firstBinding <- TypeNormalForm.fromSource(firstBindingSourceText)
        secondBinding <- TypeNormalForm.fromSource(secondBindingSourceText)
        constructed <- QuasiTypequotes.tqr(templateText, firstBindingText -> firstBinding, secondBindingText -> secondBinding)
        lowered <- QuasiTypeConstruct.toTypeRepr(constructed)
        inspected <- TargetTypeReprInspector.inspect(lowered)
      yield s"constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"
    Expr(summary.fold(_.message, identity))

  private def constructedTypeReprLoweringMessageImpl(templateSource: Expr[String], bindingName: Expr[String], bindingSource: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val templateText = templateSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val bindingSourceText = bindingSource.valueOrAbort
    val summary =
      for
        binding <- TypeNormalForm.fromSource(bindingSourceText)
        constructed <- QuasiTypequotes.tqr(templateText, bindingText -> binding)
        lowered <- constructed.toTypeRepr
      yield lowered.show
    Expr(summary.fold(_.message, identity))

  private def normalFormLoweringMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val sourceText = source.valueOrAbort
    val summary =
      for
        normalForm <- TypeNormalForm.fromSource(sourceText)
        lowered <- TypeReprLowerer.lowerNormalForm(normalForm)
      yield lowered.show
    Expr(summary.fold(_.message, identity))

  private def rawIdentifierLoweringMessageImpl(name: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val nameText = name.valueOrAbort
    val summary = TypeReprLowerer.lowerNormalForm(TypeNormalForm.STypeIdent(nameText)).map(_.show)
    Expr(summary.fold(_.message, identity))

  private def rawAppliedLoweringMessageImpl(constructorName: Expr[String], argumentName: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val constructorText = constructorName.valueOrAbort
    val argumentText = argumentName.valueOrAbort
    val normalForm = TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(constructorText), List(TypeNormalForm.STypeIdent(argumentText)))
    val summary = TypeReprLowerer.lowerNormalForm(normalForm).map(_.show)
    Expr(summary.fold(_.message, identity))

  private def rawTupleArityLoweringMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*

    val normalForm = TypeNormalForm.STypeTuple(List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"), TypeNormalForm.STypeIdent("Boolean")))
    val summary = TypeReprLowerer.lowerNormalForm(normalForm).map(_.show)
    Expr(summary.fold(_.message, identity))

  private def rawFunctionArityLoweringMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*

    val normalForm = TypeNormalForm.STypeFunction(List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String")), TypeNormalForm.STypeIdent("Boolean"))
    val summary = TypeReprLowerer.lowerNormalForm(normalForm).map(_.show)
    Expr(summary.fold(_.message, identity))

  private def typeConstructionLoweringDualitySummaryImpl(patternSource: Expr[String], targetSource: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val summary =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        result <- pattern.matchSource(targetText)
        matchResult <- result.toRight(TypeQuasiquoteError("type pattern did not match target"))
        constructed <- QuasiTypeConstruct.fromTemplate(patternText, matchResult.bindings)
        lowered <- constructed.toTypeRepr
        inspected <- TargetTypeReprInspector.inspect(lowered)
      yield s"constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"
    Expr(summary.fold(_.message, identity))

  private def typePatternBindingSummaryImpl(patternSource: Expr[String], targetSource: Expr[String], bindingName: Expr[String])(using Quotes): Expr[String] =
    val patternText = patternSource.valueOrAbort
    val targetText = targetSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val summary =
      for
        pattern <- QuasiTypePattern.repr(patternText)
        result <- pattern.matchSource(targetText)
      yield result.flatMap(_.binding(bindingText)).fold("matched=false")(_.render)
    Expr(summary.fold(_.message, identity))

  private def unsupportedMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    QuasiTypeRepr.fromSource(sourceText) match
      case Left(error) => Expr(error.message)
      case Right(quasiType) => Expr(s"Unexpectedly supported ${quasiType.renderedTypeRepr}")
