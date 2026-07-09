package quasiquotes.construct

import scala.quoted.*

import quasiquotes.types.*

object TypedTermConstructExamples:
  inline def typedAscriptionSummary(termKind: String, templateSource: String, bindingName: String, bindingSource: String): String =
    ${ typedAscriptionSummaryImpl('termKind, 'templateSource, 'bindingName, 'bindingSource) }

  inline def typedAscriptionSummary(
      termKind: String,
      templateSource: String,
      firstBindingName: String,
      firstBindingSource: String,
      secondBindingName: String,
      secondBindingSource: String
  ): String =
    ${ typedAscriptionSummaryImpl('termKind, 'templateSource, 'firstBindingName, 'firstBindingSource, 'secondBindingName, 'secondBindingSource) }

  inline def typedAscriptionMessage(templateSource: String, bindingName: String, bindingSource: String): String =
    ${ typedAscriptionMessageImpl('templateSource, 'bindingName, 'bindingSource) }

  inline def typedAscriptionMessage(templateSource: String, firstBindingName: String, firstBindingSource: String, secondBindingName: String, secondBindingSource: String): String =
    ${ typedAscriptionMessageImpl('templateSource, 'firstBindingName, 'firstBindingSource, 'secondBindingName, 'secondBindingSource) }

  inline def typedAscriptionMissingBindingMessage(templateSource: String): String =
    ${ typedAscriptionMissingBindingMessageImpl('templateSource) }

  inline def typedAscriptionUnsupportedNormalFormMessage(source: String): String =
    ${ typedAscriptionUnsupportedNormalFormMessageImpl('source) }

  private def typedAscriptionSummaryImpl(termKind: Expr[String], templateSource: Expr[String], bindingName: Expr[String], bindingSource: Expr[String])(using Quotes): Expr[String] =
    val termKindText = termKind.valueOrAbort
    val templateText = templateSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val bindingSourceText = bindingSource.valueOrAbort
    val summary =
      for
        binding <- TypeNormalForm.fromSource(bindingSourceText)
        constructed <- QuasiTypeConstruct.fromTemplate(templateText, bindingText -> binding)
        term <- termFor(termKindText)
        typed <- TypedTermConstruct.ascribe(term, constructed)
        typedView = typedTermSummary(typed)
        lowered <- constructed.toTypeRepr
        inspected <- TargetTypeReprInspector.inspect(lowered)
      yield s"term=$typedView constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"
    Expr(summary.fold(_.message, identity))

  private def typedAscriptionSummaryImpl(
      termKind: Expr[String],
      templateSource: Expr[String],
      firstBindingName: Expr[String],
      firstBindingSource: Expr[String],
      secondBindingName: Expr[String],
      secondBindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    val termKindText = termKind.valueOrAbort
    val templateText = templateSource.valueOrAbort
    val firstBindingText = firstBindingName.valueOrAbort
    val firstBindingSourceText = firstBindingSource.valueOrAbort
    val secondBindingText = secondBindingName.valueOrAbort
    val secondBindingSourceText = secondBindingSource.valueOrAbort
    val summary =
      for
        firstBinding <- TypeNormalForm.fromSource(firstBindingSourceText)
        secondBinding <- TypeNormalForm.fromSource(secondBindingSourceText)
        constructed <- QuasiTypeConstruct.fromTemplate(templateText, firstBindingText -> firstBinding, secondBindingText -> secondBinding)
        term <- termFor(termKindText)
        typed <- TypedTermConstruct.ascribe(term, constructed)
        typedView = typedTermSummary(typed)
        lowered <- constructed.toTypeRepr
        inspected <- TargetTypeReprInspector.inspect(lowered)
      yield s"term=$typedView constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"
    Expr(summary.fold(_.message, identity))

  private def typedAscriptionMessageImpl(templateSource: Expr[String], bindingName: Expr[String], bindingSource: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val templateText = templateSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val bindingSourceText = bindingSource.valueOrAbort
    val message =
      for
        binding <- TypeNormalForm.fromSource(bindingSourceText)
        term <- termFor("int")
        typed <- TypedTermConstruct.ascribeTemplate(term, templateText, bindingText -> binding)
      yield typed.show
    Expr(message.fold(_.message, identity))

  private def typedAscriptionMessageImpl(
      templateSource: Expr[String],
      firstBindingName: Expr[String],
      firstBindingSource: Expr[String],
      secondBindingName: Expr[String],
      secondBindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    import quotes.reflect.*

    val templateText = templateSource.valueOrAbort
    val firstBindingText = firstBindingName.valueOrAbort
    val firstBindingSourceText = firstBindingSource.valueOrAbort
    val secondBindingText = secondBindingName.valueOrAbort
    val secondBindingSourceText = secondBindingSource.valueOrAbort
    val message =
      for
        firstBinding <- TypeNormalForm.fromSource(firstBindingSourceText)
        secondBinding <- TypeNormalForm.fromSource(secondBindingSourceText)
        term <- termFor("int")
        typed <- TypedTermConstruct.ascribeTemplate(term, templateText, firstBindingText -> firstBinding, secondBindingText -> secondBinding)
      yield typed.show
    Expr(message.fold(_.message, identity))

  private def typedAscriptionMissingBindingMessageImpl(templateSource: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val templateText = templateSource.valueOrAbort
    val message =
      for
        term <- termFor("int")
        typed <- TypedTermConstruct.ascribeTemplate(term, templateText)
      yield typed.show
    Expr(message.fold(_.message, identity))

  private def typedAscriptionUnsupportedNormalFormMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val sourceText = source.valueOrAbort
    val message =
      for
        normalForm <- TypeNormalForm.fromSource(sourceText)
        term <- termFor("int")
        typed <- TypedTermConstruct.ascribeNormalForm(term, normalForm)
      yield typed.show
    Expr(message.fold(_.message, identity))

  private def typedTermSummary(using Quotes)(term: quotes.reflect.Term): String =
    import quotes.reflect.*

    term match
      case Typed(_, _) => "typed=true"
      case other => s"typed=false tree=${other.show(using Printer.TreeStructure)}"

  private def termFor(using Quotes)(kind: String): Either[TypeQuasiquoteError, quotes.reflect.Term] =
    import quotes.reflect.*

    kind match
      case "int" => Right('{ 1 }.asTerm)
      case "listInt" => Right('{ List(1) }.asTerm)
      case "optionString" => Right('{ Option("value") }.asTerm)
      case "tupleIntString" => Right('{ (1, "value") }.asTerm)
      case "functionIntString" => Right('{ (value: Int) => value.toString }.asTerm)
      case "tupleIntInt" => Right('{ (1, 1) }.asTerm)
      case other => Left(TypeQuasiquoteError(s"Unknown typed-term example kind: $other"))
