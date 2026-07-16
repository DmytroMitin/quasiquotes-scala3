package quasiquotes.types

import scala.quoted.*

object ConstructedTypeBridgeExamples:
  inline def bridgeSummary(templateSource: String, bindingName: String, bindingSource: String): String =
    ${ bridgeSummaryImpl('templateSource, 'bindingName, 'bindingSource) }

  inline def bridgeSummary(
      templateSource: String,
      firstBindingName: String,
      firstBindingSource: String,
      secondBindingName: String,
      secondBindingSource: String
  ): String =
    ${ bridgeSummaryImpl('templateSource, 'firstBindingName, 'firstBindingSource, 'secondBindingName, 'secondBindingSource) }

  inline def missingBindingMessage(templateSource: String): String =
    ${ missingBindingMessageImpl('templateSource) }

  inline def unsupportedNormalFormMessage(source: String): String =
    ${ unsupportedNormalFormMessageImpl('source) }

  private def bridgeSummaryImpl(
      templateSource: Expr[String],
      bindingName: Expr[String],
      bindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    val templateText = templateSource.valueOrAbort
    val bindingText = bindingName.valueOrAbort
    val bindingSourceText = bindingSource.valueOrAbort
    val summary =
      for
        binding <- TypeNormalForm.fromSource(bindingSourceText)
        constructed <- QuasiTypequotes.tqr(templateText, bindingText -> binding)
        bridged <- bridgeAndInspect(constructed)
      yield bridged
    Expr(summary.fold(_.message, identity))

  private def bridgeSummaryImpl(
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
        bridged <- bridgeAndInspect(constructed)
      yield bridged
    Expr(summary.fold(_.message, identity))

  private def missingBindingMessageImpl(templateSource: Expr[String])(using Quotes): Expr[String] =
    val templateText = templateSource.valueOrAbort
    val summary = ConstructedTypeBridge.withTemplateType(templateText) {
      [t] => (evidence: Type[t]) ?=> Type.show[t]
    }
    Expr(summary.fold(_.message, identity))

  private def unsupportedNormalFormMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    val sourceText = source.valueOrAbort
    val summary =
      for
        normalForm <- TypeNormalForm.fromSource(sourceText)
        bridged <- ConstructedTypeBridge.withNormalFormType(normalForm) {
          [t] => (evidence: Type[t]) ?=> Type.show[t]
        }
      yield bridged
    Expr(summary.fold(_.message, identity))

  private def bridgeAndInspect(constructed: ConstructedType)(using Quotes): Either[TypeQuasiquoteError, String] =
    ConstructedTypeBridge.withType(constructed) {
      [t] => (evidence: Type[t]) ?=>
        val evidenceRepr = quotes.reflect.TypeRepr.of[t]
        TargetTypeReprInspector.inspect(evidenceRepr).map { inspected =>
          s"constructed=${constructed.normalForm.render} evidence=${inspected.render} matched=${constructed.normalForm == inspected}"
        }
    }.flatten
