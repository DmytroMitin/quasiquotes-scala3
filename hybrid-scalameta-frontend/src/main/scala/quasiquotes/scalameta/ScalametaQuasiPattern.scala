package quasiquotes.scalameta

import scala.annotation.targetName
import scala.quoted.Quotes

import quasiquotes.matching.{
  DefinitionModifiers,
  DefinitionPatternExtractor,
  RankedDefinitionPatternExtractor,
  RankedDefinitionPatternExtractorFactory,
  SingleParameterDefinitionPattern,
  TermMatcher
}
import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend

/** Bounded extractor for ordered captures from the opt-in pattern frontend. */
final class ScalametaTermPatternExtractor[T] private[scalameta] (
    extract: T => Option[Seq[T]]
):
  def unapplySeq(value: T): Option[Seq[T]] = extract(value)

/** Bounded extractor for ordered original reflected Type captures. */
final class ScalametaTypePatternExtractor[T] private[scalameta] (
    extract: T => Option[Seq[T]]
):
  def unapplySeq(value: T): Option[Seq[T]] = extract(value)

/** Explicit opt-in matching syntax backed by the Scalameta-primary compiler. */
object ScalametaQuasiPattern:
  extension (context: StringContext)
    def qq(using q: Quotes): ScalametaTermPatternExtractor[q.reflect.Term] =
      import q.reflect.*

      val captureCount = context.parts.size - 1
      if captureCount <= 0 then
        report.errorAndAbort(
          "Invalid Scalameta qq term-pattern template: at least one term capture slot is required."
        )
      val captureNames = Vector.tabulate(captureCount)(index => s"qqCapture$index")
      val source = context.parts.zipWithIndex.foldLeft(new StringBuilder) {
        case (builder, (part, index)) =>
          builder.append(part)
          captureNames.lift(index).foreach(name => builder.append('$').append(name))
          builder
      }.toString

      TermFrontend.compile(source) match
        case Left(failure) =>
          report.errorAndAbort(
            s"Invalid Scalameta qq term-pattern template: ${failure.message}"
          )
        case Right(compiled) =>
          new ScalametaTermPatternExtractor[q.reflect.Term](term =>
            TermMatcher.matchTerm(compiled.pattern, term) match
              case Left(_) => None
              case Right(result) =>
                captureNames.foldLeft(Option(Vector.empty[q.reflect.Term])) {
                  case (captures, name) =>
                    captures.flatMap(current =>
                      result.bindings.get(name).map(current :+ _)
                    )
                }
          )

    def tqq(using q: Quotes): ScalametaTypePatternExtractor[q.reflect.TypeRepr] =
      import q.reflect.*

      if context == null || context.parts == null || context.parts.isEmpty then
        report.errorAndAbort(
          "Invalid Scalameta tqq type-pattern template: StringContext must contain at least one part."
        )

      TypeFrontend.compile(context.parts) match
        case Left(failure) =>
          report.errorAndAbort(
            s"Invalid Scalameta tqq type-pattern template: ${failure.message}"
          )
        case Right(compiled) =>
          new ScalametaTypePatternExtractor[q.reflect.TypeRepr](target =>
            TypeFrontend
              .matchPattern(using q)(compiled, target)
              .toOption
              .flatten
              .map(_.captures)
          )

  private[scalameta] def singleParameterExtractor(
      context: StringContext
  )(using q: Quotes): SingleParameterDefinitionPattern =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend.compilePattern(context.parts) match
      case Right(pattern) => pattern
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def exactTwoExtractor(
      context: StringContext
  )(using q: Quotes): DefinitionPatternExtractor =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend.compileExactTwoPattern(context.parts) match
      case Right(pattern) => pattern
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def rankedParameterSequenceExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], q.reflect.Term)
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend.compileRankedPattern(context.parts) match
      case Right(_) => RankedDefinitionPatternExtractorFactory.exactCollect
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def rankedParameterClauseSequenceExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend.compileRankedParameterClauseSequencePattern(context.parts) match
      case Right(_) => RankedDefinitionPatternExtractorFactory.exactCollectParamss
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedNameRankedParameterClauseSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedNameRankedParameterClauseSequenceCapturedResultPattern(context.parts) match
      case Right(_) => RankedDefinitionPatternExtractorFactory.capturedNameParamssResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.TypeDef],
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameRankedParameterClauseSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[Seq[q.reflect.ValDef]],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameRankedParameterClauseSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameParamssResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameOrdinaryParameterSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameOrdinaryParameterSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameOrdinaryParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameNamedUsingParameterSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameNamedUsingParameterSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameNamedUsingParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedNameNamedUsingParameterSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedNameNamedUsingParameterSequenceCapturedResultPattern(context.parts) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedNameNamedUsingParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameMixedOrdinaryNamedUsingParameterSequencesCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
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
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameMixedOrdinaryNamedUsingParameterSequencesCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameMixedOrdinaryNamedUsingParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedNameMixedOrdinaryNamedUsingParameterSequencesCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      String,
      Seq[q.reflect.ValDef],
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedNameMixedOrdinaryNamedUsingParameterSequencesCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedNameMixedOrdinaryNamedUsingParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
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
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameMixedOrdinaryScala2ImplicitParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameScala2ImplicitParameterSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (
      DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
      String,
      Seq[q.reflect.ValDef],
      q.reflect.TypeRepr,
      q.reflect.Term
    )
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameScala2ImplicitParameterSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameScala2ImplicitParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedNameScala2ImplicitParameterSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedNameScala2ImplicitParameterSequenceCapturedResultPattern(context.parts) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedNameScala2ImplicitParamsResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  private[scalameta] def capturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
      context: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
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
    if context == null then
      q.reflect.report.errorAndAbort(
        "Invalid Scalameta dqq definition-pattern template: StringContext must not be null."
      )
    ScalametaDefinitionFrontend
      .compileCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
        context.parts
      ) match
      case Right(_) =>
        RankedDefinitionPatternExtractorFactory.capturedModifiersNameTypeParamsParamssResult
      case Left(failure) =>
        q.reflect.report.errorAndAbort(
          s"Invalid Scalameta dqq definition-pattern template: ${failure.message}"
        )

  /** JVM-linkage bridge for callers compiled against the pre-Q014 extension.
    * New source calls use the transparent inline structural selector.
    */
  @targetName("dqq")
  private[scalameta] def dqqLegacy(
      context: StringContext
  )(using q: Quotes): SingleParameterDefinitionPattern =
    singleParameterExtractor(context)

  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ ScalametaDefinitionPatternMacro.extractor('context, 'q) }
