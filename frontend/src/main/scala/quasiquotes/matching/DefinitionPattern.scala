package quasiquotes.matching

import scala.quoted.*
import scala.annotation.targetName
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.types.{
  TargetTypeReprInspector,
  TypeNormalForm,
  TypeNormalFormSource
}

final class DefinitionPatternError private[matching] (
    val message: String
)

final class SingleParameterDefinitionMatch[Tpe, Trm] private[matching] (
    val methodName: String,
    val parameterName: String,
    val parameterType: Tpe,
    val resultType: Tpe,
    val body: Trm
)

final class SingleParameterDefinitionPattern private[matching] (
    private val expectedMethodName: String,
    private val expectedParameterName: String,
    private val expectedParameterType: TypeNormalForm,
    private val expectedResultType: TypeNormalForm
):
  def matchDefinition(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[
    SingleParameterDefinitionMatch[q.reflect.TypeRepr, q.reflect.Term]
  ] =
    import q.reflect.*

    if target == null ||
        target.name != expectedMethodName ||
        target.symbol == Symbol.noSymbol ||
        !target.symbol.isDefDef ||
        !DefinitionModifierSemantics.isSemanticallyEmpty(target.symbol) ||
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
          clause.params match
            case List(parameter)
                if admittedParameter(target, parameter) &&
                  parameter.name == expectedParameterName =>
              val parameterType = parameter.tpt.tpe
              val resultType = target.returnTpt.tpe
              val body = target.rhs

              for
                rhs <- body
                actualParameterType <- TargetTypeReprInspector
                  .inspect(parameterType)
                  .toOption
                if actualParameterType == expectedParameterType
                actualResultType <- TargetTypeReprInspector
                  .inspect(resultType)
                  .toOption
                if actualResultType == expectedResultType
              yield
                new SingleParameterDefinitionMatch(
                  target.name,
                  parameter.name,
                  parameterType,
                  resultType,
                  rhs
                )
            case _ => None
        case _ => None

  def unapply(using q: Quotes)(
      target: q.reflect.DefDef
  ): Option[q.reflect.Term] =
    matchDefinition(target).map(_.body)

  private def admittedParameter(using q: Quotes)(
      target: q.reflect.DefDef,
      parameter: q.reflect.ValDef
  ): Boolean =
    import q.reflect.*

    val methodParameters = target.symbol.paramSymss.flatten
    parameter.symbol != Symbol.noSymbol &&
      !parameter.symbol.flags.is(Flags.HasDefault) &&
      methodParameters.size == 1 &&
      methodParameters.head == parameter.symbol &&
      parameter.symbol.owner == target.symbol

object DefinitionPattern:
  private[matching] enum StaticPatternKind:
    case SingleParameter
    case ExactTwo
    case RankedParameterSequence
    case RankedParameterClauseSequence
    case CapturedNameRankedParameterClauseSequenceCapturedResult
    case CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult

  private final case class ParsedPattern(
      methodName: String,
      parameterName: String,
      parameterTypeSource: String,
      resultTypeSource: String
  )

  private val InvalidPatternMessage =
    "Invalid single-parameter definition pattern; expected one ordinary method with fixed supported parameter and result types and `$body` as the complete right-hand side."

  private val InvalidDqqPrefix =
    "Invalid dqq definition-pattern template:"

  private[matching] def singleParameterExtractor(
      sc: StringContext
  )(using q: Quotes): SingleParameterDefinitionPattern =
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    if sc == null then abort("StringContext must not be null.")
    val parts = sc.parts
    if parts == null || parts.isEmpty then
      abort("StringContext must contain exactly two literal parts.")
    if parts.exists(_ == null) then
      abort("StringContext literal parts must not be null.")
    RankedPatternSource
      .unsupportedFamilyRankDiagnostic(parts, "Definition")
      .foreach(abort)
    if parts.size != 2 then
      abort(s"Expected exactly one body capture slot, but found ${parts.size - 1}.")

    val source = parts.head + "$body" + parts.last
    singleParameter(source) match
      case Right(pattern) => pattern
      case Left(error) => abort(error.message)

  private[matching] def twoParameterExtractor(
      sc: StringContext
  )(using q: Quotes): DefinitionPatternExtractor =
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    validateParts(sc).flatMap(parts =>
      DefinitionPatternExtractor.compileExactTwo(parts.head + "$body" + parts.last)
    ) match
      case Right(pattern) => pattern
      case Left(message) => abort(message)

  private[matching] def rankedParameterSequenceExtractor(
      sc: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[q.reflect.ValDef], q.reflect.Term)
  ] =
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    val parts = Option(sc).flatMap(value => Option(value.parts)).map(_.toList)
    parts match
      case Some(values) if isExactRankedParameterSequence(values) =>
        RankedDefinitionPatternExtractorFactory.exactCollect
      case Some(values) =>
        abort(rankedParameterSequenceDiagnostic(values))
      case None => abort("StringContext must not be null.")

  private[matching] def rankedParameterClauseSequenceExtractor(
      sc: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
  ] =
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    val parts = Option(sc).flatMap(value => Option(value.parts)).map(_.toList)
    parts match
      case Some(values) if isExactRankedParameterClauseSequence(values) =>
        RankedDefinitionPatternExtractorFactory.exactCollectParamss
      case Some(values) =>
        abort(rankedParameterClauseSequenceDiagnostic(values))
      case None => abort("StringContext must not be null.")

  private[matching] def capturedNameRankedParameterClauseSequenceCapturedResultExtractor(
      sc: StringContext
  )(using q: Quotes): RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
  ] =
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    val parts = Option(sc).flatMap(value => Option(value.parts)).map(_.toList)
    parts match
      case Some(values) if isExactCapturedNameRankedParameterClauseSequenceCapturedResult(values) =>
        RankedDefinitionPatternExtractorFactory.capturedNameParamssResult
      case Some(_) =>
        abort(
          "expected exactly `def $name(...$paramss): $result = $body` with four captures in semantic-name, complete-paramss, semantic-result, and complete-body order"
        )
      case None => abort("StringContext must not be null.")

  private[matching] def capturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultExtractor(
      sc: StringContext
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
    def abort(message: String): Nothing =
      q.reflect.report.errorAndAbort(s"$InvalidDqqPrefix $message")

    val parts = Option(sc).flatMap(value => Option(value.parts)).map(_.toList)
    parts match
      case Some(values)
          if isExactCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
            values
          ) =>
        RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult
      case Some(_) =>
        abort(
          "expected exactly `def $name[..$tparams](...$paramss): $result = $body` with five captures in semantic-name, complete-type-parameters, complete-paramss, semantic-result, and complete-body order"
        )
      case None => abort("StringContext must not be null.")

  private[matching] def classifyStaticParts(
      parts: List[String]
  ): Either[String, StaticPatternKind] =
    if parts == null || parts.isEmpty then
      Left("StringContext must contain exactly two literal parts.")
    else if parts.exists(_ == null) then
      Left("StringContext literal parts must not be null.")
    else if isExactCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
        parts
      )
    then
      Right(
        StaticPatternKind.CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
      )
    else if isExactCapturedNameRankedParameterClauseSequenceCapturedResult(parts) then
      Right(StaticPatternKind.CapturedNameRankedParameterClauseSequenceCapturedResult)
    else if isExactRankedParameterClauseSequence(parts) then
      Right(StaticPatternKind.RankedParameterClauseSequence)
    else if isExactRankedParameterSequence(parts) then
      Right(StaticPatternKind.RankedParameterSequence)
    else
      RankedPatternSource
        .unsupportedFamilyRankDiagnostic(parts, "Definition")
        .map(Left(_))
        .getOrElse {
          if parts.size != 2 then
            Left(s"Expected exactly one body capture slot, but found ${parts.size - 1}.")
          else
            val source = parts.head + "$body" + parts.last
            singleParameter(source) match
              case Right(_) => Right(StaticPatternKind.SingleParameter)
              case Left(_) =>
                DefinitionPatternExtractor.compileExactTwo(source) match
                  case Right(_) => Right(StaticPatternKind.ExactTwo)
                  case Left(message) => Left(message)
        }

  private def isExactRankedParameterSequence(parts: List[String]): Boolean =
    parts match
      case List(prefix, between, suffix) =>
        prefix.matches("(?s)\\s*def\\s+collect\\s*\\(\\s*\\.\\.\\s*") &&
          between.matches("(?s)\\s*\\)\\s*:\\s*Int\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactRankedParameterClauseSequence(parts: List[String]): Boolean =
    parts match
      case List(prefix, between, suffix) =>
        prefix.matches("(?s)\\s*def\\s+collect\\s*\\(\\s*\\.\\.\\.\\s*") &&
          between.matches("(?s)\\s*\\)\\s*:\\s*Int\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactCapturedNameRankedParameterClauseSequenceCapturedResult(
      parts: List[String]
  ): Boolean =
    parts match
      case List(prefix, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeParamss.matches("(?s)\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
      parts: List[String]
  ): Boolean =
    parts match
      case List(prefix, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def rankedParameterSequenceDiagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    if literal.contains("...") then
      "rank-3 captures are not supported for Definition patterns"
    else if parts.count(_.contains("..")) > 1 then
      "only one rank-2 parameter-sequence capture is supported for Definition patterns"
    else if parts.exists(_.contains("..")) then
      "rank-2 capture is supported only as the complete immediate parameter list in `def collect(..$params): Int = $body`"
    else
      "expected exactly `def collect(..$params): Int = $body`"

  private def rankedParameterClauseSequenceDiagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    if parts.count(_.contains("...")) > 1 then
      "only one rank-3 parameter-clause capture is supported for Definition patterns"
    else if literal.contains("..") && !literal.contains("...") then
      "rank-2 capture does not represent the complete parameter-clause region"
    else if literal.contains("...") then
      "rank-3 capture is supported only as the complete parameter-clause region in `def collect(...$paramss): Int = $body`"
    else
      "expected exactly `def collect(...$paramss): Int = $body`"

  private def validateParts(sc: StringContext): Either[String, List[String]] =
    if sc == null then Left("StringContext must not be null.")
    else
      val rawParts = sc.parts
      if rawParts == null then Left("StringContext must contain exactly two literal parts.")
      else
        val parts = rawParts.toList
        classifyStaticParts(parts).flatMap {
          case StaticPatternKind.ExactTwo => Right(parts)
          case _ => Left("Invalid exact-two definition pattern.")
        }

  /** JVM-linkage bridge for callers compiled against the pre-Q012R extension.
    * New source calls use the transparent inline structural selector.
    */
  @targetName("dqq")
  private[matching] def dqqLegacy(
      sc: StringContext
  )(using q: Quotes): SingleParameterDefinitionPattern =
    singleParameterExtractor(sc)

  extension (inline sc: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ DefinitionPatternMacro.extractor('sc, 'q) }

  def singleParameter(
      source: String
  ): Either[DefinitionPatternError, SingleParameterDefinitionPattern] =
    if source == null then
      Left(new DefinitionPatternError("Definition pattern source must not be null."))
    else
      try compile(source)
      catch
        case NonFatal(_) => invalidPattern

  private[quasiquotes] def singleParameterStructured(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm
  ): SingleParameterDefinitionPattern =
    new SingleParameterDefinitionPattern(
      methodName.decoded,
      parameterName.decoded,
      parameterType,
      resultType
    )

  private[quasiquotes] def structured(
      methodName: DefinitionName,
      parameterClauses: Vector[Vector[(DefinitionName, TypeNormalForm)]],
      resultType: TypeNormalForm
  ): DefinitionPatternExtractor =
    DefinitionPatternExtractor.structured(
      methodName.decoded,
      parameterClauses.map(_.map((name, parameterType) => (name.decoded, parameterType))),
      resultType
    )

  private def compile(
      source: String
  ): Either[DefinitionPatternError, SingleParameterDefinitionPattern] =
    parseExactShape(source).flatMap(compileParsed)

  private def compileParsed(
      parsed: ParsedPattern
  ): Either[DefinitionPatternError, SingleParameterDefinitionPattern] =
    for
      methodName <- DefinitionName.plain(parsed.methodName).left.map(_ => patternError)
      parameterName <- DefinitionName
        .plain(parsed.parameterName)
        .left
        .map(_ => patternError)
      parameterType <- TypeNormalFormSource
        .fromSource(parsed.parameterTypeSource)
        .left
        .map(_ => patternError)
      resultType <- TypeNormalFormSource
        .fromSource(parsed.resultTypeSource)
        .left
        .map(_ => patternError)
    yield singleParameterStructured(
        methodName,
        parameterName,
        parameterType,
        resultType
      )

  private def parseExactShape(
      source: String
  ): Either[DefinitionPatternError, ParsedPattern] =
    var cursor = 0

    def skipWhitespace(): Unit =
      while cursor < source.length && source.charAt(cursor).isWhitespace do
        cursor += 1

    def consume(text: String): Boolean =
      if source.startsWith(text, cursor) then
        cursor += text.length
        true
      else false

    def consumeWhitespace(): Boolean =
      val start = cursor
      skipWhitespace()
      cursor > start

    def consumePunctuation(text: String): Boolean =
      skipWhitespace()
      consume(text)

    def readName(): Option[String] =
      skipWhitespace()
      val start = cursor
      if cursor < source.length && isNameStart(source.charAt(cursor)) then
        cursor += 1
        while cursor < source.length && isNamePart(source.charAt(cursor)) do
          cursor += 1
        Some(source.substring(start, cursor))
      else None

    def readParameterType(): Option[String] =
      skipWhitespace()
      val start = cursor
      var parentheses = 0
      var brackets = 0
      while cursor < source.length do
        source.charAt(cursor) match
          case '(' => parentheses += 1
          case ')' if parentheses == 0 && brackets == 0 =>
            return nonEmptySlice(source, start, cursor)
          case ')' => parentheses -= 1
          case '[' => brackets += 1
          case ']' => brackets -= 1
          case _ => ()
        if parentheses < 0 || brackets < 0 then return None
        cursor += 1
      None

    def readResultType(): Option[String] =
      skipWhitespace()
      val start = cursor
      var parentheses = 0
      var brackets = 0
      while cursor < source.length do
        source.charAt(cursor) match
          case '(' => parentheses += 1
          case ')' => parentheses -= 1
          case '[' => brackets += 1
          case ']' => brackets -= 1
          case '='
              if parentheses == 0 && brackets == 0 &&
                (cursor + 1 >= source.length || source.charAt(cursor + 1) != '>') =>
            return nonEmptySlice(source, start, cursor)
          case _ => ()
        if parentheses < 0 || brackets < 0 then return None
        cursor += 1
      None

    def atEnd: Boolean =
      skipWhitespace()
      cursor == source.length

    skipWhitespace()
    val parsed = for
      _ <- Option.when(consume("def") && consumeWhitespace())(())
      methodName <- readName()
      _ <- Option.when(consumePunctuation("("))(())
      parameterName <- readName()
      _ <- Option.when(consumePunctuation(":"))(())
      parameterType <- readParameterType()
      _ <- Option.when(consumePunctuation(")"))(())
      _ <- Option.when(consumePunctuation(":"))(())
      resultType <- readResultType()
      _ <- Option.when(consumePunctuation("="))(())
      _ <- Option.when(consumePunctuation("$body"))(())
      _ <- Option.when(atEnd)(())
    yield ParsedPattern(
      methodName,
      parameterName,
      parameterType,
      resultType
    )

    parsed.toRight(patternError)

  private def nonEmptySlice(
      source: String,
      start: Int,
      end: Int
  ): Option[String] =
    Option(source.substring(start, end).trim).filter(_.nonEmpty)

  private def isNameStart(char: Char): Boolean =
    char == '_' || char.isLetter && char <= '\u007f'

  private def isNamePart(char: Char): Boolean =
    isNameStart(char) || char.isDigit && char <= '\u007f'

  private def invalidPattern[A]: Either[DefinitionPatternError, A] =
    Left(patternError)

  private def patternError: DefinitionPatternError =
    new DefinitionPatternError(InvalidPatternMessage)
