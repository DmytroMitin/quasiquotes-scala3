package quasiquotes.matching

import scala.quoted.*
import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm, TypeNormalFormSource}

final class TwoParameterDefinitionPattern private[matching] (
    private val expectedMethodName: String,
    private val expectedFirstParameterName: String,
    private val expectedFirstParameterType: TypeNormalForm,
    private val expectedSecondParameterName: String,
    private val expectedSecondParameterType: TypeNormalForm,
    private val expectedResultType: TypeNormalForm
):
  def unapply(using q: Quotes)(target: q.reflect.DefDef): Option[q.reflect.Term] =
    import q.reflect.*

    if target == null ||
        target.name != expectedMethodName ||
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
          clause.params match
            case List(first, second)
                if admittedParameters(target, first, second) &&
                  first.name == expectedFirstParameterName &&
                  second.name == expectedSecondParameterName =>
              for
                body <- target.rhs
                firstType <- TargetTypeReprInspector.inspect(first.tpt.tpe).toOption
                if firstType == expectedFirstParameterType
                secondType <- TargetTypeReprInspector.inspect(second.tpt.tpe).toOption
                if secondType == expectedSecondParameterType
                resultType <- TargetTypeReprInspector.inspect(target.returnTpt.tpe).toOption
                if resultType == expectedResultType
              yield body
            case _ => None
        case _ => None

  private def admittedParameters(using q: Quotes)(
      target: q.reflect.DefDef,
      first: q.reflect.ValDef,
      second: q.reflect.ValDef
  ): Boolean =
    import q.reflect.*

    target.symbol.paramSymss match
      case List(List(firstSymbol, secondSymbol)) =>
        first.symbol != Symbol.noSymbol &&
          second.symbol != Symbol.noSymbol &&
          first.symbol != second.symbol &&
          !first.symbol.flags.is(Flags.HasDefault) &&
          !second.symbol.flags.is(Flags.HasDefault) &&
          !first.symbol.flags.is(Flags.Erased) &&
          !second.symbol.flags.is(Flags.Erased) &&
          firstSymbol == first.symbol &&
          secondSymbol == second.symbol &&
          first.symbol.owner == target.symbol &&
          second.symbol.owner == target.symbol
      case _ => false

private[matching] object TwoParameterDefinitionPattern:
  private final case class Parsed(
      methodName: String,
      firstParameterName: String,
      firstParameterTypeSource: String,
      secondParameterName: String,
      secondParameterTypeSource: String,
      resultTypeSource: String
  )

  private val InvalidPatternMessage =
    "Invalid exact-two definition pattern; expected one ordinary method with two distinct parameters, standalone Int/String/Boolean types, and `$body` as the complete right-hand side."
  private val AdmittedTypes = Set("Int", "String", "Boolean")

  def compile(source: String): Either[String, TwoParameterDefinitionPattern] =
    if source == null then Left("Definition pattern source must not be null.")
    else
      try parse(source).flatMap(compileParsed)
      catch case NonFatal(_) => Left(InvalidPatternMessage)

  private def compileParsed(parsed: Parsed): Either[String, TwoParameterDefinitionPattern] =
    for
      methodName <- name(parsed.methodName)
      firstName <- name(parsed.firstParameterName)
      secondName <- name(parsed.secondParameterName)
      _ <- Either.cond(firstName != secondName, (), InvalidPatternMessage)
      firstType <- admittedType(parsed.firstParameterTypeSource)
      secondType <- admittedType(parsed.secondParameterTypeSource)
      resultType <- admittedType(parsed.resultTypeSource)
    yield new TwoParameterDefinitionPattern(
      methodName.decoded,
      firstName.decoded,
      firstType,
      secondName.decoded,
      secondType,
      resultType
    )

  private def name(source: String): Either[String, DefinitionName] =
    DefinitionName.plain(source).left.map(_ => InvalidPatternMessage)

  private def admittedType(source: String): Either[String, TypeNormalForm] =
    TypeNormalFormSource.fromSource(source).left.map(_ => InvalidPatternMessage).flatMap {
      case value @ TypeNormalForm.STypeIdent(name) if AdmittedTypes(name) => Right(value)
      case _ => Left(InvalidPatternMessage)
    }

  private def parse(source: String): Either[String, Parsed] =
    var cursor = 0

    def skipWhitespace(): Unit =
      while cursor < source.length && source.charAt(cursor).isWhitespace do cursor += 1

    def consume(text: String): Boolean =
      if source.startsWith(text, cursor) then
        cursor += text.length
        true
      else false

    def consumeWhitespace(): Boolean =
      val start = cursor
      skipWhitespace()
      cursor > start

    def punctuation(text: String): Boolean =
      skipWhitespace()
      consume(text)

    def readName(): Option[String] =
      skipWhitespace()
      val start = cursor
      if cursor < source.length && isNameStart(source.charAt(cursor)) then
        cursor += 1
        while cursor < source.length && isNamePart(source.charAt(cursor)) do cursor += 1
        Some(source.substring(start, cursor))
      else None

    def readTypeUntil(delimiter: Char): Option[String] =
      skipWhitespace()
      val start = cursor
      var parentheses = 0
      var brackets = 0
      while cursor < source.length do
        source.charAt(cursor) match
          case '(' => parentheses += 1
          case ')' if delimiter == ')' && parentheses == 0 && brackets == 0 =>
            return slice(source, start, cursor)
          case ')' => parentheses -= 1
          case '[' => brackets += 1
          case ']' => brackets -= 1
          case ',' if delimiter == ',' && parentheses == 0 && brackets == 0 =>
            return slice(source, start, cursor)
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
          case '=' if parentheses == 0 && brackets == 0 &&
              (cursor + 1 >= source.length || source.charAt(cursor + 1) != '>') =>
            return slice(source, start, cursor)
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
      _ <- Option.when(punctuation("("))(())
      firstName <- readName()
      _ <- Option.when(punctuation(":"))(())
      firstType <- readTypeUntil(',')
      _ <- Option.when(punctuation(","))(())
      secondName <- readName()
      _ <- Option.when(punctuation(":"))(())
      secondType <- readTypeUntil(')')
      _ <- Option.when(punctuation(")"))(())
      _ <- Option.when(punctuation(":"))(())
      resultType <- readResultType()
      _ <- Option.when(punctuation("="))(())
      _ <- Option.when(punctuation("$body"))(())
      _ <- Option.when(atEnd)(())
    yield Parsed(methodName, firstName, firstType, secondName, secondType, resultType)

    parsed.toRight(InvalidPatternMessage)

  private def slice(source: String, start: Int, end: Int): Option[String] =
    Option(source.substring(start, end).trim).filter(_.nonEmpty)

  private def isNameStart(char: Char): Boolean =
    char == '_' || char.isLetter && char <= '\u007f'

  private def isNamePart(char: Char): Boolean =
    isNameStart(char) || char.isDigit && char <= '\u007f'
