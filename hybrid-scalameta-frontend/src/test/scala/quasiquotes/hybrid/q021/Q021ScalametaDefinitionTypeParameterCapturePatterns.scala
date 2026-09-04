package quasiquotes.hybrid.q021

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.quoted.*
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q021.Q021CandidateFactory

object Q021ScalametaDefinitionTypeParameterCapturePatternMacro:
  def typeDefs(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.typeDefs(using $callerQuotes) }

  def symbols(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.symbols(using $callerQuotes) }

  def nameBounds(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q021CandidateFactory.nameBounds(using $callerQuotes) }

  private def validate(context: Expr[StringContext])(using Quotes): Unit =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) if exactLayout(values) =>
        structuralSentinelCheck(values) match
          case Right(()) => ()
          case Left(detail) => abort(context, detail)
      case Some(values) => abort(context, diagnostic(values))
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q021 typed-Scalameta Definition type-parameter capture template must be statically known.",
          context
        )

  private def exactLayout(parts: List[String]): Boolean =
    parts match
      case List(prefix, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def structuralSentinelCheck(parts: List[String]): Either[String, Unit] =
    val List(prefix, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) = parts: @unchecked
    val literal = parts.mkString("<capture>")
    val method = freshName("q021Method", literal)
    val firstType = freshName("Q021A", literal)
    val secondType = freshName("Q021B", literal)
    val firstTerm = freshName("q021First", literal)
    val secondTerm = freshName("q021Second", literal)
    val typeMarker = beforeTparams.lastIndexOf("..")
    val termMarker = beforeParamss.lastIndexOf("...")
    val source =
      prefix + method +
        beforeTparams.substring(0, typeMarker) +
        s"$firstType, $secondType <: List[$firstType]" +
        beforeParamss.substring(0, termMarker) +
        s"$firstTerm: $firstType)($secondTerm: $secondType" +
        beforeResult + secondType + beforeBody + secondTerm + suffix

    parseDefinition(source).flatMap { definition =>
      val valid = for
        group <- definition.paramClauseGroups match
          case value :: Nil => Some(value)
          case _ => None
        tparams <- group.tparamClause.values match
          case first :: second :: Nil => Some((first, second))
          case _ => None
        (firstTparam, secondTparam) = tparams
        if definition.mods.isEmpty &&
          definition.name.value == method &&
          firstTparam.name.value == firstType &&
          secondTparam.name.value == secondType &&
          secondTparam.tbounds.hi.exists(_.syntax == s"List[$firstType]")
        clauses <- group.paramClauses match
          case first :: second :: Nil if first.mod.isEmpty && second.mod.isEmpty => Some((first, second))
          case _ => None
        (firstClause, secondClause) = clauses
        firstParameter <- firstClause.values match
          case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Some(value)
          case _ => None
        secondParameter <- secondClause.values match
          case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Some(value)
          case _ => None
        if firstParameter.name.value == firstTerm &&
          firstParameter.decltpe.exists(_.syntax == firstType) &&
          secondParameter.name.value == secondTerm &&
          secondParameter.decltpe.exists(_.syntax == secondType) &&
          definition.decltpe.exists(_.syntax == secondType) &&
          (definition.body match
            case name: Term.Name => name.value == secondTerm
            case _ => false)
      yield ()

      valid.toRight(
        "Scalameta sentinel projection did not preserve the exact Q021 Definition type-parameter topology"
      )
    }

  private def parseDefinition(source: String): Either[String, Defn.Def] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) => Right(definition)
        case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
        case error: Parsed.Error =>
          Left(s"Scalameta rejected the sentinel at ${error.pos.start}..${error.pos.end}")
    catch
      case NonFatal(error) => Left(s"Scalameta sentinel failure: ${error.getClass.getSimpleName}")

  private def freshName(base: String, source: String): String =
    Iterator.from(0).map(index => if index == 0 then base else s"$base$index")
      .find(candidate => !source.contains(candidate)).get

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    if !literal.contains("[..") then "a rank-2 type-parameter capture is required"
    else if !literal.contains("(...") then "a rank-3 term-parameter-clause capture is required"
    else "name, complete type parameters, complete paramss, result, and body captures are required"

  private def abort(context: Expr[StringContext], detail: String)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Q021 typed-Scalameta dqq Definition template: $detail.",
      context
    )

object Q021TypeDefScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021ScalametaDefinitionTypeParameterCapturePatternMacro.typeDefs('context, 'q) }

object Q021SymbolScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021ScalametaDefinitionTypeParameterCapturePatternMacro.symbols('context, 'q) }

object Q021NameBoundsScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q021ScalametaDefinitionTypeParameterCapturePatternMacro.nameBounds('context, 'q) }
