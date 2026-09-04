package quasiquotes.hybrid.q019

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.quoted.*
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q019.Q019CandidateFactory

object Q019ScalametaDefinitionCapturePatternMacro:
  def semantic(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q019CandidateFactory.semantic(using $callerQuotes) }

  def tree(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q019CandidateFactory.tree(using $callerQuotes) }

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
          "Q019 typed-Scalameta Definition capture template must be statically known.",
          context
        )

  private def exactLayout(parts: List[String]): Boolean =
    parts match
      case List(prefix, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeParamss.matches("(?s)\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def structuralSentinelCheck(parts: List[String]): Either[String, Unit] =
    val List(prefix, beforeParamss, beforeResult, beforeBody, suffix) = parts: @unchecked
    val literal = parts.mkString("<capture>")
    val method = freshName("q019Method", literal)
    val first = freshName("q019First", literal)
    val second = freshName("q019Second", literal)
    val body = freshName("q019Body", literal)
    val markerOffset = beforeParamss.lastIndexOf("...")
    val source =
      prefix + method +
        beforeParamss.substring(0, markerOffset) +
        s"$first: Int)($second: String" +
        beforeResult + "Either[Int, String]" + beforeBody + body + suffix

    parseDefinition(source).flatMap { definition =>
      val valid = for
        group <- definition.paramClauseGroups match
          case value :: Nil => Some(value)
          case _ => None
        if definition.mods.isEmpty &&
          definition.name.value == method &&
          definition.name.syntax == method &&
          group.tparamClause.values.isEmpty
        clauses <- group.paramClauses match
          case firstClause :: secondClause :: Nil
              if firstClause.mod.isEmpty && secondClause.mod.isEmpty =>
            Some((firstClause, secondClause))
          case _ => None
        (firstClause, secondClause) = clauses
        firstParameter <- firstClause.values match
          case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Some(value)
          case _ => None
        secondParameter <- secondClause.values match
          case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Some(value)
          case _ => None
        if firstParameter.name.value == first &&
          firstParameter.decltpe.exists(_.syntax == "Int") &&
          secondParameter.name.value == second &&
          secondParameter.decltpe.exists(_.syntax == "String") &&
          definition.decltpe.exists(_.syntax == "Either[Int, String]") &&
          (definition.body match
            case name: Term.Name => name.value == body
            case _ => false)
      yield ()

      valid.toRight(
        "Scalameta sentinel projection did not preserve the exact Q019 Definition capture topology"
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
    Iterator
      .from(0)
      .map(index => if index == 0 then base else s"$base$index")
      .find(candidate => !source.contains(candidate))
      .get

  private def diagnostic(parts: List[String]): String =
    val literal = parts.mkString("<capture>")
    if literal.contains("..") && !literal.contains("...") then
      "rank-2 capture is not rank-3 parameter-clause capture"
    else if parts.count(_.contains("...")) > 1 then
      "exactly one rank-3 capture is required"
    else
      "name, complete rank-3 paramss, result, and complete body captures are required"

  private def abort(context: Expr[StringContext], detail: String)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Q019 typed-Scalameta dqq Definition template: $detail.",
      context
    )

object Q019SemanticScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q019ScalametaDefinitionCapturePatternMacro.semantic('context, 'q) }

object Q019TreeScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q019ScalametaDefinitionCapturePatternMacro.tree('context, 'q) }
