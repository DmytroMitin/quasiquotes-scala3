package quasiquotes.hybrid.q023

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.quoted.*
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q023.Q023CandidateFactory

object Q023ScalametaDefinitionModifierCapturePatternMacro:
  def flags(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.flags(using $callerQuotes) }

  def structured(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.structured(using $callerQuotes) }

  def symbol(context: Expr[StringContext], callerQuotes: Expr[Quotes])(using Quotes): Expr[Any] =
    validate(context)
    '{ Q023CandidateFactory.symbol(using $callerQuotes) }

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
      case Some(_) =>
        abort(
          context,
          "expected exactly `$mods def $name[..$tparams](...$paramss): $result = $body`"
        )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q023 typed-Scalameta Definition modifier-capture template must be statically known.",
          context
        )

  private def exactLayout(parts: List[String]): Boolean =
    parts match
      case List(
            beforeMods,
            beforeName,
            beforeTparams,
            beforeParamss,
            beforeResult,
            beforeBody,
            suffix
          ) =>
        beforeMods.trim.isEmpty &&
          beforeName.matches("(?s)\\s+def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def structuralSentinelCheck(parts: List[String]): Either[String, Unit] =
    val List(_, beforeName, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) =
      parts: @unchecked
    val literal = parts.mkString("<capture>")
    val method = freshName("q023Method", literal)
    val firstType = freshName("Q023A", literal)
    val secondType = freshName("Q023B", literal)
    val parameter = freshName("q023Value", literal)
    val typeMarker = beforeTparams.lastIndexOf("..")
    val termMarker = beforeParamss.lastIndexOf("...")
    val source =
      "@deprecated(\"q023\", \"\") private final" + beforeName + method +
        beforeTparams.substring(0, typeMarker) +
        s"$firstType, $secondType <: List[$firstType]" +
        beforeParamss.substring(0, termMarker) +
        s"$parameter: $secondType" + beforeResult + secondType + beforeBody + parameter + suffix

    parseDefinition(source).flatMap { definition =>
      val valid = for
        _ <- definition.mods match
          case List(_: Mod.Annot, _: Mod.Private, _: Mod.Final) => Some(())
          case _ => None
        group <- definition.paramClauseGroups match
          case value :: Nil => Some(value)
          case _ => None
        tparams <- group.tparamClause.values match
          case first :: second :: Nil => Some((first, second))
          case _ => None
        (firstTparam, secondTparam) = tparams
        if definition.name.value == method &&
          firstTparam.name.value == firstType &&
          secondTparam.name.value == secondType &&
          secondTparam.tbounds.hi.exists(_.syntax == s"List[$firstType]")
        clause <- group.paramClauses match
          case value :: Nil if value.mod.isEmpty => Some(value)
          case _ => None
        value <- clause.values match
          case one :: Nil if one.mods.isEmpty && one.default.isEmpty => Some(one)
          case _ => None
        if value.name.value == parameter &&
          value.decltpe.exists(_.syntax == secondType) &&
          definition.decltpe.exists(_.syntax == secondType) &&
          (definition.body match
            case name: Term.Name => name.value == parameter
            case _ => false)
      yield ()

      valid.toRight(
        "Scalameta sentinel did not preserve ordered annotation/private/final mods and the complete Q023 Definition topology"
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

  private def abort(context: Expr[StringContext], detail: String)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(
      s"Invalid Q023 typed-Scalameta dqq Definition template: $detail.",
      context
    )

object Q023FlagsScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023ScalametaDefinitionModifierCapturePatternMacro.flags('context, 'q) }

object Q023StructuredScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023ScalametaDefinitionModifierCapturePatternMacro.structured('context, 'q) }

object Q023SymbolScalametaPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q023ScalametaDefinitionModifierCapturePatternMacro.symbol('context, 'q) }
