package quasiquotes.hybrid.q042

import scala.quoted.*
import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q042.Q042ByNameOrdinaryCandidateFactory

object Q042ScalametaDefinitionPatternMacros:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) =>
        validatePatternParts(values) match
          case Right(_) =>
            '{ Q042ByNameOrdinaryCandidateFactory.capturedModifiers(using $callerQuotes) }
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Q042 typed-Scalameta dqq Definition template: $detail.",
              context
            )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q042 typed-Scalameta by-name ordinary Definition template must be statically known.",
          context
        )

  private def validatePatternParts(parts: List[String]): Either[String, Unit] =
    parts match
      case List(beforeModifiers, beforeName, beforeParameters, beforeResult, beforeBody, suffix)
          if beforeModifiers.trim.isEmpty &&
            beforeName.matches("(?s)\\s+def\\s+") &&
            beforeParameters.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
            beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
            beforeBody.matches("(?s)\\s*=\\s*") &&
            suffix.trim.isEmpty =>
        val literalSource = parts.mkString
        def fresh(prefix: String, occupied: Set[String]): String =
          Iterator.from(0).map(index => s"$prefix$index").find(name =>
            !literalSource.contains(name) && !occupied.contains(name)
          ).get
        val method = fresh("__q042_method_", Set.empty)
        val strict = fresh("__q042_strict_", Set(method))
        val delayed = fresh("__q042_delayed_", Set(method, strict))
        val result = fresh("__q042_result_", Set(method, strict, delayed))
        val body = fresh("__q042_body_", Set(method, strict, delayed, result))
        val parameterMarker = beforeParameters.lastIndexOf("..")
        val source =
          "@deprecated(\"q042\", \"\") private[quasiquotes] final" + beforeName + method +
            beforeParameters.substring(0, parameterMarker) +
            s"$strict: Int, $delayed: => List[Option[Int]]" + beforeResult + result +
            beforeBody + body + suffix
        try
          TermQ3DialectPolicy.selected(source).parse[Stat] match
            case Parsed.Success(definition: Defn.Def) =>
              val groups = definition.paramClauseGroups
              val clauses = groups.flatMap(_.paramClauses)
              val modifierSyntax = definition.mods.map(_.syntax)
              val byNameType = clauses.head.values.last.decltpe.collect {
                case value: _root_.scala.meta.Type.ByName => value
              }
              val valid =
                definition.name.value == method &&
                  modifierSyntax.exists(_.startsWith("@deprecated")) &&
                  modifierSyntax.contains("private[quasiquotes]") &&
                  modifierSyntax.contains("final") &&
                  groups.size == 1 && groups.head.tparamClause.values.isEmpty &&
                  clauses.size == 1 && clauses.head.mod.isEmpty &&
                  clauses.head.values.map(_.name.value) == List(strict, delayed) &&
                  clauses.head.values.forall(_.mods.isEmpty) &&
                  clauses.head.values.forall(_.default.isEmpty) &&
                  byNameType.exists(_.tpe.syntax == "List[Option[Int]]") &&
                  definition.decltpe.exists(_.syntax == result) &&
                  (definition.body match
                    case value: Term.Name => value.value == body
                    case _ => false)
              Either.cond(valid, (), "Scalameta sentinels did not preserve the exact by-name ordinary structure")
            case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
            case error: Parsed.Error => Left(s"Scalameta rejected sentinel source: ${error.message}")
        catch
          case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
      case _ =>
        Left("only `$mods def $name(..$params): $result = $body` is selected")

object Q042ScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q042ScalametaDefinitionPatternMacros.extractor('context, 'q) }
