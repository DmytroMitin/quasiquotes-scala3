package quasiquotes.hybrid.q038

import scala.quoted.*
import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q038.Q038DefaultedOrdinaryCandidateFactory

object Q038ScalametaDefinitionPatternMacros:
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
            '{ Q038DefaultedOrdinaryCandidateFactory.capturedModifiers(using $callerQuotes) }
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Q038 typed-Scalameta dqq Definition template: $detail.",
              context
            )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q038 typed-Scalameta defaulted-ordinary Definition template must be statically known.",
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
        val method = fresh("__q038_method_", Set.empty)
        val first = fresh("__q038_parameter_", Set(method))
        val second = fresh("__q038_parameter_", Set(method, first))
        val result = fresh("__q038_result_", Set(method, first, second))
        val body = fresh("__q038_body_", Set(method, first, second, result))
        val parameterMarker = beforeParameters.lastIndexOf("..")
        val source =
          "@deprecated(\"q038\", \"\") private[quasiquotes] final" + beforeName + method +
            beforeParameters.substring(0, parameterMarker) +
            s"$first: Int, $second: String = \"q038\"" + beforeResult + result +
            beforeBody + body + suffix
        try
          TermQ3DialectPolicy.selected(source).parse[Stat] match
            case Parsed.Success(definition: Defn.Def) =>
              val groups = definition.paramClauseGroups
              val clauses = groups.flatMap(_.paramClauses)
              val modifierSyntax = definition.mods.map(_.syntax)
              val valid =
                definition.name.value == method &&
                  modifierSyntax.exists(_.startsWith("@deprecated")) &&
                  modifierSyntax.contains("private[quasiquotes]") &&
                  modifierSyntax.contains("final") &&
                  groups.size == 1 && groups.head.tparamClause.values.isEmpty &&
                  clauses.size == 1 && clauses.head.mod.isEmpty &&
                  clauses.head.values.map(_.name.value) == List(first, second) &&
                  clauses.head.values.forall(_.mods.isEmpty) &&
                  clauses.head.values.map(_.default.nonEmpty) == List(false, true) &&
                  clauses.head.values(1).default.exists(_.syntax == "\"q038\"") &&
                  definition.decltpe.exists(_.syntax == result) &&
                  (definition.body match
                    case value: Term.Name => value.value == body
                    case _ => false)
              Either.cond(valid, (), "Scalameta sentinels did not preserve the exact defaulted-ordinary structure")
            case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
            case error: Parsed.Error => Left(s"Scalameta rejected sentinel source: ${error.message}")
        catch
          case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
      case _ =>
        Left("only `$mods def $name(..$params): $result = $body` is selected")

object Q038ScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q038ScalametaDefinitionPatternMacros.extractor('context, 'q) }
