package quasiquotes.hybrid.q033

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q033ScalametaClauseSummary(
    typeParameterCount: Int,
    contextBounds: List[List[String]],
    clauseModes: List[String],
    parameterNames: List[List[String]],
    parameterModifiers: List[List[List[String]]],
    defaults: List[List[Boolean]]
)

object Q033ScalametaMixedClauseSyntax:
  def inspect(source: String): Either[String, Q033ScalametaClauseSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val typeParameters = groups.flatMap(_.tparamClause.values)
          val clauses = groups.flatMap(_.paramClauses)
          Right(Q033ScalametaClauseSummary(
            typeParameters.size,
            typeParameters.map(_.bounds.context.map(_.syntax)),
            clauses.map(_.mod.map(_.syntax).getOrElse("ordinary")),
            clauses.map(_.values.map(_.name.value)),
            clauses.map(_.values.map(_.mods.map(_.syntax))),
            clauses.map(_.values.map(_.default.nonEmpty))
          ))
        case Parsed.Success(other) =>
          Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
        case error: Parsed.Error =>
          Left(s"Scalameta rejected source at ${error.pos.start}..${error.pos.end}: ${error.message}")
    catch
      case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")

  def validatePatternParts(parts: List[String]): Either[String, Unit] =
    parts match
      case List(beforeModifiers, beforeName, beforeOrdinary, beforeUsing, beforeResult, beforeBody, suffix)
          if beforeModifiers.trim.isEmpty &&
            beforeName.matches("(?s)\\s+def\\s+") &&
            beforeOrdinary.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
            beforeUsing.matches("(?s)\\s*\\)\\s*\\(\\s*using\\s+\\.\\.\\s*") &&
            beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
            beforeBody.matches("(?s)\\s*=\\s*") &&
            suffix.trim.isEmpty =>
        val ordinaryMarker = beforeOrdinary.lastIndexOf("..")
        val usingMarker = beforeUsing.lastIndexOf("..")
        val source =
          "@deprecated(\"q033\", \"\") private[quasiquotes] final" + beforeName +
            "__q033_method" + beforeOrdinary.substring(0, ordinaryMarker) +
            "__q033_x: Int, __q033_y: String" + beforeUsing.substring(0, usingMarker) +
            "__q033_ord: Ordering[Int], __q033_num: Numeric[Int]" + beforeResult +
            "__q033_result" + beforeBody + "__q033_body" + suffix
        try
          TermQ3DialectPolicy.selected(source).parse[Stat] match
            case Parsed.Success(definition: Defn.Def) =>
              val groups = definition.paramClauseGroups
              val clauses = groups.flatMap(_.paramClauses)
              val valid =
                definition.name.value == "__q033_method" &&
                  groups.size == 1 &&
                  groups.head.tparamClause.values.isEmpty &&
                  clauses.size == 2 &&
                  clauses.head.mod.isEmpty &&
                  clauses(1).mod.exists(_.syntax == "using") &&
                  clauses.head.values.map(_.name.value) == List("__q033_x", "__q033_y") &&
                  clauses(1).values.map(_.name.value) == List("__q033_ord", "__q033_num") &&
                  clauses.flatten.forall(_.default.isEmpty) &&
                  definition.decltpe.exists(_.syntax == "__q033_result") &&
                  (definition.body match
                    case value: Term.Name => value.value == "__q033_body"
                    case _ => false)
              Either.cond(valid, (), "Scalameta sentinels did not preserve the exact mixed-clause structure")
            case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
            case error: Parsed.Error => Left(s"Scalameta rejected sentinel source: ${error.message}")
        catch
          case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
      case _ =>
        Left("only `$mods def $name(..$params)(using ..$usingParams): $result = $body` is selected")
