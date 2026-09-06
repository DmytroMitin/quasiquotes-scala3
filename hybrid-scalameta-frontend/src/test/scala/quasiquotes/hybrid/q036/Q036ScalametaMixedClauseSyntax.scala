package quasiquotes.hybrid.q036

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q036ScalametaClauseSummary(
    typeParameterCount: Int,
    contextBounds: List[List[String]],
    clauseModes: List[String],
    parameterNames: List[List[String]],
    parameterModifiers: List[List[List[String]]],
    defaults: List[List[Boolean]]
)

object Q036ScalametaMixedClauseSyntax:
  def inspect(source: String): Either[String, Q036ScalametaClauseSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val typeParameters = groups.flatMap(_.tparamClause.values)
          val clauses = groups.flatMap(_.paramClauses)
          Right(Q036ScalametaClauseSummary(
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
      case List(beforeModifiers, beforeName, beforeOrdinary, beforeImplicit, beforeResult, beforeBody, suffix)
          if beforeModifiers.trim.isEmpty &&
            beforeName.matches("(?s)\\s+def\\s+") &&
            beforeOrdinary.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
            beforeImplicit.matches("(?s)\\s*\\)\\s*\\(\\s*implicit\\s+\\.\\.\\s*") &&
            beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
            beforeBody.matches("(?s)\\s*=\\s*") &&
            suffix.trim.isEmpty =>
        val ordinaryMarker = beforeOrdinary.lastIndexOf("..")
        val implicitMarker = beforeImplicit.lastIndexOf("..")
        val source =
          "@deprecated(\"q036\", \"\") private[quasiquotes] final" + beforeName +
            "__q036_method" + beforeOrdinary.substring(0, ordinaryMarker) +
            "__q036_x: Int, __q036_y: String" + beforeImplicit.substring(0, implicitMarker) +
            "__q036_ord: Ordering[Int], __q036_num: Numeric[Int]" + beforeResult +
            "__q036_result" + beforeBody + "__q036_body" + suffix
        try
          TermQ3DialectPolicy.selected(source).parse[Stat] match
            case Parsed.Success(definition: Defn.Def) =>
              val groups = definition.paramClauseGroups
              val clauses = groups.flatMap(_.paramClauses)
              val valid =
                definition.name.value == "__q036_method" &&
                  definition.mods.map(_.syntax).exists(_.startsWith("@deprecated")) &&
                  definition.mods.map(_.syntax).contains("private[quasiquotes]") &&
                  definition.mods.map(_.syntax).contains("final") &&
                  groups.size == 1 &&
                  groups.head.tparamClause.values.isEmpty &&
                  clauses.size == 2 &&
                  clauses.head.mod.isEmpty &&
                  clauses(1).mod.exists(_.syntax == "implicit") &&
                  clauses.head.values.forall(_.mods.isEmpty) &&
                  clauses(1).values.forall(_.mods.map(_.syntax) == List("implicit")) &&
                  clauses.head.values.map(_.name.value) == List("__q036_x", "__q036_y") &&
                  clauses(1).values.map(_.name.value) == List("__q036_ord", "__q036_num") &&
                  clauses.flatten.forall(_.default.isEmpty) &&
                  definition.decltpe.exists(_.syntax == "__q036_result") &&
                  (definition.body match
                    case value: Term.Name => value.value == "__q036_body"
                    case _ => false)
              Either.cond(valid, (), "Scalameta sentinels did not preserve the exact mixed-clause structure")
            case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
            case error: Parsed.Error => Left(s"Scalameta rejected sentinel source: ${error.message}")
        catch
          case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
      case _ =>
        Left("only `$mods def $name(..$params)(implicit ..$implicitParams): $result = $body` is selected")
