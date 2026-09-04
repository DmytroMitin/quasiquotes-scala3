package quasiquotes.hybrid.q027

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q027ScalametaDefinitionSummary(
    clauseModes: List[String],
    parameterNames: List[List[String]],
    parameterModifiers: List[List[List[String]]],
    contextBounds: List[List[String]]
)

object Q027ScalametaDefinitionClauseSyntax:
  def inspect(source: String): Either[String, Q027ScalametaDefinitionSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val clauses = groups.flatMap(_.paramClauses)
          val tparams = groups.flatMap(_.tparamClause.values)
          Right(Q027ScalametaDefinitionSummary(
            clauses.map(_.mod.map(_.syntax).getOrElse("ordinary")),
            clauses.map(_.values.map(_.name.value)),
            clauses.map(_.values.map(_.mods.map(_.syntax))),
            tparams.map(_.bounds.context.map(_.syntax))
          ))
        case Parsed.Success(other) =>
          Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
        case error: Parsed.Error =>
          Left(s"Scalameta rejected source at ${error.pos.start}..${error.pos.end}: ${error.message}")
    catch
      case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
