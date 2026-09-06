package quasiquotes.hybrid.q038

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q038ScalametaParameterSummary(
    name: String,
    typeSyntax: Option[String],
    defaultPresent: Boolean,
    defaultFamily: Option[String],
    defaultSyntax: Option[String],
    modifiers: List[String]
)

final case class Q038ScalametaDefinitionSummary(
    typeParameterCount: Int,
    clauseModes: List[String],
    parameters: List[List[Q038ScalametaParameterSummary]],
    modifiers: List[String]
)

object Q038ScalametaDefaultSyntax:
  def inspect(source: String): Either[String, Q038ScalametaDefinitionSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val clauses = groups.flatMap(_.paramClauses)
          Right(Q038ScalametaDefinitionSummary(
            groups.flatMap(_.tparamClause.values).size,
            clauses.map(_.mod.map(_.syntax).getOrElse("ordinary")),
            clauses.map(_.values.map(parameter =>
              Q038ScalametaParameterSummary(
                parameter.name.value,
                parameter.decltpe.map(_.syntax),
                parameter.default.nonEmpty,
                parameter.default.map(_.productPrefix),
                parameter.default.map(_.syntax),
                parameter.mods.map(_.syntax)
              )
            )),
            definition.mods.map(_.syntax)
          ))
        case Parsed.Success(other) =>
          Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
        case error: Parsed.Error =>
          Left(s"Scalameta rejected source at ${error.pos.start}..${error.pos.end}: ${error.message}")
    catch
      case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
