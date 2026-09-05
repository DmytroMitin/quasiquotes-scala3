package quasiquotes.hybrid.q030

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q030ScalametaTypeParameter(
    name: String,
    lowerBound: Option[String],
    upperBound: Option[String],
    contextBounds: List[String]
)

final case class Q030ScalametaParameter(
    name: String,
    declaredType: Option[String],
    modifiers: List[String],
    hasDefault: Boolean
)

final case class Q030ScalametaDefinitionSummary(
    typeParameters: List[Q030ScalametaTypeParameter],
    clauseModes: List[String],
    parameterClauses: List[List[Q030ScalametaParameter]]
)

object Q030ScalametaDefinitionSyntax:
  def inspect(source: String): Either[String, Q030ScalametaDefinitionSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val tparams = groups.flatMap(_.tparamClause.values)
          val clauses = groups.flatMap(_.paramClauses)
          Right(Q030ScalametaDefinitionSummary(
            tparams.map(parameter =>
              Q030ScalametaTypeParameter(
                parameter.name.value,
                parameter.tbounds.lo.map(_.syntax),
                parameter.tbounds.hi.map(_.syntax),
                parameter.bounds.context.map(_.syntax)
              )
            ),
            clauses.map(_.mod.map(_.syntax).getOrElse("ordinary")),
            clauses.map(_.values.map(parameter =>
              Q030ScalametaParameter(
                parameter.name.value,
                parameter.decltpe.map(_.syntax),
                parameter.mods.map(_.syntax),
                parameter.default.nonEmpty
              )
            ))
          ))
        case Parsed.Success(other) =>
          Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
        case error: Parsed.Error =>
          Left(s"Scalameta rejected source at ${error.pos.start}..${error.pos.end}: ${error.message}")
    catch
      case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
