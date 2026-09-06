package quasiquotes.hybrid.q040

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy

final case class Q040ScalametaParameterSummary(
    name: String,
    typeFamily: Option[String],
    typeSyntax: Option[String],
    repeatedElementFamily: Option[String],
    repeatedElementSyntax: Option[String],
    defaultPresent: Boolean,
    modifiers: List[String]
)

final case class Q040ScalametaDefinitionSummary(
    typeParameterCount: Int,
    clauseModes: List[String],
    parameters: List[List[Q040ScalametaParameterSummary]],
    modifiers: List[String]
)

object Q040ScalametaRepeatedSyntax:
  def inspect(source: String): Either[String, Q040ScalametaDefinitionSummary] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) =>
          val groups = definition.paramClauseGroups
          val clauses = groups.flatMap(_.paramClauses)
          Right(Q040ScalametaDefinitionSummary(
            groups.flatMap(_.tparamClause.values).size,
            clauses.map(_.mod.map(_.syntax).getOrElse("ordinary")),
            clauses.map(_.values.map(parameter =>
              val repeatedElement = parameter.decltpe.collect {
                case repeated: Type.Repeated => repeated.tpe
              }
              Q040ScalametaParameterSummary(
                parameter.name.value,
                parameter.decltpe.map(_.productPrefix),
                parameter.decltpe.map(_.syntax),
                repeatedElement.map(_.productPrefix),
                repeatedElement.map(_.syntax),
                parameter.default.nonEmpty,
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
