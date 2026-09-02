package quasiquotes.hybrid

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.types.hybrid.ScalametaTypeFrontend

private[hybrid] object Q006DefinitionFrontendProbe:
  final case class Projection(
      methodName: String,
      parameterName: String,
      parameterTypeSource: String,
      resultTypeSource: String,
      bodyParameterName: String
  )

  final case class BodyPatternProjection(
      methodName: String,
      parameterName: String,
      parameterTypeSource: String,
      resultTypeSource: String,
      captureName: String
  )

  final case class ConstructionProjection(
      methodName: String,
      parameterName: String,
      parameterTypeSource: String,
      resultTypeSource: String,
      bodyParameterName: String
  )

  def project(
      source: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[String, Projection] =
    exactSourceGuard(source)
      .flatMap(_ => parseDefinition(source, dialect))
      .flatMap(projectDefinition)

  def semanticTypes(
      projection: Projection
  ): Either[String, (String, String)] =
    for
      parameter <- ScalametaTypeFrontend
        .normalForm(projection.parameterTypeSource)
        .left
        .map(_.message)
      result <- ScalametaTypeFrontend
        .normalForm(projection.resultTypeSource)
        .left
        .map(_.message)
      _ <- require(parameter == result, "PARAMETER_RESULT_TYPE_MISMATCH")
    yield parameter.render -> result.render

  def projectBodyPattern(
      parts: Seq[String]
  ): Either[String, BodyPatternProjection] =
    if parts == null || parts.size != 2 || parts.exists(_ == null) then
      Left("BODY_CAPTURE_ARITY_UNSUPPORTED")
    else
      val captureName = freshName("__qq_q006_body_0", parts.mkString)
      val source = parts.head + captureName + parts.last
      exactSourceGuard(source)
        .flatMap(_ => parseDefinition(source, TermQ3DialectPolicy.selected))
        .flatMap(projectPatternDefinition(_, captureName))

  def projectConstruction(
      parts: Seq[String]
  ): Either[String, ConstructionProjection] =
    if parts == null || parts.size != 3 || parts.exists(_ == null) then
      Left("TYPE_SPLICE_ARITY_UNSUPPORTED")
    else
      val literalSource = parts.mkString
      val parameterPlaceholder = freshIndexed("__qq_q006_type_", literalSource, Set.empty)
      val resultPlaceholder = freshIndexed(
        "__qq_q006_type_",
        literalSource,
        Set(parameterPlaceholder)
      )
      val source =
        parts(0) + parameterPlaceholder + parts(1) + resultPlaceholder + parts(2)
      project(source).flatMap { projection =>
        require(
          projection.parameterTypeSource == parameterPlaceholder &&
            projection.resultTypeSource == resultPlaceholder,
          "TYPE_PLACEHOLDER_POSITION_UNSUPPORTED"
        ).map(_ =>
          ConstructionProjection(
            projection.methodName,
            projection.parameterName,
            projection.parameterTypeSource,
            projection.resultTypeSource,
            projection.bodyParameterName
          )
        )
      }

  private def projectDefinition(definition: Defn.Def): Either[String, Projection] =
    for
      _ <- require(definition.mods.isEmpty, "DEFINITION_MODIFIERS_UNSUPPORTED")
      _ <- require(definition.name.syntax == definition.name.value, "METHOD_NAME_UNSUPPORTED")
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ => Left("PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED")
      _ <- require(group.tparamClause.values.isEmpty, "TYPE_PARAMETERS_UNSUPPORTED")
      clause <- group.paramClauses match
        case value :: Nil if value.mod.isEmpty => Right(value)
        case _ => Left("PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED")
      parameter <- clause.values match
        case value :: Nil
            if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ => Left("PARAMETER_TOPOLOGY_UNSUPPORTED")
      _ <- require(parameter.name.syntax == parameter.name.value, "PARAMETER_NAME_UNSUPPORTED")
      parameterType <- parameter.decltpe.toRight("PARAMETER_TYPE_REQUIRED")
      resultType <- definition.decltpe.toRight("RESULT_TYPE_REQUIRED")
      bodyName <- definition.body match
        case value: Term.Name => Right(value.value)
        case _ => Left("BODY_PARAMETER_REFERENCE_REQUIRED")
      _ <- require(bodyName == parameter.name.value, "BODY_PARAMETER_MISMATCH")
    yield Projection(
      definition.name.value,
      parameter.name.value,
      parameterType.syntax,
      resultType.syntax,
      bodyName
    )

  private def projectPatternDefinition(
      definition: Defn.Def,
      captureName: String
  ): Either[String, BodyPatternProjection] =
    for
      _ <- require(definition.mods.isEmpty, "DEFINITION_MODIFIERS_UNSUPPORTED")
      _ <- require(definition.name.syntax == definition.name.value, "METHOD_NAME_UNSUPPORTED")
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ => Left("PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED")
      _ <- require(group.tparamClause.values.isEmpty, "TYPE_PARAMETERS_UNSUPPORTED")
      clause <- group.paramClauses match
        case value :: Nil if value.mod.isEmpty => Right(value)
        case _ => Left("PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED")
      parameter <- clause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ => Left("PARAMETER_TOPOLOGY_UNSUPPORTED")
      _ <- require(parameter.name.syntax == parameter.name.value, "PARAMETER_NAME_UNSUPPORTED")
      parameterType <- parameter.decltpe.toRight("PARAMETER_TYPE_REQUIRED")
      resultType <- definition.decltpe.toRight("RESULT_TYPE_REQUIRED")
      _ <- definition.body match
        case value: Term.Name if value.value == captureName => Right(())
        case _ => Left("COMPLETE_BODY_CAPTURE_REQUIRED")
    yield BodyPatternProjection(
      definition.name.value,
      parameter.name.value,
      parameterType.syntax,
      resultType.syntax,
      captureName
    )

  private def freshName(base: String, source: String): String =
    Iterator
      .from(0)
      .map(index => if index == 0 then base else s"${base.dropRight(1)}$index")
      .find(candidate => !source.contains(candidate))
      .get

  private def parseDefinition(
      source: String,
      dialect: Dialect
  ): Either[String, Defn.Def] =
    try
      dialect(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) => Right(definition)
        case Parsed.Success(other) => Left(s"ROOT_KIND_UNSUPPORTED:${other.productPrefix}")
        case error: Parsed.Error =>
          Left(s"SCALAMETA_PARSE_FAILURE:${error.pos.start}:${error.pos.end}:${error.message}")
    catch
      case NonFatal(error) =>
        Left(
          s"SCALAMETA_PARSE_FAILURE:0:${source.length}:${error.getClass.getSimpleName}:${Option(error.getMessage).getOrElse("")}"
        )

  private def freshIndexed(
      prefix: String,
      source: String,
      reserved: Set[String]
  ): String =
    Iterator
      .from(0)
      .map(index => s"$prefix$index")
      .find(candidate => !source.contains(candidate) && !reserved.contains(candidate))
      .get

  private def exactSourceGuard(source: String): Either[String, Unit] =
    if source == null then Left("SOURCE_MISSING")
    else if source.contains('`') then Left("BACKTICKED_NAME_UNSUPPORTED")
    else if source.contains("//") || source.contains("/*") then
      Left("COMMENT_NORMALIZATION_UNSUPPORTED")
    else Right(())

  private def require(condition: Boolean, failure: String): Either[String, Unit] =
    Either.cond(condition, (), failure)
