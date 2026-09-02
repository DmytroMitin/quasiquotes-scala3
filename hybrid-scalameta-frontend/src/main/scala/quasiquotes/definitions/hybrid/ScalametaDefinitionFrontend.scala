package quasiquotes.definitions.hybrid

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.quoted.Quotes
import scala.util.control.NonFatal

import _root_.quasiquotes.construct.TypedSingleParameterDefinitionLowerer
import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.matching.{
  DefinitionPattern,
  RankedPatternSource,
  SingleParameterDefinitionPattern
}
import _root_.quasiquotes.types.TypeNormalForm
import _root_.quasiquotes.types.hybrid.ScalametaTypeFrontend

private[quasiquotes] object ScalametaDefinitionFrontend:
  final case class Failure(
      category: String,
      start: Int,
      end: Int,
      detail: String
  ) derives CanEqual:
    def message: String = s"$category[$start..$end]: $detail"

  final case class ConstructionProjection(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterTypePlaceholder: String,
      resultTypePlaceholder: String
  )

  final case class PatternProjection(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      bodySentinel: String
  )

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.TypeRepr]
  ): Either[Failure, q.reflect.DefDef] =
    if arguments == null || arguments.size != 2 then
      Left(
        Failure(
          "TYPE_SPLICE_ARITY_UNSUPPORTED",
          0,
          0,
          s"expected exactly two TypeRepr splices, but received ${Option(arguments).fold(0)(_.size)}."
        )
      )
    else
      projectConstruction(parts).flatMap(projection =>
        TypedSingleParameterDefinitionLowerer
          .lower(using q)(
            projection.methodName,
            projection.parameterName,
            arguments.head,
            arguments(1)
          )
          .left
          .map(detail => Failure("TYPED_DEFINITION_LOWERING_FAILURE", 0, 0, detail))
      )

  def compilePattern(
      parts: Seq[String]
  ): Either[Failure, SingleParameterDefinitionPattern] =
    projectPattern(parts).map(projection =>
      DefinitionPattern.singleParameterStructured(
        projection.methodName,
        projection.parameterName,
        projection.parameterType,
        projection.resultType
      )
    )

  private[quasiquotes] def projectConstruction(
      parts: Seq[String]
  ): Either[Failure, ConstructionProjection] =
    for
      checkedParts <- checkedParts(parts, 3, "exactly two TypeRepr splice positions")
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      parameterPlaceholder = freshIndexed(
        "__qq_scmeta_definition_type_",
        literalSource,
        Set.empty
      )
      resultPlaceholder = freshIndexed(
        "__qq_scmeta_definition_type_",
        literalSource,
        Set(parameterPlaceholder)
      )
      source =
        checkedParts(0) + parameterPlaceholder + checkedParts(1) +
          resultPlaceholder + checkedParts(2)
      definition <- parseDefinition(source)
      projection <- projectCommon(definition, source.length)
      _ <- require(
        projection.parameterTypeSource == parameterPlaceholder &&
          projection.resultTypeSource == resultPlaceholder,
        source.length,
        "TYPE_PLACEHOLDER_POSITION_UNSUPPORTED",
        "generated Type placeholders must occupy only the parameter and result declared-Type fields."
      )
      _ <- projection.body match
        case value: Term.Name if value.value == projection.parameterName.decoded => Right(())
        case _ =>
          unsupported(
            source.length,
            "BODY_PARAMETER_REFERENCE_REQUIRED",
            "the body must be the literal declared parameter reference."
          )
    yield ConstructionProjection(
      projection.methodName,
      projection.parameterName,
      parameterPlaceholder,
      resultPlaceholder
    )

  private[quasiquotes] def projectPattern(
      parts: Seq[String]
  ): Either[Failure, PatternProjection] =
    for
      checkedParts <- checkedParts(parts, 2, "exactly one complete-body capture slot")
      _ <- RankedPatternSource
        .unsupportedFamilyRankDiagnostic(checkedParts, "Definition")
        .fold[Either[Failure, Unit]](Right(()))(detail =>
          Left(Failure("DEFINITION_PATTERN_RANK_UNSUPPORTED", 0, 0, detail))
        )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set.empty
      )
      source = checkedParts.head + bodySentinel + checkedParts.last
      definition <- parseDefinition(source)
      projection <- projectCommon(definition, source.length)
      _ <- projection.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the single capture must occupy the complete Definition right-hand side."
          )
      parameterType <- normalForm(projection.parameterTypeSource, source.length)
      resultType <- normalForm(projection.resultTypeSource, source.length)
    yield PatternProjection(
      projection.methodName,
      projection.parameterName,
      parameterType,
      resultType,
      bodySentinel
    )

  private final case class CommonProjection(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterTypeSource: String,
      resultTypeSource: String,
      body: Term
  )

  private def projectCommon(
      definition: Defn.Def,
      sourceLength: Int
  ): Either[Failure, CommonProjection] =
    for
      _ <- require(
        definition.mods.isEmpty,
        sourceLength,
        "DEFINITION_MODIFIERS_UNSUPPORTED",
        "Definition modifiers are not supported."
      )
      methodName <- plainName(
        definition.name.value,
        definition.name.syntax,
        sourceLength,
        "METHOD_NAME_UNSUPPORTED"
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ =>
          unsupported(
            sourceLength,
            "PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED",
            "expected exactly one parameter-clause group."
          )
      _ <- require(
        group.tparamClause.values.isEmpty,
        sourceLength,
        "TYPE_PARAMETERS_UNSUPPORTED",
        "type parameters are not supported."
      )
      clause <- group.paramClauses match
        case value :: Nil if value.mod.isEmpty => Right(value)
        case _ =>
          unsupported(
            sourceLength,
            "PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "expected exactly one ordinary parameter clause."
          )
      parameter <- clause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            sourceLength,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "expected exactly one ordinary parameter without modifiers or a default."
          )
      parameterName <- plainName(
        parameter.name.value,
        parameter.name.syntax,
        sourceLength,
        "PARAMETER_NAME_UNSUPPORTED"
      )
      parameterType <- parameter.decltpe.toRight(
        Failure(
          "PARAMETER_TYPE_REQUIRED",
          0,
          sourceLength,
          "the parameter must have an explicit declared Type."
        )
      )
      resultType <- definition.decltpe.toRight(
        Failure(
          "RESULT_TYPE_REQUIRED",
          0,
          sourceLength,
          "the Definition must have an explicit result Type."
        )
      )
    yield CommonProjection(
      methodName,
      parameterName,
      parameterType.syntax,
      resultType.syntax,
      definition.body
    )

  private def parseDefinition(source: String): Either[Failure, Defn.Def] =
    try
      TermQ3DialectPolicy.selected(source).parse[Stat] match
        case Parsed.Success(definition: Defn.Def) => Right(definition)
        case Parsed.Success(_) =>
          unsupported(
            source.length,
            "ROOT_KIND_UNSUPPORTED",
            "expected exactly one method Definition root."
          )
        case error: Parsed.Error =>
          Left(
            Failure(
              "SCALAMETA_PARSE_FAILURE",
              error.pos.start,
              error.pos.end,
              "Scalameta rejected the Definition template."
            )
          )
    catch
      case NonFatal(_) =>
        Left(
          Failure(
            "SCALAMETA_PARSE_FAILURE",
            0,
            source.length,
            "Scalameta could not parse the Definition template."
          )
        )

  private def normalForm(
      source: String,
      definitionSourceLength: Int
  ): Either[Failure, TypeNormalForm] =
    ScalametaTypeFrontend
      .normalForm(source)
      .left
      .map(failure =>
        Failure(
          "DEFINITION_TYPE_UNSUPPORTED",
          0,
          definitionSourceLength,
          failure.message
        )
      )

  private def plainName(
      decoded: String,
      syntax: String,
      sourceLength: Int,
      category: String
  ): Either[Failure, DefinitionName] =
    if syntax != decoded then
      unsupported(
        sourceLength,
        category,
        "only ordinary unquoted ASCII identifiers are supported."
      )
    else
      DefinitionName
        .plain(decoded)
        .left
        .map(_ =>
          Failure(
            category,
            0,
            sourceLength,
            "only ordinary unquoted ASCII identifiers are supported."
          )
        )

  private def checkedParts(
      parts: Seq[String],
      expectedSize: Int,
      expectation: String
  ): Either[Failure, Vector[String]] =
    if parts == null then
      Left(Failure("SOURCE_MISSING", 0, 0, "StringContext parts must not be null."))
    else if parts.exists(_ == null) then
      Left(Failure("SOURCE_MISSING", 0, 0, "StringContext literal parts must not be null."))
    else if parts.size != expectedSize then
      Left(
        Failure(
          "INTERPOLATION_ARITY_UNSUPPORTED",
          0,
          0,
          s"expected $expectation."
        )
      )
    else Right(parts.toVector)

  private def exactSourceGuard(parts: Seq[String]): Either[Failure, Unit] =
    val source = parts.mkString
    if source.contains('`') then
      Left(
        Failure(
          "BACKTICKED_NAME_UNSUPPORTED",
          0,
          source.length,
          "backticked Definition names are outside the bounded surface."
        )
      )
    else if source.contains("//") || source.contains("/*") then
      Left(
        Failure(
          "COMMENT_NORMALIZATION_UNSUPPORTED",
          0,
          source.length,
          "comments are rejected before Scalameta normalization."
        )
      )
    else Right(())

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

  private def require(
      condition: Boolean,
      sourceLength: Int,
      category: String,
      detail: String
  ): Either[Failure, Unit] =
    if condition then Right(())
    else unsupported(sourceLength, category, detail)

  private def unsupported[A](
      sourceLength: Int,
      category: String,
      detail: String
  ): Either[Failure, A] =
    Left(Failure(category, 0, sourceLength, detail))
