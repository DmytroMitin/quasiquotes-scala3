package quasiquotes.definitions.hybrid

import scala.meta.*
import scala.meta.parsers.Parsed
import scala.quoted.Quotes
import scala.util.control.NonFatal

import _root_.quasiquotes.construct.{
  TypedSingleParameterDefinitionLowerer,
  TypedTwoParameterDefinitionLowerer
}
import _root_.quasiquotes.definitions.DefinitionName
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.matching.{
  DefinitionPattern,
  DefinitionPatternExtractor,
  RankedPatternSource,
  SingleParameterDefinitionPattern
}
import _root_.quasiquotes.types.TypeNormalForm
import _root_.quasiquotes.types.hybrid.ScalametaTypeFrontend

private[quasiquotes] object ScalametaDefinitionFrontend:
  enum PatternKind:
    case SingleParameter
    case ExactTwo
    case RankedParameterSequence
    case RankedParameterClauseSequence
    case CapturedNameRankedParameterClauseSequenceCapturedResult
    case CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
    case CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult

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

  final case class ExactTwoConstructionProjection(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      secondParameterName: DefinitionName,
      firstParameterTypePlaceholder: String,
      secondParameterTypePlaceholder: String,
      resultTypePlaceholder: String,
      selectedParameterName: DefinitionName
  )

  final case class ExactTwoPatternProjection(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      firstParameterType: TypeNormalForm,
      secondParameterName: DefinitionName,
      secondParameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      bodySentinel: String
  )

  final case class RankedPatternProjection(
      parameterSentinel: String,
      bodySentinel: String
  )

  final case class RankedParameterClauseSequenceProjection(
      firstParameterSentinel: String,
      secondParameterSentinel: String,
      bodySentinel: String
  )

  final case class CapturedNameRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel: String,
      firstParameterSentinel: String,
      secondParameterSentinel: String,
      resultSentinel: String,
      bodySentinel: String
  )

  final case class CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel: String,
      firstTypeParameterSentinel: String,
      secondTypeParameterSentinel: String,
      firstParameterSentinel: String,
      secondParameterSentinel: String,
      resultSentinel: String,
      bodySentinel: String
  )

  final case class CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel: String,
      firstTypeParameterSentinel: String,
      secondTypeParameterSentinel: String,
      firstParameterSentinel: String,
      secondParameterSentinel: String,
      resultSentinel: String,
      bodySentinel: String
  )

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.TypeRepr]
  ): Either[Failure, q.reflect.DefDef] =
    if arguments == null then
      Left(
        Failure(
          "TYPE_SPLICE_ARITY_UNSUPPORTED",
          0,
          0,
          "expected exactly two or three TypeRepr splices, but received 0."
        )
      )
    else arguments.size match
      case 2 =>
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
      case 3 =>
        projectExactTwoConstruction(parts).flatMap(projection =>
          TypedTwoParameterDefinitionLowerer
            .lower(using q)(
              projection.methodName,
              projection.firstParameterName,
              arguments.head,
              projection.secondParameterName,
              arguments(1),
              arguments(2),
              projection.selectedParameterName
            )
            .left
            .map(detail => Failure("TYPED_DEFINITION_LOWERING_FAILURE", 0, 0, detail))
        )
      case other =>
        Left(
          Failure(
            "TYPE_SPLICE_ARITY_UNSUPPORTED",
            0,
            0,
            s"expected exactly two or three TypeRepr splices, but received $other."
          )
        )

  def classifyPatternParts(parts: Seq[String]): Either[Failure, PatternKind] =
    projectPattern(parts) match
      case Right(_) => Right(PatternKind.SingleParameter)
      case Left(_) =>
        projectExactTwoPattern(parts) match
          case Right(_) => Right(PatternKind.ExactTwo)
          case Left(exactTwoFailure) =>
            RankedPatternSource.unsupportedFamilyRankDiagnostic(parts, "Definition") match
              case Some(detail) =>
                projectCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
                  parts
                ) match
                  case Right(_) =>
                    Right(
                      PatternKind.CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
                    )
                  case Left(_) =>
                    projectCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
                      parts
                    ) match
                      case Right(_) =>
                        Right(
                          PatternKind.CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult
                        )
                      case Left(_) =>
                        projectCapturedNameRankedParameterClauseSequenceCapturedResultPattern(parts) match
                          case Right(_) =>
                            Right(PatternKind.CapturedNameRankedParameterClauseSequenceCapturedResult)
                          case Left(_) =>
                            projectRankedParameterClauseSequencePattern(parts) match
                              case Right(_) => Right(PatternKind.RankedParameterClauseSequence)
                              case Left(_) =>
                                projectRankedPattern(parts) match
                                  case Right(_) => Right(PatternKind.RankedParameterSequence)
                                  case Left(_) =>
                                    Left(Failure("DEFINITION_PATTERN_RANK_UNSUPPORTED", 0, 0, detail))
              case None => Left(exactTwoFailure)

  def compileRankedPattern(
      parts: Seq[String]
  ): Either[Failure, RankedPatternProjection] =
    projectRankedPattern(parts)

  def compileRankedParameterClauseSequencePattern(
      parts: Seq[String]
  ): Either[Failure, RankedParameterClauseSequenceProjection] =
    projectRankedParameterClauseSequencePattern(parts)

  def compileCapturedNameRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[Failure, CapturedNameRankedParameterClauseSequenceCapturedResultProjection] =
    projectCapturedNameRankedParameterClauseSequenceCapturedResultPattern(parts)

  def compileCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[
    Failure,
    CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection
  ] =
    projectCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts
    )

  def compileCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[
    Failure,
    CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection
  ] =
    projectCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts
    )

  def compileExactTwoPattern(
      parts: Seq[String]
  ): Either[Failure, DefinitionPatternExtractor] =
    projectExactTwoPattern(parts).map(projection =>
      DefinitionPattern.structured(
        projection.methodName,
        Vector(
          Vector(
            (projection.firstParameterName, projection.firstParameterType),
            (projection.secondParameterName, projection.secondParameterType)
          )
        ),
        projection.resultType
      )
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

  private[quasiquotes] def projectExactTwoConstruction(
      parts: Seq[String]
  ): Either[Failure, ExactTwoConstructionProjection] =
    for
      checkedParts <- checkedParts(parts, 4, "exactly three TypeRepr splice positions")
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      firstPlaceholder = freshIndexed(
        "__qq_scmeta_definition_type_",
        literalSource,
        Set.empty
      )
      secondPlaceholder = freshIndexed(
        "__qq_scmeta_definition_type_",
        literalSource,
        Set(firstPlaceholder)
      )
      resultPlaceholder = freshIndexed(
        "__qq_scmeta_definition_type_",
        literalSource,
        Set(firstPlaceholder, secondPlaceholder)
      )
      source =
        checkedParts(0) + firstPlaceholder + checkedParts(1) +
          secondPlaceholder + checkedParts(2) + resultPlaceholder + checkedParts(3)
      definition <- parseDefinition(source)
      projection <- projectExactTwoCommon(definition, source.length)
      _ <- require(
        projection.firstParameterTypeSource == firstPlaceholder &&
          projection.secondParameterTypeSource == secondPlaceholder &&
          projection.resultTypeSource == resultPlaceholder,
        source.length,
        "TYPE_PLACEHOLDER_POSITION_UNSUPPORTED",
        "generated Type placeholders must occupy only the first parameter, second parameter, and result declared-Type fields."
      )
      selectedParameterName <- projection.body match
        case value: Term.Name if value.value == projection.firstParameterName.decoded =>
          Right(projection.firstParameterName)
        case value: Term.Name if value.value == projection.secondParameterName.decoded =>
          Right(projection.secondParameterName)
        case _ =>
          unsupported(
            source.length,
            "BODY_PARAMETER_REFERENCE_REQUIRED",
            "the body must be the literal first or second declared parameter reference."
          )
    yield ExactTwoConstructionProjection(
      projection.methodName,
      projection.firstParameterName,
      projection.secondParameterName,
      firstPlaceholder,
      secondPlaceholder,
      resultPlaceholder,
      selectedParameterName
    )

  private[quasiquotes] def projectExactTwoPattern(
      parts: Seq[String]
  ): Either[Failure, ExactTwoPatternProjection] =
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
      projection <- projectExactTwoCommon(definition, source.length)
      _ <- projection.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the single capture must occupy the complete Definition right-hand side."
          )
      firstParameterType <- normalForm(projection.firstParameterTypeSource, source.length)
      _ <- exactTwoAdmittedType(firstParameterType, source.length)
      secondParameterType <- normalForm(projection.secondParameterTypeSource, source.length)
      _ <- exactTwoAdmittedType(secondParameterType, source.length)
      resultType <- normalForm(projection.resultTypeSource, source.length)
      _ <- exactTwoAdmittedType(resultType, source.length)
    yield ExactTwoPatternProjection(
      projection.methodName,
      projection.firstParameterName,
      firstParameterType,
      projection.secondParameterName,
      secondParameterType,
      resultType,
      bodySentinel
    )

  private[quasiquotes] def projectRankedPattern(
      parts: Seq[String]
  ): Either[Failure, RankedPatternProjection] =
    for
      checkedParts <- checkedParts(
        parts,
        3,
        "one complete parameter-sequence capture and one complete-body capture"
      )
      _ <- require(
        isExactRankedParameterSequence(checkedParts),
        checkedParts.mkString.length,
        "DEFINITION_PATTERN_RANK_UNSUPPORTED",
        "rank-2 capture is supported only as `def collect(..$params): Int = $body`."
      )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      parameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set.empty
      )
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set(parameterSentinel)
      )
      markerOffset = checkedParts.head.lastIndexOf("..")
      source =
        checkedParts.head.substring(0, markerOffset) +
          s"$parameterSentinel: Int" + checkedParts(1) + bodySentinel + checkedParts(2)
      definition <- parseDefinition(source)
      projection <- projectCommon(definition, source.length)
      _ <- require(
        projection.methodName.decoded == "collect" &&
          projection.parameterName.decoded == parameterSentinel &&
          projection.parameterTypeSource == "Int" &&
          projection.resultTypeSource == "Int",
        source.length,
        "DEFINITION_PATTERN_RANK_UNSUPPORTED",
        "the ranked template must keep the fixed collect name and Int result Type."
      )
      _ <- projection.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the body capture must occupy the complete Definition right-hand side."
          )
    yield RankedPatternProjection(parameterSentinel, bodySentinel)

  private[quasiquotes] def projectRankedParameterClauseSequencePattern(
      parts: Seq[String]
  ): Either[Failure, RankedParameterClauseSequenceProjection] =
    for
      checkedParts <- checkedParts(
        parts,
        3,
        "one complete parameter-clause-sequence capture and one complete-body capture"
      )
      _ <- require(
        isExactRankedParameterClauseSequence(checkedParts),
        checkedParts.mkString.length,
        "DEFINITION_PATTERN_RANK_UNSUPPORTED",
        "rank-3 capture is supported only as `def collect(...$paramss): Int = $body`."
      )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      firstParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set.empty
      )
      secondParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(firstParameterSentinel)
      )
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set(firstParameterSentinel, secondParameterSentinel)
      )
      markerOffset = checkedParts.head.lastIndexOf("...")
      source =
        checkedParts.head.substring(0, markerOffset) +
          s"$firstParameterSentinel: Int)($secondParameterSentinel: String" +
          checkedParts(1) + bodySentinel + checkedParts(2)
      definition <- parseDefinition(source)
      _ <- require(
        definition.mods.isEmpty &&
          definition.name.value == "collect" &&
          definition.name.syntax == "collect",
        source.length,
        "DEFINITION_PATTERN_RANK_UNSUPPORTED",
        "the ranked template must keep the fixed ordinary collect method."
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED",
            "expected exactly one parameter-clause group."
          )
      _ <- require(
        group.tparamClause.values.isEmpty,
        source.length,
        "TYPE_PARAMETERS_UNSUPPORTED",
        "type parameters are not supported."
      )
      clauses <- group.paramClauses match
        case first :: second :: Nil if first.mod.isEmpty && second.mod.isEmpty =>
          Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "the rank-3 sentinel must preserve two ordinary parameter clauses."
          )
      (firstClause, secondClause) = clauses
      firstParameter <- firstClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the first rank-3 sentinel clause must contain one ordinary parameter."
          )
      secondParameter <- secondClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the second rank-3 sentinel clause must contain one ordinary parameter."
          )
      _ <- require(
        firstParameter.name.value == firstParameterSentinel &&
          firstParameter.name.syntax == firstParameterSentinel &&
          firstParameter.decltpe.exists(_.syntax == "Int") &&
          secondParameter.name.value == secondParameterSentinel &&
          secondParameter.name.syntax == secondParameterSentinel &&
          secondParameter.decltpe.exists(_.syntax == "String") &&
          definition.decltpe.exists(_.syntax == "Int"),
        source.length,
        "DEFINITION_PATTERN_RANK_UNSUPPORTED",
        "the rank-3 sentinels and fixed Int result Type must remain in their exact structural positions."
      )
      _ <- definition.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the body capture must occupy the complete Definition right-hand side."
          )
    yield RankedParameterClauseSequenceProjection(
      firstParameterSentinel,
      secondParameterSentinel,
      bodySentinel
    )

  private[quasiquotes] def projectCapturedNameRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[Failure, CapturedNameRankedParameterClauseSequenceCapturedResultProjection] =
    for
      checkedParts <- checkedParts(
        parts,
        5,
        "semantic name, complete parameter-clause sequence, semantic result Type, and complete body captures"
      )
      _ <- require(
        isExactCapturedNameRankedParameterClauseSequenceCapturedResult(checkedParts),
        checkedParts.mkString.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "only `def $name(...$paramss): $result = $body` is supported for the four-capture Definition shape."
      )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      methodSentinel = freshIndexed(
        "__qq_scmeta_definition_method_",
        literalSource,
        Set.empty
      )
      firstParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(methodSentinel)
      )
      secondParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(methodSentinel, firstParameterSentinel)
      )
      resultSentinel = freshIndexed(
        "__qq_scmeta_definition_result_",
        literalSource,
        Set(methodSentinel, firstParameterSentinel, secondParameterSentinel)
      )
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set(methodSentinel, firstParameterSentinel, secondParameterSentinel, resultSentinel)
      )
      markerOffset = checkedParts(1).lastIndexOf("...")
      source =
        checkedParts(0) + methodSentinel +
          checkedParts(1).substring(0, markerOffset) +
          s"$firstParameterSentinel: Int)($secondParameterSentinel: String" +
          checkedParts(2) + resultSentinel + checkedParts(3) + bodySentinel + checkedParts(4)
      definition <- parseDefinition(source)
      _ <- require(
        definition.mods.isEmpty &&
          definition.name.value == methodSentinel &&
          definition.name.syntax == methodSentinel,
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the method-name sentinel must occupy the complete Definition name position."
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED",
            "expected exactly one parameter-clause group."
          )
      _ <- require(
        group.tparamClause.values.isEmpty,
        source.length,
        "TYPE_PARAMETERS_UNSUPPORTED",
        "type parameters are not supported."
      )
      clauses <- group.paramClauses match
        case first :: second :: Nil if first.mod.isEmpty && second.mod.isEmpty =>
          Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "the rank-3 sentinel must preserve two ordinary parameter clauses."
          )
      (firstClause, secondClause) = clauses
      firstParameter <- firstClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the first rank-3 sentinel clause must contain one ordinary parameter."
          )
      secondParameter <- secondClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the second rank-3 sentinel clause must contain one ordinary parameter."
          )
      _ <- require(
        firstParameter.name.value == firstParameterSentinel &&
          firstParameter.name.syntax == firstParameterSentinel &&
          firstParameter.decltpe.exists(_.syntax == "Int") &&
          secondParameter.name.value == secondParameterSentinel &&
          secondParameter.name.syntax == secondParameterSentinel &&
          secondParameter.decltpe.exists(_.syntax == "String") &&
          definition.decltpe.exists {
            case value: Type.Name =>
              value.value == resultSentinel && value.syntax == resultSentinel
            case _ => false
          },
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the paramss and result sentinels must remain in their exact structural positions."
      )
      _ <- definition.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the body capture must occupy the complete Definition right-hand side."
          )
    yield CapturedNameRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel,
      firstParameterSentinel,
      secondParameterSentinel,
      resultSentinel,
      bodySentinel
    )

  private[quasiquotes] def projectCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[
    Failure,
    CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection
  ] =
    for
      checkedParts <- checkedParts(
        parts,
        6,
        "semantic name, complete type-parameter sequence, complete parameter-clause sequence, semantic result Type, and complete body captures"
      )
      _ <- require(
        isExactCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
          checkedParts
        ),
        checkedParts.mkString.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "only `def $name[..$tparams](...$paramss): $result = $body` is supported for the five-capture Definition shape."
      )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      methodSentinel = freshIndexed("__qq_scmeta_definition_method_", literalSource, Set.empty)
      firstTypeParameterSentinel = freshIndexed(
        "__QqScmetaDefinitionTypeParameter_",
        literalSource,
        Set(methodSentinel)
      )
      secondTypeParameterSentinel = freshIndexed(
        "__QqScmetaDefinitionTypeParameter_",
        literalSource,
        Set(methodSentinel, firstTypeParameterSentinel)
      )
      firstParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(methodSentinel, firstTypeParameterSentinel, secondTypeParameterSentinel)
      )
      secondParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel
        )
      )
      resultSentinel = freshIndexed(
        "__qq_scmeta_definition_result_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel,
          secondParameterSentinel
        )
      )
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel,
          secondParameterSentinel,
          resultSentinel
        )
      )
      typeMarkerOffset = checkedParts(1).lastIndexOf("..")
      termMarkerOffset = checkedParts(2).lastIndexOf("...")
      source =
        checkedParts(0) + methodSentinel +
          checkedParts(1).substring(0, typeMarkerOffset) +
          s"$firstTypeParameterSentinel, $secondTypeParameterSentinel <: List[$firstTypeParameterSentinel]" +
          checkedParts(2).substring(0, termMarkerOffset) +
          s"$firstParameterSentinel: $firstTypeParameterSentinel)($secondParameterSentinel: $secondTypeParameterSentinel" +
          checkedParts(3) + resultSentinel + checkedParts(4) + bodySentinel + checkedParts(5)
      definition <- parseDefinition(source)
      _ <- require(
        definition.mods.isEmpty &&
          definition.name.value == methodSentinel &&
          definition.name.syntax == methodSentinel,
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the method-name sentinel must occupy the complete Definition name position."
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED",
            "expected exactly one parameter-clause group."
          )
      typeParameters <- group.tparamClause.values match
        case first :: second :: Nil => Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the type-parameter sentinel must preserve two ordered parameters."
          )
      (firstTypeParameter, secondTypeParameter) = typeParameters
      _ <- require(
        firstTypeParameter.mods.isEmpty &&
          secondTypeParameter.mods.isEmpty &&
          firstTypeParameter.name.value == firstTypeParameterSentinel &&
          firstTypeParameter.name.syntax == firstTypeParameterSentinel &&
          secondTypeParameter.name.value == secondTypeParameterSentinel &&
          secondTypeParameter.name.syntax == secondTypeParameterSentinel &&
          secondTypeParameter.tbounds.hi.exists(
            _.syntax == s"List[$firstTypeParameterSentinel]"
          ),
        source.length,
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "the ordered type-parameter sentinels and dependent bound must remain structural."
      )
      clauses <- group.paramClauses match
        case first :: second :: Nil if first.mod.isEmpty && second.mod.isEmpty =>
          Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "the rank-3 sentinel must preserve two ordinary parameter clauses."
          )
      (firstClause, secondClause) = clauses
      firstParameter <- firstClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the first rank-3 sentinel clause must contain one ordinary parameter."
          )
      secondParameter <- secondClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the second rank-3 sentinel clause must contain one ordinary parameter."
          )
      _ <- require(
        firstParameter.name.value == firstParameterSentinel &&
          firstParameter.decltpe.exists(_.syntax == firstTypeParameterSentinel) &&
          secondParameter.name.value == secondParameterSentinel &&
          secondParameter.decltpe.exists(_.syntax == secondTypeParameterSentinel) &&
          definition.decltpe.exists {
            case value: Type.Name =>
              value.value == resultSentinel && value.syntax == resultSentinel
            case _ => false
          },
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the type-parameter, paramss, and result sentinels must remain in their exact positions."
      )
      _ <- definition.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the body capture must occupy the complete Definition right-hand side."
          )
    yield CapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel,
      firstTypeParameterSentinel,
      secondTypeParameterSentinel,
      firstParameterSentinel,
      secondParameterSentinel,
      resultSentinel,
      bodySentinel
    )

  private[quasiquotes] def projectCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultPattern(
      parts: Seq[String]
  ): Either[
    Failure,
    CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection
  ] =
    for
      checkedParts <- checkedParts(
        parts,
        7,
        "semantic modifiers, semantic name, complete type-parameter sequence, complete parameter-clause sequence, semantic result Type, and complete body captures"
      )
      _ <- require(
        isExactCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
          checkedParts
        ),
        checkedParts.mkString.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "only `$mods def $name[..$tparams](...$paramss): $result = $body` is supported for the six-capture Definition shape."
      )
      _ <- exactSourceGuard(checkedParts)
      literalSource = checkedParts.mkString
      methodSentinel = freshIndexed("__qq_scmeta_definition_method_", literalSource, Set.empty)
      firstTypeParameterSentinel = freshIndexed(
        "__QqScmetaDefinitionTypeParameter_",
        literalSource,
        Set(methodSentinel)
      )
      secondTypeParameterSentinel = freshIndexed(
        "__QqScmetaDefinitionTypeParameter_",
        literalSource,
        Set(methodSentinel, firstTypeParameterSentinel)
      )
      firstParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(methodSentinel, firstTypeParameterSentinel, secondTypeParameterSentinel)
      )
      secondParameterSentinel = freshIndexed(
        "__qq_scmeta_definition_parameter_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel
        )
      )
      resultSentinel = freshIndexed(
        "__qq_scmeta_definition_result_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel,
          secondParameterSentinel
        )
      )
      bodySentinel = freshIndexed(
        "__qq_scmeta_definition_body_",
        literalSource,
        Set(
          methodSentinel,
          firstTypeParameterSentinel,
          secondTypeParameterSentinel,
          firstParameterSentinel,
          secondParameterSentinel,
          resultSentinel
        )
      )
      typeMarkerOffset = checkedParts(2).lastIndexOf("..")
      termMarkerOffset = checkedParts(3).lastIndexOf("...")
      source =
        "@deprecated(\"q025\", \"\") private[quasiquotes] final" + checkedParts(1) +
          methodSentinel + checkedParts(2).substring(0, typeMarkerOffset) +
          s"$firstTypeParameterSentinel, $secondTypeParameterSentinel <: List[$firstTypeParameterSentinel]" +
          checkedParts(3).substring(0, termMarkerOffset) +
          s"$firstParameterSentinel: $firstTypeParameterSentinel)($secondParameterSentinel: $secondTypeParameterSentinel" +
          checkedParts(4) + resultSentinel + checkedParts(5) + bodySentinel + checkedParts(6)
      definition <- parseDefinition(source)
      _ <- require(
        (definition.mods match
          case List(_: Mod.Annot, _: Mod.Private, _: Mod.Final) => true
          case _ => false) &&
          definition.name.value == methodSentinel &&
          definition.name.syntax == methodSentinel,
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the ordered annotation/qualified-private/final probe and method-name sentinel must remain structural."
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED",
            "expected exactly one parameter-clause group."
          )
      typeParameters <- group.tparamClause.values match
        case first :: second :: Nil => Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the type-parameter sentinel must preserve two ordered parameters."
          )
      (firstTypeParameter, secondTypeParameter) = typeParameters
      _ <- require(
        firstTypeParameter.mods.isEmpty &&
          secondTypeParameter.mods.isEmpty &&
          firstTypeParameter.name.value == firstTypeParameterSentinel &&
          firstTypeParameter.name.syntax == firstTypeParameterSentinel &&
          secondTypeParameter.name.value == secondTypeParameterSentinel &&
          secondTypeParameter.name.syntax == secondTypeParameterSentinel &&
          secondTypeParameter.tbounds.hi.exists(
            _.syntax == s"List[$firstTypeParameterSentinel]"
          ),
        source.length,
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
        "the ordered type-parameter sentinels and dependent bound must remain structural."
      )
      clauses <- group.paramClauses match
        case first :: second :: Nil if first.mod.isEmpty && second.mod.isEmpty =>
          Right((first, second))
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_CLAUSE_TOPOLOGY_UNSUPPORTED",
            "the rank-3 sentinel must preserve two ordinary parameter clauses."
          )
      (firstClause, secondClause) = clauses
      firstParameter <- firstClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the first rank-3 sentinel clause must contain one ordinary parameter."
          )
      secondParameter <- secondClause.values match
        case value :: Nil if value.mods.isEmpty && value.default.isEmpty => Right(value)
        case _ =>
          unsupported(
            source.length,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "the second rank-3 sentinel clause must contain one ordinary parameter."
          )
      _ <- require(
        firstParameter.name.value == firstParameterSentinel &&
          firstParameter.decltpe.exists(_.syntax == firstTypeParameterSentinel) &&
          secondParameter.name.value == secondParameterSentinel &&
          secondParameter.decltpe.exists(_.syntax == secondTypeParameterSentinel) &&
          definition.decltpe.exists {
            case value: Type.Name =>
              value.value == resultSentinel && value.syntax == resultSentinel
            case _ => false
          },
        source.length,
        "DEFINITION_PATTERN_CAPTURE_LAYOUT_UNSUPPORTED",
        "the type-parameter, paramss, and result sentinels must remain in their exact positions."
      )
      _ <- definition.body match
        case value: Term.Name if value.value == bodySentinel => Right(())
        case _ =>
          unsupported(
            source.length,
            "COMPLETE_BODY_CAPTURE_REQUIRED",
            "the body capture must occupy the complete Definition right-hand side."
          )
    yield CapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResultProjection(
      methodSentinel,
      firstTypeParameterSentinel,
      secondTypeParameterSentinel,
      firstParameterSentinel,
      secondParameterSentinel,
      resultSentinel,
      bodySentinel
    )

  private def isExactRankedParameterSequence(parts: Vector[String]): Boolean =
    parts match
      case Vector(prefix, between, suffix) =>
        prefix.matches("(?s)\\s*def\\s+collect\\s*\\(\\s*\\.\\.\\s*") &&
          between.matches("(?s)\\s*\\)\\s*:\\s*Int\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactRankedParameterClauseSequence(parts: Vector[String]): Boolean =
    parts match
      case Vector(prefix, between, suffix) =>
        prefix.matches("(?s)\\s*def\\s+collect\\s*\\(\\s*\\.\\.\\.\\s*") &&
          between.matches("(?s)\\s*\\)\\s*:\\s*Int\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactCapturedNameRankedParameterClauseSequenceCapturedResult(
      parts: Vector[String]
  ): Boolean =
    parts match
      case Vector(prefix, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeParamss.matches("(?s)\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactCapturedNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
      parts: Vector[String]
  ): Boolean =
    parts match
      case Vector(prefix, beforeTparams, beforeParamss, beforeResult, beforeBody, suffix) =>
        prefix.matches("(?s)\\s*def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private def isExactCapturedModifiersNameTypeParameterSequenceRankedParameterClauseSequenceCapturedResult(
      parts: Vector[String]
  ): Boolean =
    parts match
      case Vector(
            beforeModifiers,
            beforeName,
            beforeTparams,
            beforeParamss,
            beforeResult,
            beforeBody,
            suffix
          ) =>
        beforeModifiers.trim.isEmpty &&
          beforeName.matches("(?s)\\s+def\\s+") &&
          beforeTparams.matches("(?s)\\s*\\[\\s*\\.\\.\\s*") &&
          beforeParamss.matches("(?s)\\s*\\]\\s*\\(\\s*\\.\\.\\.\\s*") &&
          beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
          beforeBody.matches("(?s)\\s*=\\s*") &&
          suffix.trim.isEmpty
      case _ => false

  private final case class CommonProjection(
      methodName: DefinitionName,
      parameterName: DefinitionName,
      parameterTypeSource: String,
      resultTypeSource: String,
      body: Term
  )

  private final case class ExactTwoCommonProjection(
      methodName: DefinitionName,
      firstParameterName: DefinitionName,
      firstParameterTypeSource: String,
      secondParameterName: DefinitionName,
      secondParameterTypeSource: String,
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

  private def projectExactTwoCommon(
      definition: Defn.Def,
      sourceLength: Int
  ): Either[Failure, ExactTwoCommonProjection] =
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
      parameters <- clause.values match
        case first :: second :: Nil
            if first.mods.isEmpty && first.default.isEmpty &&
              second.mods.isEmpty && second.default.isEmpty =>
          Right((first, second))
        case _ =>
          unsupported(
            sourceLength,
            "PARAMETER_TOPOLOGY_UNSUPPORTED",
            "expected exactly two ordinary parameters without modifiers or defaults."
          )
      (firstParameter, secondParameter) = parameters
      firstParameterName <- plainName(
        firstParameter.name.value,
        firstParameter.name.syntax,
        sourceLength,
        "PARAMETER_NAME_UNSUPPORTED"
      )
      secondParameterName <- plainName(
        secondParameter.name.value,
        secondParameter.name.syntax,
        sourceLength,
        "PARAMETER_NAME_UNSUPPORTED"
      )
      _ <- require(
        firstParameterName != secondParameterName,
        sourceLength,
        "DUPLICATE_PARAMETER_NAME_UNSUPPORTED",
        "the two ordinary parameter names must be distinct."
      )
      firstParameterType <- firstParameter.decltpe.toRight(
        Failure(
          "PARAMETER_TYPE_REQUIRED",
          0,
          sourceLength,
          "the first parameter must have an explicit declared Type."
        )
      )
      secondParameterType <- secondParameter.decltpe.toRight(
        Failure(
          "PARAMETER_TYPE_REQUIRED",
          0,
          sourceLength,
          "the second parameter must have an explicit declared Type."
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
    yield ExactTwoCommonProjection(
      methodName,
      firstParameterName,
      firstParameterType.syntax,
      secondParameterName,
      secondParameterType.syntax,
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

  private def exactTwoAdmittedType(
      normalForm: TypeNormalForm,
      definitionSourceLength: Int
  ): Either[Failure, Unit] =
    normalForm match
      case TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => Right(())
      case _ =>
        unsupported(
          definitionSourceLength,
          "DEFINITION_TYPE_UNSUPPORTED",
          "exact-two Definition Types must be standalone Int, String, or Boolean."
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
