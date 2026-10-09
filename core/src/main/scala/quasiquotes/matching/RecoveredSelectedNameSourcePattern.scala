package quasiquotes.matching

import scala.util.control.NonFatal

import quasiquotes.parser.TermShape
import quasiquotes.source.{GeneratedHoleIndex, GeneratedSourceMap, HoleOccurrence, HoleRole, SourceOrigin}

private[quasiquotes] sealed trait SelectedNameSourceRecoveryError derives CanEqual:
  def message: String
  def sourceOccurrences: Vector[HoleOccurrence]

private[quasiquotes] object SelectedNameSourceRecoveryError:
  final case class InvalidSelectedNameSourceOccurrence(
      detail: String,
      sourceOccurrences: Vector[HoleOccurrence] = Vector.empty
  ) extends SelectedNameSourceRecoveryError:
    def message: String = s"Invalid selected-name source occurrence: $detail."

  final case class MissingSelectedNameSourceRole(
      sourceOccurrences: Vector[HoleOccurrence]
  ) extends SelectedNameSourceRecoveryError:
    def message: String =
      "No mapped source occurrence occupies a compiled selected-name field."

  final case class MultipleSelectedNameSourceRoles(
      selectedOccurrences: Vector[HoleOccurrence]
  ) extends SelectedNameSourceRecoveryError:
    def sourceOccurrences: Vector[HoleOccurrence] = selectedOccurrences
    def message: String =
      "The first selected-name source-role tranche requires exactly one mapped selected-name occurrence."

  final case class SourcePatternSidecarMismatch(
      detail: String,
      sourceOccurrences: Vector[HoleOccurrence]
  ) extends SelectedNameSourceRecoveryError:
    def message: String = s"Mapped source and compiled pattern sidecars disagree: $detail."

  final case class SelectedNameSourceCategoryConflict(
      name: String,
      selectedOccurrence: HoleOccurrence,
      ordinaryOccurrences: Vector[HoleOccurrence]
  ) extends SelectedNameSourceRecoveryError:
    def sourceOccurrences: Vector[HoleOccurrence] = selectedOccurrence +: ordinaryOccurrences
    def message: String =
      s"Source name $name is used by both the selected-name role and an ordinary Term-hole role."

  final case class UnderlyingSelectedNamePatternError(
      error: NeutralSelectedNameMatchError,
      selectedOccurrence: HoleOccurrence
  ) extends SelectedNameSourceRecoveryError:
    def sourceOccurrences: Vector[HoleOccurrence] = Vector(selectedOccurrence)
    def message: String = error.message

/** Source-associated provenance around an N061 semantic matcher.
  *
  * Instances intentionally retain identity equality: exact spans and source maps are provenance,
  * while semantic matcher equality remains available through `validatedPattern`.
  */
private[quasiquotes] final class RecoveredSelectedNameSourcePattern private (
    val validatedPattern: ValidatedSelectedNamePattern,
    val selectedOccurrence: HoleOccurrence,
    val ordinaryTermHoleNames: Vector[String],
    val sourceMap: GeneratedSourceMap
):
  def matchTarget(
      target: TermShape
  ): Either[NeutralSelectedNameMatchError, NeutralSelectedNameMatchResult] =
    SelectedNamePatternMatcher.matchTarget(validatedPattern, target)

private[quasiquotes] object RecoveredSelectedNameSourcePattern:
  import SelectedNameSourceRecoveryError.*

  private final case class SelectedField(ordinal: Int, name: String)

  private final case class PatternScan(
      selectedFields: Vector[SelectedField],
      ordinaryTermHoleNames: Vector[String]
  )

  def recover(
      mapped: MappedPatternSource,
      compiled: TermPattern
  ): Either[SelectedNameSourceRecoveryError, RecoveredSelectedNameSourcePattern] =
    for
      presentMapped <- Option(mapped).toRight(
        InvalidSelectedNameSourceOccurrence("mapped source must be present")
      )
      presentCompiled <- Option(compiled).toRight(
        InvalidSelectedNameSourceOccurrence("compiled pattern must be present")
      )
      occurrences <- validateMappedSource(presentMapped)
      scan <- scanPattern(presentCompiled)
      candidateData <- selectCandidate(occurrences, scan)
      (selectedIndex, selectedOccurrence, selectedOrdinal) = candidateData
      remainingOccurrences = occurrences.patch(selectedIndex, Nil, 1)
      ordinaryWithSelectedName =
        remainingOccurrences.filter(_.name == selectedOccurrence.name)
      _ <- Either.cond(
        !scan.ordinaryTermHoleNames.contains(selectedOccurrence.name) &&
          ordinaryWithSelectedName.isEmpty,
        (),
        SelectedNameSourceCategoryConflict(
          selectedOccurrence.name,
          selectedOccurrence,
          ordinaryWithSelectedName
        )
      )
      validated <- ValidatedSelectedNamePattern
        .create(
          presentCompiled,
          SelectedNamePatternOccurrence(
            name = selectedOccurrence.name,
            selectOrdinal = selectedOrdinal,
            transport = selectedOccurrence.generatedName
          )
        )
        .left
        .map(UnderlyingSelectedNamePatternError(_, selectedOccurrence))
      ordinaryNames = remainingOccurrences.map(_.name)
      _ <- Either.cond(
        ordinaryNames == scan.ordinaryTermHoleNames,
        (),
        SourcePatternSidecarMismatch(
          s"ordinary Term-hole source order ${renderNames(ordinaryNames)} does not equal compiled order ${renderNames(scan.ordinaryTermHoleNames)}",
          occurrences
        )
      )
    yield new RecoveredSelectedNameSourcePattern(
      validated,
      selectedOccurrence,
      ordinaryNames,
      presentMapped.originMap
    )

  private def validateMappedSource(
      mapped: MappedPatternSource
  ): Either[SelectedNameSourceRecoveryError, Vector[HoleOccurrence]] =
    for
      patternSource <- Option(mapped.patternSource).toRight(
        InvalidSelectedNameSourceOccurrence("PatternSource sidecar must be present")
      )
      generatedSource <- Option(patternSource.source).toRight(
        InvalidSelectedNameSourceOccurrence("generated pattern source must be present")
      )
      holes <- Option(patternSource.holes).toRight(
        InvalidSelectedNameSourceOccurrence("PatternSource holes must be present")
      )
      sourceMap <- Option(mapped.originMap).toRight(
        InvalidSelectedNameSourceOccurrence("generated source map must be present")
      )
      occurrences <- Option(mapped.occurrences).toRight(
        InvalidSelectedNameSourceOccurrence("mapped occurrence vector must be present")
      )
      _ <- validateOccurrences(occurrences, generatedSource, sourceMap)
      _ <- Either.cond(
        holes == occurrences.map(_.name),
        (),
        SourcePatternSidecarMismatch(
          "PatternSource hole order does not equal mapped occurrence order",
          occurrences
        )
      )
      _ <- Either.cond(
        Option(sourceMap.generatedSource).contains(generatedSource),
        (),
        SourcePatternSidecarMismatch(
          "GeneratedSourceMap source does not equal PatternSource source",
          occurrences
        )
      )
      _ <- validateGeneratedHoleIndex(occurrences)
    yield occurrences

  private def validateOccurrences(
      occurrences: Vector[HoleOccurrence],
      generatedSource: String,
      sourceMap: GeneratedSourceMap
  ): Either[SelectedNameSourceRecoveryError, Unit] =
    if occurrences.exists(_ == null) then
      Left(
        InvalidSelectedNameSourceOccurrence(
          "mapped occurrence entries must be present",
          occurrences.filter(_ != null)
        )
      )
    else if sourceMap.segments == null || sourceMap.segments.exists(_ == null) then
      Left(
        InvalidSelectedNameSourceOccurrence(
          "generated source-map segments must be present",
          occurrences
        )
      )
    else
      occurrences.collectFirst(Function.unlift { occurrence =>
        invalidOccurrenceDetail(occurrence, generatedSource, sourceMap)
          .map(detail => InvalidSelectedNameSourceOccurrence(detail, Vector(occurrence)))
      }) match
        case Some(error) => Left(error)
        case None        => Right(())

  private def invalidOccurrenceDetail(
      occurrence: HoleOccurrence,
      generatedSource: String,
      sourceMap: GeneratedSourceMap
  ): Option[String] =
    if occurrence.name == null || occurrence.name.isEmpty then
      Some("logical name must be present and non-empty")
    else if occurrence.generatedName == null || occurrence.generatedName.isEmpty then
      Some("generated transport must be present and non-empty")
    else if occurrence.originalSpan == null || occurrence.originalSpan.isEmpty then
      Some("original source span must be present and non-empty")
    else if occurrence.generatedSpan == null || occurrence.generatedSpan.isEmpty then
      Some("generated source span must be present and non-empty")
    else if occurrence.role != HoleRole.TermPattern then
      Some("occurrence role must be HoleRole.TermPattern")
    else if occurrence.generatedSpan.end > generatedSource.length then
      Some("generated source span lies outside PatternSource source")
    else if generatedSource.substring(
        occurrence.generatedSpan.start,
        occurrence.generatedSpan.end
      ) != occurrence.generatedName
    then Some("generated source span does not contain the occurrence transport")
    else if !sourceMap.segments.exists { segment =>
        segment.generatedSpan == occurrence.generatedSpan &&
        (segment.origin match
          case SourceOrigin.RewrittenHole(sourceId, originalSpan, name, role) =>
            sourceId != null &&
              originalSpan == occurrence.originalSpan &&
              name == occurrence.name &&
              role == occurrence.role
          case _ => false)
      }
    then Some("GeneratedSourceMap does not retain the exact rewritten-hole occurrence")
    else None

  private def validateGeneratedHoleIndex(
      occurrences: Vector[HoleOccurrence]
  ): Either[SelectedNameSourceRecoveryError, Unit] =
    try
      val index = GeneratedHoleIndex.fromOccurrences(occurrences)
      occurrences.collectFirst {
        case occurrence
            if index.semanticNameFor(occurrence.generatedName) != Some(occurrence.name) =>
          occurrence
      } match
        case Some(occurrence) =>
          Left(
            SourcePatternSidecarMismatch(
              s"generated transport ${occurrence.generatedName} does not map to ${occurrence.name}",
              occurrences
            )
          )
        case None => Right(())
    catch
      case NonFatal(error) =>
        Left(
          SourcePatternSidecarMismatch(
            Option(error.getMessage).getOrElse("invalid GeneratedHoleIndex mapping"),
            occurrences
          )
        )

  private def selectCandidate(
      occurrences: Vector[HoleOccurrence],
      scan: PatternScan
  ): Either[SelectedNameSourceRecoveryError, (Int, HoleOccurrence, Int)] =
    val fieldsByTransport = scan.selectedFields.groupBy(_.name)
    val candidates = occurrences.zipWithIndex.flatMap { case (occurrence, index) =>
      fieldsByTransport.get(occurrence.generatedName).map(fields => (index, occurrence, fields))
    }

    candidates.collectFirst { case (_, occurrence, fields) if fields.size > 1 =>
      (occurrence, fields)
    } match
      case Some((occurrence, fields)) =>
        Left(
          SourcePatternSidecarMismatch(
            s"generated transport ${occurrence.generatedName} occupies Select ordinals ${fields.map(_.ordinal).mkString(", ")}",
            Vector(occurrence)
          )
        )
      case None =>
        candidates match
          case Vector() => Left(MissingSelectedNameSourceRole(occurrences))
          case Vector((index, occurrence, fields)) =>
            Right((index, occurrence, fields.head.ordinal))
          case values =>
            Left(MultipleSelectedNameSourceRoles(values.map(_._2)))

  private def scanPattern(
      pattern: TermPattern
  ): Either[SelectedNameSourceRecoveryError, PatternScan] =
    var nextSelectOrdinal = 0

    def loop(current: TermPattern): Either[SelectedNameSourceRecoveryError, PatternScan] =
      Option(current)
        .toRight(
          InvalidSelectedNameSourceOccurrence("compiled pattern nodes must be present")
        )
        .flatMap {
          case TermPattern.Hole(name) =>
            Option(name)
              .filter(_.nonEmpty)
              .toRight(
                InvalidSelectedNameSourceOccurrence(
                  "compiled ordinary Term-hole names must be present and non-empty"
                )
              )
              .map(value => PatternScan(Vector.empty, Vector(value)))
          case TermPattern.Identifier(_) | TermPattern.BoundReference(_, _) |
              TermPattern.Literal(_) =>
            Right(PatternScan(Vector.empty, Vector.empty))
          case TermPattern.Lambda1(_, _, _, body) => loop(body)
          case TermPattern.Select(qualifier, name) =>
            val ordinal = nextSelectOrdinal
            nextSelectOrdinal += 1
            for
              selectedName <- Option(name)
                .filter(_.nonEmpty)
                .toRight(
                  InvalidSelectedNameSourceOccurrence(
                    "compiled selected names must be present and non-empty"
                  )
                )
              qualifierScan <- loop(qualifier)
            yield PatternScan(
              SelectedField(ordinal, selectedName) +: qualifierScan.selectedFields,
              qualifierScan.ordinaryTermHoleNames
            )
          case TermPattern.Apply(function, arguments) =>
            combineNodeAndList(function, arguments)
          case TermPattern.New(_, arguments) => combineList(arguments)
          case TermPattern.Infix(left, _, right) => combineNodes(Vector(left, right))
          case TermPattern.Unary(_, operand) => loop(operand)
          case TermPattern.InterpolatedString(_, parts, arguments) =>
            if parts == null then
              Left(
                InvalidSelectedNameSourceOccurrence(
                  "compiled interpolation parts must be present"
                )
              )
            else combineList(arguments)
          case TermPattern.Typed(expression, _) => loop(expression)
          case TermPattern.Tuple(elements) => combineList(elements)
          case TermPattern.If(condition, thenBranch, elseBranch) =>
            combineNodes(Vector(condition, thenBranch, elseBranch))
          case TermPattern.Block(statements, result) =>
            Option(statements)
              .toRight(
                InvalidSelectedNameSourceOccurrence(
                  "compiled block statements must be present"
                )
              )
              .flatMap { values =>
                values.foldLeft[Either[SelectedNameSourceRecoveryError, PatternScan]](
                  Right(PatternScan(Vector.empty, Vector.empty))
                ) { (acc, statement) =>
                  for
                    currentScan <- acc
                    statementScan <- scanStatement(statement)
                  yield combine(currentScan, statementScan)
                }
              }
              .flatMap(scan => loop(result).map(combine(scan, _)))
          case TermPattern.Parenthesized(expression) => loop(expression)
        }

    def scanStatement(
        statement: BlockPatternStatement
    ): Either[SelectedNameSourceRecoveryError, PatternScan] =
      Option(statement)
        .toRight(
          InvalidSelectedNameSourceOccurrence("compiled block statements must be present")
        )
        .flatMap {
          case BlockPatternStatement.LocalVal(_, _, _, initializer) => loop(initializer)
          case term: TermPattern                                    => loop(term)
        }

    def combineNodeAndList(
        node: TermPattern,
        values: List[TermPattern]
    ): Either[SelectedNameSourceRecoveryError, PatternScan] =
      for
        nodeScan <- loop(node)
        listScan <- combineList(values)
      yield combine(nodeScan, listScan)

    def combineList(
        values: List[TermPattern]
    ): Either[SelectedNameSourceRecoveryError, PatternScan] =
      Option(values)
        .toRight(
          InvalidSelectedNameSourceOccurrence("compiled pattern child lists must be present")
        )
        .flatMap(value => combineNodes(value.toVector))

    def combineNodes(
        values: Vector[TermPattern]
    ): Either[SelectedNameSourceRecoveryError, PatternScan] =
      values.foldLeft[Either[SelectedNameSourceRecoveryError, PatternScan]](
        Right(PatternScan(Vector.empty, Vector.empty))
      ) { (acc, value) =>
        for
          current <- acc
          next <- loop(value)
        yield combine(current, next)
      }

    loop(pattern)

  private def combine(left: PatternScan, right: PatternScan): PatternScan =
    PatternScan(
      left.selectedFields ++ right.selectedFields,
      left.ordinaryTermHoleNames ++ right.ordinaryTermHoleNames
    )

  private def renderNames(names: Vector[String]): String =
    names.map(name => s"$$$name").mkString("[", ", ", "]")
