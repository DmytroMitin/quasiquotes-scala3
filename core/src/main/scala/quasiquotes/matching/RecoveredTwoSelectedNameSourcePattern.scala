package quasiquotes.matching

import quasiquotes.parser.TermShape
import quasiquotes.source.{GeneratedHoleIndex, GeneratedSourceMap, HoleOccurrence}

private[quasiquotes] sealed trait TwoSelectedNameSourceRecoveryError
    derives CanEqual:
  def message: String
  def sourceOccurrences: Vector[HoleOccurrence]

private[quasiquotes] object TwoSelectedNameSourceRecoveryError:
  final case class NormalizationFailure(
      error: SelectedNameSourceNormalizationError
  ) extends TwoSelectedNameSourceRecoveryError:
    val sourceOccurrences: Vector[HoleOccurrence] = Vector.empty
    def message: String = error.message

  final case class InvalidCompiledPattern(
      detail: String,
      sourceOccurrences: Vector[HoleOccurrence] = Vector.empty
  ) extends TwoSelectedNameSourceRecoveryError:
    def message: String = s"Invalid compiled selected-name pattern: $detail."

  final case class SourcePatternMismatch(
      detail: String,
      sourceOccurrences: Vector[HoleOccurrence]
  ) extends TwoSelectedNameSourceRecoveryError:
    def message: String =
      s"Normalized source and compiled pattern disagree: $detail."

  final case class UnderlyingSelectedNamePatternError(
      error: NeutralSelectedNameMatchError,
      sourceOccurrences: Vector[HoleOccurrence]
  ) extends TwoSelectedNameSourceRecoveryError:
    def message: String = error.message

/** Exact source provenance around the N065 two-selected-name matcher.
  *
  * Instances intentionally retain identity equality. `validatedPattern` owns semantic pattern
  * equality; this wrapper owns one concrete original/public/private source provenance identity.
  */
private[quasiquotes] final class RecoveredTwoSelectedNameSourcePattern private (
    val originalSource: String,
    val publicMapped: MappedPatternSource,
    val generatedSource: String,
    val sourceMap: GeneratedSourceMap,
    val occurrences: Vector[HoleOccurrence],
    val ordinaryHoleIndex: GeneratedHoleIndex,
    val selectedSourceOccurrences: Vector[HoleOccurrence],
    val selectedOccurrences: Vector[SelectedNamePatternOccurrence],
    val ordinaryTermHoleNames: Vector[String],
    val validatedPattern: ValidatedSelectedNamePattern
):
  def matchTarget(
      target: TermShape
  ): Either[NeutralSelectedNameMatchError, NeutralSelectedNameMatchResult] =
    SelectedNamePatternMatcher.matchTarget(validatedPattern, target)

private[quasiquotes] object RecoveredTwoSelectedNameSourcePattern:
  import TwoSelectedNameSourceRecoveryError.*

  private final case class SelectedField(ordinal: Int, name: String)

  private final case class PatternScan(
      selectedFields: Vector[SelectedField],
      ordinaryTermHoleNames: Vector[String],
      nonSelectedIdentifiers: Vector[String]
  )

  def recover(
      originalSource: String,
      selectedPositions: Vector[SelectedNameSourcePosition],
      compiled: TermPattern
  ): Either[
    TwoSelectedNameSourceRecoveryError,
    RecoveredTwoSelectedNameSourcePattern
  ] =
    for
      presentCompiled <- Option(compiled).toRight(
        InvalidCompiledPattern("compiled pattern must be present")
      )
      normalized <- SelectedNameSourceNormalizer
        .normalize(originalSource, selectedPositions)
        .left
        .map(NormalizationFailure.apply)
      scan <- scanPattern(presentCompiled)
      canonicalSelected = normalized.selectedOccurrences.sortBy(_.selectOrdinal)
      selectedSourceOccurrences <- canonicalSelected.foldLeft[
        Either[TwoSelectedNameSourceRecoveryError, Vector[HoleOccurrence]]
      ](Right(Vector.empty)) { (result, selected) =>
        result.flatMap { values =>
          normalized.occurrences
            .find(_.generatedName == selected.transport)
            .toRight(
              SourcePatternMismatch(
                s"selected transport ${selected.transport} has no normalized source occurrence",
                normalized.occurrences
              )
            )
            .map(values :+ _)
        }
      }
      _ <- validateSelectedFields(
        normalized,
        canonicalSelected,
        selectedSourceOccurrences,
        scan
      )
      ordinaryNames = ordinarySourceNames(normalized, selectedPositions)
      _ <- Either.cond(
        ordinaryNames == scan.ordinaryTermHoleNames,
        (),
        SourcePatternMismatch(
          s"ordinary Term-hole source order ${renderNames(ordinaryNames)} does not equal compiled order ${renderNames(scan.ordinaryTermHoleNames)}",
          normalized.occurrences
        )
      )
      validated <- ValidatedSelectedNamePattern
        .createTwo(
          presentCompiled,
          canonicalSelected.head,
          canonicalSelected(1)
        )
        .left
        .map(
          UnderlyingSelectedNamePatternError(
            _,
            selectedSourceOccurrences
          )
        )
    yield new RecoveredTwoSelectedNameSourcePattern(
      normalized.originalSource,
      normalized.publicMapped,
      normalized.generatedSource,
      normalized.sourceMap,
      normalized.occurrences,
      normalized.ordinaryHoleIndex,
      selectedSourceOccurrences,
      canonicalSelected,
      ordinaryNames,
      validated
    )

  private def validateSelectedFields(
      normalized: NormalizedSelectedNameSource,
      selected: Vector[SelectedNamePatternOccurrence],
      selectedSourceOccurrences: Vector[HoleOccurrence],
      scan: PatternScan
  ): Either[TwoSelectedNameSourceRecoveryError, Unit] =
    val selectedTransports = selected.map(_.transport).toSet
    val allNormalizedTransports = normalized.occurrences.map(_.generatedName).toSet
    val ordinaryTransports = allNormalizedTransports -- selectedTransports
    val fieldsByName = scan.selectedFields.groupBy(_.name)

    selected.collectFirst(Function.unlift { occurrence =>
      val matchingFields = fieldsByName.getOrElse(occurrence.transport, Vector.empty)
      if matchingFields.size != 1 then
        Some(
          SourcePatternMismatch(
            s"selected transport ${occurrence.transport} occupies ${matchingFields.size} Select fields instead of exactly one",
            selectedSourceOccurrences
          )
        )
      else
        scan.selectedFields.lift(occurrence.selectOrdinal) match
          case Some(field) if field.name == occurrence.transport => None
          case Some(field) =>
            Some(
              SourcePatternMismatch(
                s"Select ordinal ${occurrence.selectOrdinal} contains ${field.name} instead of ${occurrence.transport}",
                selectedSourceOccurrences
              )
            )
          case None =>
            Some(
              SourcePatternMismatch(
                s"declared Select ordinal ${occurrence.selectOrdinal} does not exist",
                selectedSourceOccurrences
              )
            )
    }) match
      case Some(error) => Left(error)
      case None =>
        scan.selectedFields.find(field => ordinaryTransports(field.name)) match
          case Some(field) =>
            Left(
              SourcePatternMismatch(
                s"ordinary transport ${field.name} occupies undeclared Select ordinal ${field.ordinal}",
                normalized.occurrences
              )
            )
          case None =>
            scan.nonSelectedIdentifiers.find(selectedTransports) match
              case Some(transport) =>
                Left(
                  SourcePatternMismatch(
                    s"selected transport $transport is reused outside its declared Select field",
                    selectedSourceOccurrences
                  )
                )
              case None => Right(())

  private def ordinarySourceNames(
      normalized: NormalizedSelectedNameSource,
      positions: Vector[SelectedNameSourcePosition]
  ): Vector[String] =
    val selectedIndexes = positions.map(_.sourceOccurrenceIndex).toSet
    normalized.occurrences.zipWithIndex.collect {
      case (occurrence, index) if !selectedIndexes(index) => occurrence.name
    }

  private def scanPattern(
      pattern: TermPattern
  ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
    var nextSelectOrdinal = 0

    def loop(
        current: TermPattern
    ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
      Option(current)
        .toRight(InvalidCompiledPattern("compiled pattern nodes must be present"))
        .flatMap {
          case TermPattern.Hole(name) =>
            presentText(name, "ordinary Term-hole name").map(value =>
              PatternScan(Vector.empty, Vector(value), Vector(value))
            )
          case TermPattern.Identifier(name) =>
            presentText(name, "identifier").map(value =>
              PatternScan(Vector.empty, Vector.empty, Vector(value))
            )
          case TermPattern.BoundReference(_, displayName) =>
            presentText(displayName, "bound-reference display name").map(value =>
              PatternScan(Vector.empty, Vector.empty, Vector(value))
            )
          case TermPattern.Literal(_) => Right(PatternScan.empty)
          case TermPattern.Lambda1(_, displayName, _, body) =>
            for
              name <- presentText(displayName, "lambda parameter display name")
              bodyScan <- loop(body)
            yield bodyScan.copy(
              nonSelectedIdentifiers = name +: bodyScan.nonSelectedIdentifiers
            )
          case TermPattern.Select(qualifier, name) =>
            val ordinal = nextSelectOrdinal
            nextSelectOrdinal += 1
            for
              selectedName <- presentText(name, "selected name")
              qualifierScan <- loop(qualifier)
            yield qualifierScan.copy(
              selectedFields =
                SelectedField(ordinal, selectedName) +: qualifierScan.selectedFields
            )
          case TermPattern.Apply(function, arguments) =>
            combineNodeAndList(function, arguments)
          case TermPattern.New(_, arguments) => combineList(arguments)
          case TermPattern.Infix(left, _, right) => combineNodes(Vector(left, right))
          case TermPattern.Unary(_, operand) => loop(operand)
          case TermPattern.InterpolatedString(_, parts, arguments) =>
            Option(parts)
              .toRight(InvalidCompiledPattern("interpolation parts must be present"))
              .flatMap(_ => combineList(arguments))
          case TermPattern.Typed(expression, _) => loop(expression)
          case TermPattern.Tuple(elements) => combineList(elements)
          case TermPattern.If(condition, thenBranch, elseBranch) =>
            combineNodes(Vector(condition, thenBranch, elseBranch))
          case TermPattern.Block(statements, result) =>
            Option(statements)
              .toRight(InvalidCompiledPattern("block statements must be present"))
              .flatMap { values =>
                values.foldLeft[
                  Either[TwoSelectedNameSourceRecoveryError, PatternScan]
                ](Right(PatternScan.empty)) { (acc, statement) =>
                  for
                    accumulated <- acc
                    current <- scanStatement(statement)
                  yield combine(accumulated, current)
                }
              }
              .flatMap(scan => loop(result).map(combine(scan, _)))
          case TermPattern.Parenthesized(expression) => loop(expression)
        }

    def scanStatement(
        statement: BlockPatternStatement
    ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
      Option(statement)
        .toRight(InvalidCompiledPattern("block statements must be present"))
        .flatMap {
          case BlockPatternStatement.LocalVal(_, displayName, _, initializer) =>
            for
              name <- presentText(displayName, "local value display name")
              initializerScan <- loop(initializer)
            yield initializerScan.copy(
              nonSelectedIdentifiers = name +: initializerScan.nonSelectedIdentifiers
            )
          case BlockPatternStatement.LocalDef(
                _,
                methodDisplayName,
                _,
                parameterDisplayName,
                _,
                _,
                body
              ) =>
            for
              method <- presentText(methodDisplayName, "local method display name")
              parameter <- presentText(
                parameterDisplayName,
                "local method parameter display name"
              )
              bodyScan <- loop(body)
            yield bodyScan.copy(
              nonSelectedIdentifiers =
                Vector(method, parameter) ++ bodyScan.nonSelectedIdentifiers
            )
          case term: TermPattern => loop(term)
        }

    def combineNodeAndList(
        node: TermPattern,
        values: List[TermPattern]
    ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
      for
        nodeScan <- loop(node)
        listScan <- combineList(values)
      yield combine(nodeScan, listScan)

    def combineList(
        values: List[TermPattern]
    ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
      Option(values)
        .toRight(InvalidCompiledPattern("compiled child lists must be present"))
        .flatMap(value => combineNodes(value.toVector))

    def combineNodes(
        values: Vector[TermPattern]
    ): Either[TwoSelectedNameSourceRecoveryError, PatternScan] =
      values.foldLeft[
        Either[TwoSelectedNameSourceRecoveryError, PatternScan]
      ](Right(PatternScan.empty)) { (acc, value) =>
        for
          accumulated <- acc
          current <- loop(value)
        yield combine(accumulated, current)
      }

    loop(pattern)

  private object PatternScan:
    val empty: PatternScan =
      PatternScan(Vector.empty, Vector.empty, Vector.empty)

  private def presentText(
      value: String,
      label: String
  ): Either[TwoSelectedNameSourceRecoveryError, String] =
    Option(value)
      .filter(_.nonEmpty)
      .toRight(InvalidCompiledPattern(s"$label must be present and non-empty"))

  private def combine(left: PatternScan, right: PatternScan): PatternScan =
    PatternScan(
      left.selectedFields ++ right.selectedFields,
      left.ordinaryTermHoleNames ++ right.ordinaryTermHoleNames,
      left.nonSelectedIdentifiers ++ right.nonSelectedIdentifiers
    )

  private def renderNames(names: Vector[String]): String =
    names.map(name => s"$$$name").mkString("[", ", ", "]")
