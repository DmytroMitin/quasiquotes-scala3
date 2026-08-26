package quasiquotes.source

final case class SourceId(value: String) derives CanEqual:
  require(value.nonEmpty, "SourceId must not be empty")

object SourceId:
  val TermConstructionTemplate: SourceId = SourceId("term-construction-template")
  val TermPattern: SourceId = SourceId("term-pattern")
  val TypePattern: SourceId = SourceId("type-pattern")
  val TypeTemplate: SourceId = SourceId("type-template")
  val DefinitionConstructionTemplate: SourceId =
    SourceId("definition-construction-template")
  val VirtualExpressionParserInput: SourceId = SourceId("virtual-expression-parser-input")
  val VirtualTermPatternParserInput: SourceId = SourceId("virtual-term-pattern-parser-input")
  val VirtualTypePatternParserInput: SourceId = SourceId("virtual-type-pattern-parser-input")
  val VirtualTypeTemplateParserInput: SourceId = SourceId("virtual-type-template-parser-input")
  val VirtualDefinitionTemplateParserInput: SourceId =
    SourceId("virtual-definition-template-parser-input")

/** Zero-based, half-open UTF-16 code-unit offsets. */
final case class SourceSpan(start: Int, end: Int) derives CanEqual:
  require(start >= 0, s"SourceSpan start must be non-negative: $start")
  require(end >= start, s"SourceSpan end must be at least start: [$start, $end)")

  def length: Int = end - start
  def isEmpty: Boolean = start == end
  def contains(offset: Int): Boolean = !isEmpty && start <= offset && offset < end
  def overlaps(other: SourceSpan): Boolean =
    !isEmpty && !other.isEmpty && start < other.end && other.start < end

  def intersection(other: SourceSpan): Option[SourceSpan] =
    val intersectionStart = math.max(start, other.start)
    val intersectionEnd = math.min(end, other.end)
    Option.when(intersectionStart < intersectionEnd)(SourceSpan(intersectionStart, intersectionEnd))

enum InterpolationCategory derives CanEqual:
  case TermSplice
  case ConstructedTypeSplice
  case SelectedMemberNameSplice
  case DefinitionTypeSplice
  case DefinitionBodyTermSplice
  case DefinitionBodyTypeSplice

enum HoleRole derives CanEqual:
  case TermTemplate
  case TermPattern
  case TypePattern
  case TypeTemplate
  case DefinitionTypeTemplate
  case DefinitionBodyTermTemplate
  case DefinitionBodyTypeTemplate

sealed trait SourceOrigin derives CanEqual

object SourceOrigin:
  final case class OriginalText(sourceId: SourceId, originalSpan: SourceSpan) extends SourceOrigin

  final case class LiteralPart(
      templateSourceId: SourceId,
      partIndex: Int,
      spanWithinPart: SourceSpan
  ) extends SourceOrigin:
    require(partIndex >= 0, s"Literal part index must be non-negative: $partIndex")

  final case class InterpolationArgument(
      templateSourceId: SourceId,
      argumentIndex: Int,
      category: InterpolationCategory
  ) extends SourceOrigin:
    require(argumentIndex >= 0, s"Interpolation argument index must be non-negative: $argumentIndex")

  final case class RewrittenHole(
      sourceId: SourceId,
      originalSpan: SourceSpan,
      holeName: String,
      role: HoleRole
  ) extends SourceOrigin:
    require(holeName.nonEmpty, "Rewritten hole name must not be empty")

final case class GeneratedSegment(generatedSpan: SourceSpan, origin: SourceOrigin) derives CanEqual

final case class MappedOrigin(generatedSpan: SourceSpan, origin: SourceOrigin) derives CanEqual

final case class GeneratedSourceMap(
    generatedSource: String,
    generatedSourceId: SourceId,
    segments: Vector[GeneratedSegment]
) derives CanEqual:
  require(
    segments.forall(segment => segment.generatedSpan.end <= generatedSource.length),
    "Generated source segment is outside the generated source"
  )
  require(
    segments.zip(segments.drop(1)).forall { case (left, right) =>
      left.generatedSpan.start <= right.generatedSpan.start &&
      left.generatedSpan.end <= right.generatedSpan.start
    },
    "Generated source segments must be ordered and non-overlapping"
  )

  def originAt(offset: Int): Option[SourceOrigin] =
    if offset < 0 || offset >= generatedSource.length then None
    else segments.find(_.generatedSpan.contains(offset)).map(_.origin)

  def originsFor(span: SourceSpan): Vector[MappedOrigin] =
    if span.isEmpty || span.end > generatedSource.length then Vector.empty
    else
      segments.flatMap { segment =>
        segment.generatedSpan.intersection(span).map(intersection => MappedOrigin(intersection, segment.origin))
      }

  def generatedSpansFor(predicate: SourceOrigin => Boolean): Vector[SourceSpan] =
    segments.collect { case segment if predicate(segment.origin) => segment.generatedSpan }

enum DiagnosticPrecision derives CanEqual:
  case ExactOccurrence
  case WholeSource

final case class DiagnosticLocation(
    sourceId: SourceId,
    span: SourceSpan,
    origins: Vector[SourceOrigin],
    precision: DiagnosticPrecision
) derives CanEqual:
  require(!span.isEmpty, "DiagnosticLocation span must not be empty")
  require(origins.nonEmpty, "DiagnosticLocation origins must not be empty")

object DiagnosticLocation:
  def direct(
      sourceId: SourceId,
      span: SourceSpan,
      precision: DiagnosticPrecision
  ): Option[DiagnosticLocation] =
    Option.when(!span.isEmpty) {
      DiagnosticLocation(
        sourceId,
        span,
        Vector(SourceOrigin.OriginalText(sourceId, span)),
        precision
      )
    }

  def fromGeneratedMap(
      sourceMap: GeneratedSourceMap,
      span: SourceSpan,
      precision: DiagnosticPrecision
  ): Option[DiagnosticLocation] =
    Option
      .when(!span.isEmpty && span.end <= sourceMap.generatedSource.length) {
        sourceMap.originsFor(span).map(_.origin)
      }
      .filter(_.nonEmpty)
      .map(origins => DiagnosticLocation(sourceMap.generatedSourceId, span, origins, precision))

final case class LocatedDiagnostic[+E](diagnostic: E, location: Option[DiagnosticLocation])
