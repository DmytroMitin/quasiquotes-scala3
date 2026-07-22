package quasiquotes.source

final case class SourceId(value: String) derives CanEqual:
  require(value.nonEmpty, "SourceId must not be empty")

object SourceId:
  val TermConstructionTemplate: SourceId = SourceId("term-construction-template")
  val TermPattern: SourceId = SourceId("term-pattern")
  val TypePattern: SourceId = SourceId("type-pattern")
  val TypeTemplate: SourceId = SourceId("type-template")
  val VirtualExpressionParserInput: SourceId = SourceId("virtual-expression-parser-input")
  val VirtualTermPatternParserInput: SourceId = SourceId("virtual-term-pattern-parser-input")
  val VirtualTypePatternParserInput: SourceId = SourceId("virtual-type-pattern-parser-input")
  val VirtualTypeTemplateParserInput: SourceId = SourceId("virtual-type-template-parser-input")

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

enum HoleRole derives CanEqual:
  case TermPattern
  case TypePattern
  case TypeTemplate

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

final case class DiagnosticLocation(
    generatedSourceId: SourceId,
    generatedSpan: SourceSpan,
    origins: Vector[SourceOrigin]
) derives CanEqual

object DiagnosticLocation:
  def from(sourceMap: GeneratedSourceMap, generatedSpan: SourceSpan): Option[DiagnosticLocation] =
    Option.when(generatedSpan.end <= sourceMap.generatedSource.length) {
      DiagnosticLocation(
        sourceMap.generatedSourceId,
        generatedSpan,
        sourceMap.originsFor(generatedSpan).map(_.origin)
      )
    }

final case class LocatedDiagnostic[+E](diagnostic: E, location: Option[DiagnosticLocation])
