package quasiquotes.types

import quasiquotes.parser.*
import quasiquotes.source.*

private[quasiquotes] final case class MappedTypePattern(
    pattern: TypePattern,
    mappedSource: MappedHoleSource,
    parsedType: ParsedType
)

/** Compiler-coupled source adapter for the core-owned `TypePattern`. */
object TypePatternSource:
  def fromSource(source: String): Either[TypeQuasiquoteError, TypePattern] =
    fromSourceLocated(source).left.map(_.diagnostic)

  def fromSourceLocated(
      source: String
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], TypePattern] =
    fromSourceWithMappingLocated(source).map(_.pattern)

  private[quasiquotes] def fromSourceWithMappingLocated(
      source: String
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], MappedTypePattern] =
    val mapped = TypePattern.rewriteSourceMapped(source)
    TinyTypeParser.parse(mapped.generatedSource) match
      case Left(error) =>
        Left(
          LocatedDiagnostic(
            TypeQuasiquoteError(
              HoleSourceRewriter.restoreSemanticHoleIdentifiers(
                error.summary,
                mapped,
                allowUnicodeIdentifiers = false
              )
            ),
            DiagnosticLocationMapper.fromParseError(error, mapped.originMap)
          )
        )
      case Right(parsed) =>
        TypePattern
          .fromShapeWithHoles(parsed.shape, mapped.generatedHoleIndex)
          .left
          .map { error =>
            LocatedDiagnostic(
              TypeQuasiquoteError(
                HoleSourceRewriter.restoreSemanticHoleIdentifiers(
                  error.message,
                  mapped,
                  allowUnicodeIdentifiers = false
                )
              ),
              DiagnosticLocationMapper.wholeSource(
                mapped.originMap,
                DottySourceSpanAdapter.fromTree(parsed.rawTree)
              )
            )
          }
          .map(MappedTypePattern(_, mapped, parsed))
