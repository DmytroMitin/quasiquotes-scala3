package quasiquotes.types

import quasiquotes.parser.*
import quasiquotes.source.*

private[quasiquotes] final case class MappedTypeTemplate(
    template: TypeTemplate,
    mappedSource: MappedHoleSource
)

/** Compiler-coupled source adapter for the core-owned `TypeTemplate`. */
object TypeTemplateSource:
  def fromSource(source: String): Either[TypeQuasiquoteError, TypeTemplate] =
    fromSourceLocated(source).left.map(_.diagnostic)

  def fromSourceLocated(
      source: String
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], TypeTemplate] =
    fromSourceWithMappingLocated(source).map(_.template)

  private[quasiquotes] def fromSourceWithMappingLocated(
      source: String
  ): Either[LocatedDiagnostic[TypeQuasiquoteError], MappedTypeTemplate] =
    val mapped = TypeTemplate.rewriteSourceMapped(source)
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
        TypeTemplate
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
          .map(MappedTypeTemplate(_, mapped))
