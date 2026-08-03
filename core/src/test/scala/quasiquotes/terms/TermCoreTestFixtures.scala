package quasiquotes.terms

import quasiquotes.parser.TermShape
import quasiquotes.source.*
import quasiquotes.types.TypeTemplate

private[terms] object TermCoreTestFixtures:
  val emptyIndex: GeneratedHoleIndex = GeneratedHoleIndex.empty

  def index(
      entries: (String, String)*
  ): GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      entries.zipWithIndex.map { case ((semanticName, generatedName), index) =>
        HoleOccurrence(
          semanticName,
          generatedName,
          SourceSpan(index, index + 1),
          SourceSpan(index, index + 1),
          HoleRole.TermTemplate
        )
      }
    )

  def template(
      root: TermShape,
      termEntries: Vector[(String, String)] = Vector.empty,
      termOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[TermConstructionError, TermTemplate] =
    TermTemplate.create(
      root,
      index(termEntries*),
      termOccurrences,
      index(typeEntries*),
      ascriptions
    )

  def ident(name: String, placeholder: Boolean = false): TermShape =
    TermShape.Identifier(name, placeholder)
