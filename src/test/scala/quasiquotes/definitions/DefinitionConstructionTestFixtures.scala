package quasiquotes.definitions

import quasiquotes.parser.TermShape
import quasiquotes.source.*
import quasiquotes.terms.*
import quasiquotes.types.TypeTemplate

private[definitions] object DefinitionConstructionTestFixtures:
  val plainName: DefinitionName =
    DefinitionName.plain("value").toOption.get

  val keywordName: DefinitionName =
    DefinitionName.backticked("`type`").toOption.get

  def ident(name: String): TermShape =
    TermShape.Identifier(name, false)

  def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  def termTemplate(
      root: TermShape,
      termEntries: Vector[(String, String)] = Vector.empty,
      termOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): TermTemplate =
    TermTemplate
      .create(
        root,
        index(termEntries, HoleRole.TermTemplate),
        termOccurrences,
        index(typeEntries, HoleRole.TypeTemplate),
        ascriptions
      )
      .fold(error => fail(error.message), identity)

  private def index(
      entries: Vector[(String, String)],
      role: HoleRole
  ): GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      entries.zipWithIndex.map {
        case ((semanticName, generatedName), ordinal) =>
          HoleOccurrence(
            semanticName,
            generatedName,
            SourceSpan(ordinal, ordinal + 1),
            SourceSpan(ordinal, ordinal + 1),
            role
          )
      }
    )

  private def fail(message: String): Nothing =
    throw new AssertionError(message)
