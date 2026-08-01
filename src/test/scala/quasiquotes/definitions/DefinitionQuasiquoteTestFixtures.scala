package quasiquotes.definitions

import quasiquotes.parser.TinyTermParser
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

private[definitions] object DefinitionQuasiquoteTestFixtures:
  import DefinitionArguments.*

  def tpe(source: String): TypeNormalForm =
    TypeNormalForm.fromSource(source).fold(error => fail(error.message), identity)

  def term(source: String): ConstructedTerm =
    ConstructedTerm
      .fromShape(TinyTermParser.parseOrThrow(source).shape)
      .fold(error => fail(error.message), identity)

  def definitionType(source: String): DefinitionTypeArgument =
    DefinitionArguments.definitionType(tpe(source))

  def bodyTerm(source: String): BodyTermArgument =
    DefinitionArguments.bodyTerm(term(source))

  def bodyType(source: String): BodyTypeArgument =
    DefinitionArguments.bodyType(tpe(source))

  private def fail(message: String): Nothing =
    throw new AssertionError(message)
