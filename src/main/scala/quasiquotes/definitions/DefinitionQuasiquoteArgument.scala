package quasiquotes.definitions

import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

private[quasiquotes] sealed trait DefinitionQuasiquoteArgument

private[quasiquotes] final class DefinitionTypeArgument private[quasiquotes] (
    val value: TypeNormalForm
) extends DefinitionQuasiquoteArgument

private[quasiquotes] final class BodyTermArgument private[quasiquotes] (
    val value: ConstructedTerm
) extends DefinitionQuasiquoteArgument

private[quasiquotes] final class BodyTypeArgument private[quasiquotes] (
    val value: TypeNormalForm
) extends DefinitionQuasiquoteArgument

private[quasiquotes] object DefinitionArguments:
  def definitionType(value: TypeNormalForm): DefinitionTypeArgument =
    new DefinitionTypeArgument(value)

  def bodyTerm(value: ConstructedTerm): BodyTermArgument =
    new BodyTermArgument(value)

  def bodyType(value: TypeNormalForm): BodyTypeArgument =
    new BodyTypeArgument(value)
