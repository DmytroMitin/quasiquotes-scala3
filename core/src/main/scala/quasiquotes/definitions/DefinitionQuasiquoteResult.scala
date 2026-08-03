package quasiquotes.definitions

private[quasiquotes] final class DefinitionQuasiquoteResult private[quasiquotes] (
    val constructed: ConstructedDefinition,
    val sourceEvidence: DefinitionQuasiquoteSourceEvidence
)
