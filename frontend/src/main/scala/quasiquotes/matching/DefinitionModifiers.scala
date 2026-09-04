package quasiquotes.matching

/** Immutable semantic view of a reflected method Definition's modifiers. */
final class DefinitionModifiers[FlagSet, Within, Annotation](
    val flags: FlagSet,
    val privateWithin: Option[Within],
    val protectedWithin: Option[Within],
    val annotations: List[Annotation]
)
