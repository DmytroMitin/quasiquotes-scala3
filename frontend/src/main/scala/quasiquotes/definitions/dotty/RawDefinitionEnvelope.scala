package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.{DefinitionComponentSpans, DefinitionName}

private[quasiquotes] enum RawDefinitionVariant derives CanEqual:
  case ParameterlessDef
  case ImmutableVal

private[quasiquotes] final case class RawDefinitionEnvelope(
    variant: RawDefinitionVariant,
    tree: untpd.Tree,
    name: DefinitionName,
    definitionType: untpd.Tree,
    body: untpd.Tree,
    components: DefinitionComponentSpans,
    context: Context
)
