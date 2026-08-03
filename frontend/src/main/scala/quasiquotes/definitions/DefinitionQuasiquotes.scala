package quasiquotes.definitions

import quasiquotes.definitions.parser.DefinitionInterpolationSourceAssembler
import quasiquotes.source.LocatedDiagnostic

private[quasiquotes] object DefinitionQuasiquotes:
  extension (context: StringContext)
    def dqr(
        arguments: DefinitionQuasiquoteArgument*
    ): Either[
      LocatedDiagnostic[DefinitionQuasiquoteError],
      DefinitionQuasiquoteResult
    ] =
      DefinitionQuasiquoteAssembly
        .create(context.parts, arguments)
        .flatMap(DefinitionInterpolationSourceAssembler.construct)
