package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile

import quasiquotes.terms.ConstructedTerm

private[quasiquotes] object ConstructedTermGeneratedOriginAdapter:
  import ConstructedTermGeneratedOriginError.*

  def lower(
      constructed: ConstructedTerm,
      virtualSourceName: String
  )(using Context): Either[
    ConstructedTermGeneratedOriginError,
    GeneratedOriginTermResult
  ] =
    for
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
      fragment <- GeneratedOriginFragmentSupport.planTerm(constructed)
      raw <- ConstructedTermUntypedBackend
        .lower(constructed)
        .left
        .map(error => RawLoweringFailure(error.message))
      source = SourceFile.virtual(virtualSourceName, fragment.source)
      positioned <- GeneratedOriginFragmentSupport
        .positionTerm(raw, fragment, source, baseOffset = 0)
      _ <- GeneratedOriginFragmentSupport
        .validatePositionedTree(
          positioned,
          source,
          sourceStart = 0,
          sourceEnd = fragment.source.length
        )
    yield new GeneratedOriginTermResult(positioned, fragment.source, source)

  private[dotty] def validatePositionedForTest(
      tree: untpd.Tree,
      expectedSource: SourceFile,
      sourceLength: Int
  )(using Context): Either[ConstructedTermGeneratedOriginError, Unit] =
    GeneratedOriginFragmentSupport.validatePositionedTree(
      tree,
      expectedSource,
      sourceStart = 0,
      sourceEnd = sourceLength
    )
