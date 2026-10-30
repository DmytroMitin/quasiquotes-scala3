package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

/** Package-local access to the checked raw graph position validator for U080. */
private[quasiquotes] object ExistingQuotedRootedOriginValidation:
  def validate(
      raw: untpd.Tree,
      positioned: untpd.Tree,
      fragment: GeneratedOriginFragmentSupport.TermFragment
  )(using Context): Either[ConstructedTermGeneratedOriginError,Unit] =
    GeneratedOriginFragmentSupport.validatePositionedTermAgainstCheckedRaw(raw,positioned,fragment)
