package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.source.SourceSpan

object DottySourceSpanAdapter:
  def fromSpan(span: Span): Option[SourceSpan] =
    Option.when(span.exists && span.start >= 0 && span.end >= span.start) {
      SourceSpan(span.start, span.end)
    }

  def fromTree(tree: untpd.Tree): Option[SourceSpan] =
    fromSpan(tree.span)
