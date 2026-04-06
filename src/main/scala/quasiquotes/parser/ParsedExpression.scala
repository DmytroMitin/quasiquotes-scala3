package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

final case class ParsedExpression(
    source: String,
    rawTree: untpd.Tree,
    shape: TermShape,
    rawStructure: String
)
