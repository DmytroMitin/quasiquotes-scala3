package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

final case class ParsedType(
    source: String,
    rawTree: untpd.Tree,
    shape: TypeShape,
    rawStructure: String
)
