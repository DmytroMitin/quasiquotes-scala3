package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

object TinyTermParser:
  def parse(source: String): Either[ParseError, ParsedExpression] =
    Scala3ParserBridge.parseExpression(source)

  def parseOrThrow(source: String): ParsedExpression =
    parse(source).fold(throw _, identity)

  def parseRaw(source: String): Either[ParseError, untpd.Tree] =
    parse(source).map(_.rawTree)
