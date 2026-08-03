package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

object TinyTypeParser:
  def parse(source: String): Either[ParseError, ParsedType] =
    Scala3ParserBridge.parseType(source)

  def parseOrThrow(source: String): ParsedType =
    parse(source).fold(throw _, identity)

  def parseRaw(source: String): Either[ParseError, untpd.Tree] =
    parse(source).map(_.rawTree)
