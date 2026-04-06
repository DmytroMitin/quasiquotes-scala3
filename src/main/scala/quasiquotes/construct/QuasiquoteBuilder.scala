package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.parser.TinyTermParser

object QuasiquoteBuilder:
  def build(using q: Quotes)(parts: Seq[String], holes: Seq[q.reflect.Term]): Either[QuasiquoteError, q.reflect.Term] =
    for
      synthesized <- PlaceholderSource.synthesize(parts, holes)
      parsed <- TinyTermParser.parse(synthesized.source).left.map(QuasiquoteError.ParseFailure.apply)
      lowered <- ParsedTermLowerer.lower(parsed.rawTree, synthesized.holes)
    yield lowered
