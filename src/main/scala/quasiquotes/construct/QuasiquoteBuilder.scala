package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.parser.TinyTermParser

object QuasiquoteBuilder:
  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice]
  ): Either[QuasiquoteError, q.reflect.Term] =
    val holes: Seq[QuasiquoteHole[q.reflect.Term]] = arguments.map {
      case splice: QuasiTypeSplice => QuasiquoteHole.ConstructedTypeSplice(splice.constructedType)
      case term => QuasiquoteHole.Term(term.asInstanceOf[q.reflect.Term])
    }
    for
      synthesized <- PlaceholderSource.synthesizeCategorized(parts, holes)
      parsed <- TinyTermParser.parse(synthesized.source).left.map(QuasiquoteError.ParseFailure.apply)
      lowered <- ParsedTermLowerer.lower(parsed.rawTree, synthesized.bindings)
    yield lowered
