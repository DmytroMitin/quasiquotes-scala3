package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.parser.TinyTermParser
import quasiquotes.source.DiagnosticLocation

object QuasiquoteBuilder:
  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice]
  ): Either[QuasiquoteError, q.reflect.Term] =
    buildLocated(parts, arguments).left.map(_.error)

  private[construct] def buildLocated(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | QuasiTypeSplice]
  ): Either[QuasiquoteBuildFailure, q.reflect.Term] =
    val holes: Seq[QuasiquoteHole[q.reflect.Term]] = arguments.map {
      case splice: QuasiTypeSplice => QuasiquoteHole.ConstructedTypeSplice(splice.constructedType)
      case term => QuasiquoteHole.Term(term.asInstanceOf[q.reflect.Term])
    }
    PlaceholderSource.synthesizeCategorized(parts, holes) match
      case Left(error) =>
        Left(QuasiquoteBuildFailure(error, None))
      case Right(synthesized) =>
        TinyTermParser.parse(synthesized.source) match
          case Left(parseError) =>
            val generatedSpan = parseError.diagnostics.iterator
              .flatMap(_.generatedSpan)
              .find(!_.isEmpty)
            val location = generatedSpan.flatMap(DiagnosticLocation.from(synthesized.originMap, _))
            Left(QuasiquoteBuildFailure(QuasiquoteError.ParseFailure(parseError), location))
          case Right(parsed) =>
            ParsedTermLowerer
              .lowerLocated(parsed.rawTree, synthesized.bindings, synthesized.literalCategorizedNames)
              .left.map { failure =>
                val location = failure.generatedSpan.flatMap(DiagnosticLocation.from(synthesized.originMap, _))
                QuasiquoteBuildFailure(failure.error, location)
              }
