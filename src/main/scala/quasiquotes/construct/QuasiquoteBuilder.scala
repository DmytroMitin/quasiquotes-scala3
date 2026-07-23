package quasiquotes.construct

import scala.quoted.Quotes

import quasiquotes.parser.{DiagnosticLocationMapper, TinyTermParser}
import quasiquotes.source.{DiagnosticLocation, DiagnosticPrecision}

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
            val location = DiagnosticLocationMapper.fromParseError(parseError, synthesized.originMap)
            Left(QuasiquoteBuildFailure(QuasiquoteError.ParseFailure(parseError), location))
          case Right(parsed) =>
            ParsedTermLowerer
              .lowerLocated(parsed.rawTree, synthesized.bindings, synthesized.literalCategorizedNames)
              .left.map { failure =>
                val location = failure.generatedSpan.flatMap(
                  DiagnosticLocation.fromGeneratedMap(
                    synthesized.originMap,
                    _,
                    DiagnosticPrecision.ExactOccurrence
                  )
                )
                QuasiquoteBuildFailure(failure.error, location)
              }
