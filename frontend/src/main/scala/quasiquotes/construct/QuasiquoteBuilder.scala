package quasiquotes.construct

import scala.quoted.Quotes
import dotty.tools.dotc.core.Types

import quasiquotes.parser.{DiagnosticLocationMapper, DottySourceSpanAdapter, ParsedExpression, TermShape, TinyTermParser}
import quasiquotes.source.{DiagnosticLocation, DiagnosticPrecision}

object QuasiquoteBuilder:
  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName]
  ): Either[QuasiquoteError, q.reflect.Term] =
    buildLocated(parts, arguments).left.map(_.error)

  private[construct] def buildLocated(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName]
  ): Either[QuasiquoteBuildFailure, q.reflect.Term] =
    val holes: Seq[QuasiquoteHole[q.reflect.Term, q.reflect.TypeRepr]] = arguments.map {
      case splice: QuasiTypeSplice => QuasiquoteHole.ConstructedTypeSplice(splice.constructedType)
      case name: SelectedMemberName => QuasiquoteHole.SelectedMemberNameSplice(name)
      case reflectedType: Types.Type =>
        QuasiquoteHole.ReflectedTypeSplice(
          reflectedType.asInstanceOf[q.reflect.TypeRepr]
        )
      case term => QuasiquoteHole.Term(term.asInstanceOf[q.reflect.Term])
    }
    PlaceholderSource.synthesizeCategorized(parts, holes) match
      case Left(error) =>
        Left(QuasiquoteBuildFailure(error, None))
      case Right(synthesized) =>
        if synthesized.source.contains("s\"\"\"") then
          Left(
            QuasiquoteBuildFailure(
              QuasiquoteError.UnsupportedTree(
                "InterpolatedString",
                "Triple-quoted interpolation is outside the bounded s tranche"
              ),
              None
            )
          )
        else TinyTermParser.parse(synthesized.source) match
          case Left(parseError) =>
            val location = DiagnosticLocationMapper.fromParseError(parseError, synthesized.originMap)
            Left(QuasiquoteBuildFailure(QuasiquoteError.ParseFailure(parseError), location))
          case Right(parsed @ ParsedUnsupportedLambda(unsupported)) =>
            Left(
              QuasiquoteBuildFailure(
                QuasiquoteError.UnsupportedTree(unsupported.nodeKind, unsupported.detail),
                DottySourceSpanAdapter.fromTree(parsed.rawTree).flatMap(
                  DiagnosticLocation.fromGeneratedMap(
                    synthesized.originMap,
                    _,
                    DiagnosticPrecision.ExactOccurrence
                  )
                )
              )
            )
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

  private object ParsedUnsupportedLambda:
    def unapply(parsed: ParsedExpression): Option[TermShape.Unsupported] =
      parsed.shape match
        case unsupported @ TermShape.Unsupported("Lambda1", _) => Some(unsupported)
        case _ => None
