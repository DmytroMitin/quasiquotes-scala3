package quasiquotes.matching

import scala.quoted.Quotes

import quasiquotes.parser.{DiagnosticLocationMapper, DottySourceSpanAdapter, TermShape, TinyTermParser}
import quasiquotes.source.{DiagnosticLocation, DiagnosticPrecision, LocatedDiagnostic}

final case class QuasiPattern(
    input: String,
    placeholderSource: String,
    shape: String,
    pattern: TermPattern
):
  def matchTermRaw(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    TermMatcher.matchTermRaw(pattern, term)

  def matchTerm(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    TermMatcher.matchTerm(pattern, term)

object QuasiPattern:
  def term(pattern: String): Either[PatternError, QuasiPattern] =
    termLocated(pattern).left.map(_.diagnostic)

  def termLocated(pattern: String): Either[LocatedDiagnostic[PatternError], QuasiPattern] =
    if pattern.contains("s\"\"\"") then
      Left(
        LocatedDiagnostic(
          PatternError.UnsupportedPatternShape(
            "InterpolatedString",
            "triple-quoted interpolation is outside the bounded s tranche"
          ),
          None
        )
      )
    else PatternSource.synthesizeMappedLocated(pattern).flatMap { mapped =>
      TinyTermParser.parse(mapped.patternSource.source) match
        case Left(error) =>
          Left(
            LocatedDiagnostic(
              PatternError.ParseFailure(error),
              DiagnosticLocationMapper.fromParseError(error, mapped.originMap)
            )
          )
        case Right(parsed @ ParsedUnsupportedLambda(unsupported)) =>
          Left(
            LocatedDiagnostic(
              PatternError.UnsupportedPatternShape(unsupported.nodeKind, unsupported.detail),
              DottySourceSpanAdapter.fromTree(parsed.rawTree).flatMap(
                DiagnosticLocation.fromGeneratedMap(
                  mapped.originMap,
                  _,
                  DiagnosticPrecision.ExactOccurrence
                )
              )
            )
          )
        case Right(parsed) =>
          PatternCompiler.compileLocated(parsed.rawTree, mapped.generatedHoleIndex) match
            case Left(failure) =>
              Left(
                LocatedDiagnostic(
                  failure.error,
                  failure.generatedSpan.flatMap(
                    DiagnosticLocation.fromGeneratedMap(
                      mapped.originMap,
                      _,
                      DiagnosticPrecision.ExactOccurrence
                    )
                  )
                )
              )
            case Right(compiled) =>
              Right(
                QuasiPattern(
                  input = pattern,
                  placeholderSource = mapped.patternSource.source,
                  shape = parsed.shape.render,
                  pattern = compiled
                )
              )
    }

  private object ParsedUnsupportedLambda:
    def unapply(parsed: quasiquotes.parser.ParsedExpression): Option[TermShape.Unsupported] =
      parsed.shape match
        case unsupported @ TermShape.Unsupported("Lambda1", _) => Some(unsupported)
        case _ => None

  def termOrThrow(pattern: String): QuasiPattern =
    term(pattern).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (sc: StringContext)
    def qq(using q: Quotes): TermPatternExtractor[q.reflect.Term] =
      import q.reflect.*

      val holeCount = sc.parts.size - 1
      if holeCount <= 0 then
        report.errorAndAbort(
          "Invalid qq term-pattern template: at least one term capture slot is required."
        )
      val holeNames = Vector.tabulate(holeCount)(index => s"qqCapture$index")

      val source = sc.parts.zipWithIndex.foldLeft(new StringBuilder) {
        case (builder, (part, index)) =>
          builder.append(part)
          holeNames.lift(index).foreach(name => builder.append('$').append(name))
          builder
      }.toString

      termLocated(source) match
        case Left(failure) =>
          report.errorAndAbort(
            s"Invalid qq term-pattern template: ${failure.diagnostic.message}"
          )
        case Right(pattern) =>
          new TermPatternExtractor[q.reflect.Term](term =>
            pattern.matchTerm(term) match
              case Left(_) => None
              case Right(result) =>
                holeNames.foldLeft(Option(Vector.empty[q.reflect.Term])) {
                  case (captures, name) =>
                    captures.flatMap(current => result.bindings.get(name).map(current :+ _))
                }
          )
