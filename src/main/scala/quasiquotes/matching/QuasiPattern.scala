package quasiquotes.matching

import scala.quoted.Quotes

import quasiquotes.parser.{DiagnosticLocationMapper, TinyTermParser}
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
    PatternSource.synthesizeMappedLocated(pattern).flatMap { mapped =>
      TinyTermParser.parse(mapped.patternSource.source) match
        case Left(error) =>
          Left(
            LocatedDiagnostic(
              PatternError.ParseFailure(error),
              DiagnosticLocationMapper.fromParseError(error, mapped.originMap)
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

  def termOrThrow(pattern: String): QuasiPattern =
    term(pattern).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (sc: StringContext)
    def qq: Nothing =
      throw new UnsupportedOperationException(
        PatternError.NoHolesInInterpolator().message
      )
