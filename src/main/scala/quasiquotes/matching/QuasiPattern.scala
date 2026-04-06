package quasiquotes.matching

import scala.quoted.Quotes

import quasiquotes.parser.TinyTermParser

final case class QuasiPattern(
    input: String,
    placeholderSource: String,
    shape: String,
    pattern: TermPattern
):
  def matchTerm(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, MatchResult[q.reflect.Term]] =
    TermMatcher.matchTerm(pattern, term)

object QuasiPattern:
  def term(pattern: String): Either[PatternError, QuasiPattern] =
    for
      source <- PatternSource.synthesize(pattern)
      parsed <- TinyTermParser.parse(source.source).left.map(PatternError.ParseFailure.apply)
      compiled <- PatternCompiler.compile(parsed.rawTree)
    yield QuasiPattern(
      input = pattern,
      placeholderSource = source.source,
      shape = parsed.shape.render,
      pattern = compiled
    )

  def termOrThrow(pattern: String): QuasiPattern =
    term(pattern).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (sc: StringContext)
    def qq: Nothing =
      throw new UnsupportedOperationException(
        PatternError.NoHolesInInterpolator().message
      )
