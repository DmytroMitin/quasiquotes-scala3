package quasiquotes.q065r

import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.matching.{PatternCompiler, PatternSource}
import _root_.quasiquotes.parser.TinyTermParser
import _root_.quasiquotes.scalameta.TermFrontend
import Q065RLexicalPublicMacros.*

private object Q065RLexicalPublicFixtures:
  val underscorePattern = underscorePatternMatches {
    def renamed(value: Int): Int = value
    renamed(41)
  }

  val unicodeTarget = ordinaryPatternMatchesUnicodeTarget {
    def renamed(λ: Int): Int = λ
    renamed(43)
  }

final class Q065RBinderLexicalAdmissionTest extends munit.FunSuite:
  private val nonStandardParameter =
    "{ def id(λ: Int): Int = λ; id($argument) }"

  test("non-standard source parameter is rejected by hybrid P3 at the accepted standard lexical boundary"):
    val mapped = PatternSource.synthesizeMapped(nonStandardParameter).fold(
      error => fail(error.message),
      identity
    )

    assert(
      ScalametaTermFrontend
        .parse(mapped.patternSource.source, TermQ3DialectPolicy.selected)
        .isRight
    )
    assert(ScalametaTermFrontend.validateExactCompiler(mapped.patternSource.source).isRight)

    val standard = TinyTermParser
      .parseRaw(mapped.patternSource.source)
      .fold(error => fail(error.toString), PatternCompiler.compile)
    assert(standard.isLeft, standard)

    val hybrid = TermFrontend.compile(nonStandardParameter).swap.toOption.getOrElse(
      fail("hybrid P3 admitted a parameter rejected by the standard lexical policy")
    )
    assertEquals(hybrid.category, "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED")
    assert(hybrid.detail.contains("P3 requires a simple parameter binder"), hybrid.message)

  test("public qq retains admitted underscore patterns and alpha-renamed Unicode targets"):
    assert(Q065RLexicalPublicFixtures.underscorePattern)
    assert(Q065RLexicalPublicFixtures.unicodeTarget)
